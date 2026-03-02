###final version (same logic, just using renderUI strategy for bookmark-safe restore)
library(shiny)
library(move2)
library(sf)
library(pals)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(shinycssloaders)
library(shinybusy)


# helper1: move track attributes to event
as_event <- function(mv, attr_names) {
  if (is.null(attr_names) || !length(attr_names)) return(mv)
  nms <- unique(as.character(attr_names))
  trkattrb <- names(mt_track_data(mv))
  out <- mv
  for (nm in nms) if (!is.null(nm) && nm %in% trkattrb) out <- mt_as_event_attribute(out, nm)
  out
}

# helper2: popup on points
make_popup <- function(d, track_col, attrs = character(0)) {
  coords <- sf::st_coordinates(d)
  dd <- sf::st_drop_geometry(d)
  
  keep <- intersect(attrs, names(dd))
  keep <- unique(keep)
  
  extra <- if (length(keep)) {
    vapply(seq_len(nrow(dd)), function(i) {
      vals_i <- sapply(keep, function(nm) dd[[nm]][i])
      paste(paste0("<b>", keep, ":</b> ", as.character(vals_i)), collapse = "<br>")
    }, character(1))
  } else {
    rep("", nrow(d))
  }
  
  paste0(
    "<b>Track:</b> ", as.character(dd[[track_col]]), "<br>",
    "<b>Time:</b> ", as.character(mt_time(d)), "<br>",
    "<b>Lon:</b> ", round(coords[, 1], 6), "<br>",
    "<b>Lat:</b> ", round(coords[, 2], 6),
    ifelse(nzchar(extra), paste0("<br>", extra), "")
  )
}

########### Interface ##########################

shinyModuleUserInterface <- function(id, label) {
  
  ns <- NS(id)
  
  tagList(
    titlePanel("Basic interactive Map using leaflet"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   
                   h4("Tracks"),
                   uiOutput(ns("animals_ui")),  
                   fluidRow(
                     column(6, actionButton(ns("select_all_animals"), "Select All Animals", class = "btn-sm")),
                     column(6, actionButton(ns("unselect_animals"), "Unselect All Animals", class = "btn-sm"))
                   ),
                   
                   br(),
                   h4("Attributes"),
                   uiOutput(ns("select_attr_ui")), 
                   
                   hr(),
                   actionButton(ns("apply_btn"), "Apply Changes", class = "btn-primary btn-block"),
                   uiOutput(ns("apply_warning")),
                   hr(),
                   
                   br(),
                   h4("Download"),
                   downloadButton(ns("save_html"), "Download as HTML", class = "btn-sm"),
                   downloadButton(ns("save_png"),  "Save Map as PNG", class = "btn-sm")
      ),
      
      mainPanel(withSpinner(leafletOutput(ns("leafmap"), height = "85vh")), width = 9)
    )
  )
}

######### Server ##########################

shinyModule <- function(input, output, session, data) {
  
  ns <- session$ns
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  locked_settings <- reactiveVal(NULL)
  locked_data <- reactiveVal(NULL)
  
  apply_warning <- reactiveVal(FALSE)
  output$apply_warning <- renderUI({
    if (isTRUE(apply_warning())) {
      logger.info("No track selected.")
      div(style = "color:#b30000; font-weight:800; margin-top:6px;", "No track selected")
    } else {
      NULL
    }
  })
  
  if (is.null(data) || nrow(data) == 0) {
    message("Input is NULL or has 0 rows — returning NULL.")
    return(NULL)
  }
  
  if (!sf::st_is_longlat(data)) data <- sf::st_transform(data, 4326)
  
  track_col <- mt_track_id_column(data)
 
  
  ###### add this part : start ###################
  ############ create animal and attribute input- bookmark safe ############# 
  
  # Checks the URL for `_state_id_=` to detect a bookmark restore:TRUE=restore, FALSE=normal run
  restoring <- reactive({
    qs <- session$clientData$url_search %||% ""
    grepl("_state_id_=", qs, fixed = TRUE)
  })
  
  all_ids <- reactive({
    sort(unique(as.character(data[[track_col]])))
  })
  
  #create animals input
  output$animals_ui <- renderUI({
    ids <- all_ids()
    req(length(ids) > 0)
    args <- list( inputId = ns("animals"),label   = NULL, choices = ids )
    if (!isTRUE(restoring())) args$selected <- ids     #fresh run: default select all
    do.call(checkboxGroupInput, args)       #restore from input
  })
  
  #create attribute input 
  attr_choices_all <- reactive({
    td <- mt_track_data(data)
    track_choices <- character(0)
    if (!is.null(td) && ncol(td) > 0) track_choices <- names(td)
    
    d2 <- as_event(data, track_choices)
    event_attr <- sf::st_drop_geometry(d2)
    
    choices <- names(event_attr)[vapply(event_attr, function(x) any(!is.na(x)), logical(1))]
    sort(unique(choices))
  })
  
  output$select_attr_ui <- renderUI({
    selectInput(ns("select_attr"),"Optionally: Select other attributes to show in the pop-up at each point (multiple can be selected):",choices = attr_choices_all(), multiple = TRUE)
  })
 ############## End  ################################
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids())
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Filtered data (selected animals)
  selected_data <- reactive({
    req(input$animals)
    d <- data
    d[d[[track_col]] %in% input$animals, ]
  })
  
  # initialize
  observe({
    if (!is.null(locked_data())) return()
    d0 <- selected_data()
    req(nrow(d0) > 0)
    locked_data(d0)
    locked_settings(list(animals = input$animals, select_attr = input$select_attr))
  })
  
  ###############
  
  # attribute choices (update immediately when tracks change)
  observeEvent(input$animals, {
    
    d <- selected_data()
    req(nrow(d) > 0)
    
    # Track attributes
    td <- mt_track_data(d)
    track_choices <- character(0)
    if (!is.null(td) && ncol(td) > 0) {
      track_choices <- names(td)
    }
    
    # Convert all track attrs to event
    d2 <- as_event(d, track_choices)
    
    event_attr <- sf::st_drop_geometry(d2)
    
    # Keep only attrs that have at least one non-NA
    choices <- names(event_attr)[vapply(event_attr, function(x) any(!is.na(x)), logical(1))]
    choices <- sort(unique(choices))
    
    # keep previous selection if still valid
    prev <- isolate(input$select_attr)
    sel  <- if (!is.null(prev)) intersect(prev, choices) else NULL
    
    updateSelectInput(session, "select_attr", choices = choices, selected = sel)
    
  }, ignoreInit = FALSE)
  
  ##############################################
  
  # Apply button
  observeEvent(input$apply_btn, {
    # if no track selected, not change map
    if (is.null(input$animals) || length(input$animals) == 0) {
      apply_warning(TRUE)
      return()
    }
    
    apply_warning(FALSE)
    locked_data(selected_data())
    locked_settings(list(animals = input$animals, select_attr = input$select_attr))
  }, ignoreInit = TRUE)
  
  # Lines
  track_lines <- reactive({
    d <- locked_data()
    req(d)
    req(nrow(d) >= 2)
    mt_track_lines(d)
  })
  
  mmap <- reactive({
    d <- locked_data()
    req(d)
    req(nrow(d) >= 1)
    
    bounds <- as.vector(sf::st_bbox(d))
    ids <- sort(unique(as.character(d[[track_col]])))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    # colors for points
    d$col <- pal(as.character(d[[track_col]]))
    
    # Build lines
    tl <- track_lines()
    tl$col <- pal(as.character(tl[[track_col]]))
    
    s <- locked_settings()
    attr_sel <- if (!is.null(s) && !is.null(s$select_attr)) s$select_attr else character(0)
    
    if (length(attr_sel)) d <- as_event(d, attr_sel)  # track to event
    d$popup_html <- make_popup(d, track_col, attr_sel)
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addScaleBar(position = "topleft") %>%
      addCircleMarkers(data = d, radius = 1, opacity = 0.7, fillOpacity = 0.5,
                       color = ~col, popup = ~popup_html, group = "Points") %>%
      addPolylines(data = tl, weight = 3, opacity = 0.5,
                   color = ~col, group = "Lines") %>%
      addLegend(position = "bottomright", pal = pal, values = ids, title = "Tracks", opacity = 0.8,group = "Legend") %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
                       overlayGroups = c("Lines", "Points", "Legend"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  ########################################
  # Auto-save map for all individuals
  
  saved_html <- reactiveVal(FALSE)
  
  observe({
    if (saved_html()) return()
    
    d <- selected_data()
    req(nrow(d) > 0)
    
    htmlwidgets::saveWidget(widget = isolate(mmap()),
                            file = "./data/output/autosave_leaflet_mapper.html",
                            selfcontained = FALSE)
    saved_html(TRUE)
  })
  
  ######################################
  
  output$leafmap <- renderLeaflet(mmap())
  
  #### download HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("LeafletMap_", Sys.Date(), ".html"),
    content = function(file) {
      
      show_modal_spinner(spin = "fading-circle", text = "Saving HTML…")
      on.exit(remove_modal_spinner(), add = TRUE)
      
      saveWidget(widget = mmap(), file = file)
    }
  )
  
  #### save map as PNG
  output$save_png <- downloadHandler(
    filename = paste0("LeafletMap_", Sys.Date(), ".png"),
    content = function(file) {
      
      show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(remove_modal_spinner(), add = TRUE)
      
      html_file <- "leaflet_export.html"
      saveWidget(mmap(), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot(url = html_file, file = file, vwidth = 1000, vheight = 800)
    }
  )
  
  return(reactive({ current() }))
}