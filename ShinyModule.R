library(shiny)
library(move2)
library(sf)
library(pals)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(shinycssloaders)
library(shinybusy)
library(zip)

`%||%` <- function(x, y) if (is.null(x)) y else x

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
  
  locked_settings <- reactiveVal(NULL)
  locked_data     <- reactiveVal(NULL)
  
  current <- reactiveVal(NULL)
  
  ###########
  save_artifact_map <- function() {
    targetDirFiles <- file.path(tempdir(), "leaflet_mapper_zip")
    unlink(targetDirFiles, recursive = TRUE, force = TRUE)
    dir.create(targetDirFiles, recursive = TRUE, showWarnings = FALSE)
    
    html_file <- file.path(targetDirFiles, "autosave_leaflet_mapper.html")
    
    logger.info("Saving leaflet html bundle -> %s", targetDirFiles)
    
    htmlwidgets::saveWidget(
      widget = isolate(mmap()),
      file = html_file,
      selfcontained = FALSE
    )
    
    zip_file <- appArtifactPath("autosave_leaflet_mapper.zip")
    
    zip::zip(
      zipfile = zip_file,
      files = list.files(targetDirFiles, full.names = TRUE),
      mode = "cherry-pick"
    )
    
    logger.info("Artifact zip success -> %s", zip_file)
  }
  #############
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
  
  all_ids_vec <- sort(unique(as.character(data[[track_col]])))
  if (!length(all_ids_vec)) logger.info(" no track ids found!")
  
  selected_data <- reactive({
    sel <- input$animals
    if (is.null(sel)) sel <- all_ids_vec
    if (!length(sel)) return(data[0, ])
    d <- data[data[[track_col]] %in% sel, ]
    d
  })
  
  # #create animals input
  output$animals_ui <- renderUI({
    restored_sel <- isolate(input$animals)
    sel <- if (!is.null(restored_sel)) restored_sel else all_ids_vec
    logger.info("animals_ui | choices=%d | selected=%d", length(all_ids_vec), length(sel))
    checkboxGroupInput(ns("animals"), label = NULL, choices = all_ids_vec, selected = sel)
  })
  
  attr_choices_all <- reactive({
    td <- mt_track_data(data)
    track_choices <- if (!is.null(td) && ncol(td) > 0) names(td) else character(0)
    d2 <- as_event(data, track_choices)
    event_attr <- sf::st_drop_geometry(d2)
    choices <- names(event_attr)[vapply(event_attr, function(x) any(!is.na(x)), logical(1))]
    choices <- sort(unique(choices))
    logger.info("attr_choices_all computed | %d choices", length(choices))
    choices
  })
  
  output$select_attr_ui <- renderUI({
    ch <- attr_choices_all()
    restored_attr <- isolate(input$select_attr) %||% character(0)
    restored_attr <- intersect(restored_attr, ch)
    logger.info("Render select_attr_ui | choices=%d | selected=%d", length(ch), length(restored_attr))
    selectInput(
      ns("select_attr"),
      "Optionally: Select other attributes to show in the pop-up at each point (multiple can be selected):",
      choices = ch,
      selected = restored_attr,
      multiple = TRUE
    )
  })
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids_vec)
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  ## initialize
  init_done <- reactiveVal(FALSE)
  
  observeEvent(input$animals, {
    if (isTRUE(init_done())) return()
    if (is.null(input$animals)) return()
    
    d0 <- selected_data()
    if (nrow(d0) == 0) return()
    
    locked_data(d0)
    locked_settings(list(
      animals = input$animals,
      select_attr = input$select_attr %||% character(0)
    ))
    
    
    tryCatch({
      save_artifact_map()
    }, error = function(e) {
      logger.info("Artifact save failed on init: %s", e$message)
    })
    
    current(d0)
    
    init_done(TRUE)
    logger.info("INIT done | locked rows=%d | animals=%d | attrs=%d",
        nrow(d0), length(input$animals), length(input$select_attr %||% character(0)))
  }, ignoreInit = FALSE)
  
  # keep attr selection valid when animals change
  observeEvent(input$animals, {
    d <- selected_data()
    if (nrow(d) == 0) {
      logger.info("Animals changed so empty selection.")
      return()
    }
    
    td <- mt_track_data(d)
    track_choices <- if (!is.null(td) && ncol(td) > 0) names(td) else character(0)
    
    d2 <- as_event(d, track_choices)
    event_attr <- sf::st_drop_geometry(d2)
    choices <- names(event_attr)[vapply(event_attr, function(x) any(!is.na(x)), logical(1))]
    choices <- sort(unique(choices))
    
    prev <- isolate(input$select_attr) %||% character(0)
    sel  <- intersect(prev, choices)
    
    logger.info("updateSelectInput(select_attr) | choices=%d | keep_selected=%d", length(choices), length(sel))
    updateSelectInput(session, "select_attr", choices = choices, selected = sel)
  }, ignoreInit = FALSE)
  
  # Apply button locks what the map shows
  observeEvent(input$apply_btn, {
    
    if (is.null(input$animals) || length(input$animals) == 0) {
      logger.info("Apply blocked: no animals selected.")
      apply_warning(TRUE)
      return()
    }
    
    apply_warning(FALSE)
    d_applied <- selected_data()
    
    locked_data(d_applied)
    locked_settings(list(
      animals = input$animals,
      select_attr = input$select_attr %||% character(0)
    ))
    
    # IMPORTANT for MoveApps output.rds:
    tryCatch({
      save_artifact_map()
    }, error = function(e) {
      logger.info("Artifact save failed on apply: %s", e$message)
    })
    
    current(d_applied)
    logger.info("Apply ok -> locked rows=%d", nrow(d_applied))
    
  }, ignoreInit = TRUE)
  
  ####### Map ######
  track_lines <- reactive({
    d <- locked_data() %||% selected_data()
    req(d)
    if (is.null(d) || nrow(d) < 2) return(NULL)
    mt_track_lines(d)
  })
  
  mmap <- reactive({
    d <- locked_data() %||% selected_data()
    
    if (is.null(d) || nrow(d) == 0) {
      logger.info("mmap: empty -> base map only")
      return(leaflet() %>% addProviderTiles("OpenStreetMap"))
    }
    
    s <- locked_settings() %||% list(select_attr = character(0))
    attr_sel <- s$select_attr %||% character(0)
    
    ids <- sort(unique(as.character(d[[track_col]])))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    d$col <- pal(as.character(d[[track_col]]))
    if (length(attr_sel)) d <- as_event(d, attr_sel)
    d$popup_html <- make_popup(d, track_col, attr_sel)
    
    bounds <- as.vector(sf::st_bbox(d))
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addScaleBar(position = "topleft") %>%
      addCircleMarkers(
        data = d, radius = 1, opacity = 0.7, fillOpacity = 0.5,
        color = ~col, popup = ~popup_html, group = "Points"
      )
    
    tl <- track_lines()
    if (!is.null(tl) && nrow(tl) > 0) {
      tl$col <- pal(as.character(tl[[track_col]]))
      m <- m %>% addPolylines(data = tl, weight = 3, opacity = 0.5, color = ~col, group = "Lines")
    } else {
      logger.info("mmap: no lines layer")
    }
    
    m %>%
      addLegend(position = "bottomright", pal = pal, values = ids,
                title = "Tracks", opacity = 0.8, group = "Legend") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("Lines", "Points", "Legend"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$leafmap <- renderLeaflet({
    mmap()
  })
  
  
  #### download HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("LeafletMap_", Sys.Date(), ".html"),
    content = function(file) {
      logger.info("Download HTML -> %s", file)
      saveWidget(isolate(mmap()), file = file, selfcontained = TRUE)
    }
  )
  
  #### save map as PNG
  output$save_png <- downloadHandler(
    filename = function() paste0("LeafletMap_", Sys.Date(), ".png"),
    content = function(file) {
      logger.info("Download PNG -> %s", file)
      html_file <- tempfile(fileext = ".html")
      saveWidget(isolate(mmap()), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot(url = html_file, file = file, vwidth = 1000, vheight = 800)
    }
  )
  
  # MoveApps will store current() into output.rds
  return(reactive({
    req(current())
    current()
  }))
}