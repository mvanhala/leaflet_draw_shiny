
library(shiny)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sp)
library(rgeos)
library(dplyr)
library(purrr)
library(sweetalertR)
library(DT)
library(V8)

quakes <- readRDS("data/quakes.rds")
ports <- readRDS("data/ports.rds")

magcol <- colorNumeric(
  palette = "plasma",
  domain = quakes$magnitude)

icons <- awesomeIcons(
  icon = "ship",
  iconColor = "black",
  library = "fa",
  markerColor = "green"
)
  
ui <- bootstrapPage(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.swal = function(params) { swal.apply(this, params); }"),
  sweetalert(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("quakes", width = "100%", height = "100%"),
  absolutePanel(
    class = "panel panel-default", 
    fixed = TRUE, 
    top = 20, left = "auto", right = 20, bottom = "auto",
    width = 330, height = "auto",
    checkboxGroupInput(
      "harbors_picked", 
      h4("Harbor sizes to include"),
      c("Large", "Medium", "Small", "Very small"),
      selected = c("Large", "Medium"), 
      inline = TRUE
    ),
    br(),
    h3(textOutput("num_ports")),
    br(),
    DT::dataTableOutput("port_sizes")
  ),
  title = "Ports and 2016 Earthquakes"
)

server <- function(input, output, session) {
  output$quakes <- renderLeaflet({
    leaflet(data = quakes) %>% 
      addCircles(
        lat = ~lat, lng = ~lng, radius = ~rad,
        weight = 0, fillOpacity = 0.7, fillColor = ~magcol(magnitude)
      ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addLegend(
        "bottomleft", title = "Magnitude", pal = magcol,
        values = ~magnitude, opacity = 1
      ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(
          showArea = FALSE,
          shapeOptions = drawShapeOptions(color = "blue", fillColor = "blue", fillOpacity = 0.1)
        ),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          showArea = FALSE,
          shapeOptions = drawShapeOptions(color = "blue", fillColor = "blue", fillOpacity = 0.1)
        ),
        markerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  })
  
  ports_filtered <- reactive({
    ports[ports$harborsize %in% input$harbors_picked, ]
  })
  
  drawn_poly <- reactive({
    req(input$quakes_draw_all_features)
    features <- input$quakes_draw_all_features$features
    
    if (length(features) > 0) {
      features <- features %>%
        keep(~ .[["geometry"]][["type"]] != "Point")
    }
    
    if (length(features) == 0) {
      return(SpatialPolygons(list(), proj4string = CRS(proj4string(ports))))
    }
    
    features %>%
      map(~ .[["geometry"]][["coordinates"]][[1]]) %>%
      modify_depth(1, compose(simplify_all, transpose)) %>%
      map(~ matrix(c(.[[1]], .[[2]]), ncol = 2)) %>%
      map(~ Polygons(list(Polygon(.)), ID = dplyr:::random_table_name())) %>%
      SpatialPolygons(proj4string = CRS(proj4string(ports)))
  })
  
  selected_ports <- reactive({
    req(drawn_poly())
    
    if (length(drawn_poly()) == 0) {
      return(ports[integer(0), ])
    }
    
    test_poly_valid <- try(gIsValid(drawn_poly()))
    
    if (inherits(test_poly_valid, "try-error") || !test_poly_valid) {
      shinyjs::js$swal(
        "Invalid polygon", 
        "You have drawn a self-intersecting polygon. Please remove the invalid polygon.", 
        "error"
      )
      return(ports[integer(0), ])
    } 
    
    picked_ports <- ports[ports$harborsize %in% input$harbors_picked, ]
    port_numbers <- which(unname(!is.na(sp::over(picked_ports, drawn_poly()))))
    picked_ports[port_numbers, ]
  })
  
  output$num_ports <- renderText({
    req(nrow(selected_ports()) > 0)
    paste(nrow(selected_ports()), "ports in selected polygons")
  })
  
  output$port_sizes <- DT::renderDataTable({
    req(nrow(selected_ports()) > 0)
    selected_ports()@data %>%
      count(harbortype, sort = TRUE) %>%
      mutate(harbortype = coalesce(harbortype, "(Missing)")) %>%
      datatable(
        rownames = FALSE,
        colnames = c("Harbor type", "Ports"),
        options = list(
          dom = "t", 
          ordering = FALSE,
          pageLength = nrow(.)
        )
      )
  })
  
  observe({
    ports <- selected_ports()
    proxy <- leafletProxy("quakes") %>%
      clearMarkerClusters()
    
    if (nrow(ports) > 0) {
      proxy <- proxy %>%
        addAwesomeMarkers(
          data = ports, 
          icon = icons, 
          label = ~txt,
          clusterOptions = markerClusterOptions()
        )
    }
  })
}

shinyApp(ui, server)

