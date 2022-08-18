library(shiny)
library(move)
library(sp)
library(raster)
library(RColorBrewer)
library(mapview)
library(leaflet)

data("fishers")
data <- fishers

ui <- fluidPage(
  titlePanel("Rasterize n. of observations/individuals/species \n on a leaflet map"),
  fluidRow(
    column(4,
           sliderInput(inputId = "pxSize", 
                       label = "Choose the raster cell resolution in degrees", 
                       value = 0.5, min = 0.01, max = 5) # range of about 1 km to 500 km
    ),
    column(4,
           selectInput(inputId = "entity", 
                       label = "Choose which entity you want to rasterize", 
                       choices = list( "N. of GPS locations" = "n_locations", 
                                       "N. of individuals" = "n_individuals", 
                                       "N. of species" = "n_species", 
                                       "N. of Movebank studies" = "n_studies"),
                       selected = "n_locations")
    )),
  
  hr(),
  
  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("selected"))
  ),
  
  leafletOutput("leafmap"),
  downloadButton('savePlot', 'Save Plot')
)


server <- function(input, output, pxSize=0.5, entity="n_locations") {
  
  #### make map as reactive object to be able to save it ####
  rmap <- reactive({
 
    SP <- SpatialPointsDataFrame(coords=as.data.frame(data)[,c("location.long","location.lat")], 
                                 data=as.data.frame(data), 
                                 proj4string=CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    SP$rowNum <- 1:nrow(SP)
    rr <- raster(ext=extent(SP), resolution=input$pxSize, crs =CRS("+proj=longlat +ellps=WGS84 +no_defs"), vals=NULL)
    
    if(input$entity=="n_locations"){
      SPr <- rasterize(SP, rr, field="rowNum", fun="count", update=TRUE) #why do we need update=T?
      legendTitle <- "N. of GPS locations"
    }else if(input$entity=="n_individuals"){
      SPr <- rasterize(SP, rr, field="individual.local.identifier", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of individuals"
    }else if(input$entity=="n_species"){
      SPr <- rasterize(SP, rr, field="individual.taxon.canonical.name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of species"
    }else if(input$entity=="n_studies"){
      SPr <- rasterize(SP, rr, field="study.name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      legendTitle <- "N. of Movebank studies"
    }
    bounds <- as.vector(bbox(extent(data)))
    SPr_l <- projectRasterForLeaflet(SPr, method = "ngb")
    brewCol <- brewer.pal(7, name = "YlGnBu")
    rPal <- colorNumeric(brewCol, values(SPr_l), na.color = "transparent", reverse = F)
    
    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE, group = "raster") %>%
      addLegend(position="topright", opacity = 0.6, bins = 7, 
                pal = rPal, values = values(SPr_l), title = legendTitle) %>%
      addScaleBar(position="bottomright",
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("TopoMap", "Aerial"),
        overlayGroups = "raster",
        options = layersControlOptions(collapsed = FALSE)
      )
    
    outl   
  })
  
  ### render map to be able to see it ####
  output$leafmap <- renderLeaflet({
    rmap()
  })
  
  ### save map, takes some seconds ###
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("Leaflet_densityMap.png", sep="")
    },
    content = function(file) {
      leafmap <- rmap()
      mapshot( x = leafmap
               , remove_controls = "zoomControl"
               , file = file
               , cliprect = "viewport"
               , selfcontained = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)




