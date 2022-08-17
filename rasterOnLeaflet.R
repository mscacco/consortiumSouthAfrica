library(move)
library(sp)
library(raster)
library(RColorBrewer)
library(mapview)
library(pals)
library(leaflet)
library(leaflet.extras)

creds <- movebankLogin("afriRaptors", "afriRaptors2022")
studies <- getMovebank("study", login=creds)
studies <- studies[studies$i_am_collaborator=="true",]

# movestack of all individuals and studies
data1 <- getMovebankData(study=studies[4,"id"], login=creds, includeExtraSensors=F, removeDuplicatedTimestamps=T)
data2 <- getMovebankData(study=studies[5,"id"], login=creds, includeExtraSensors=F, removeDuplicatedTimestamps=T)
data <- moveStack(data1, data2)

### User defined values:
pxSize = 0.5 #value in degree set by the user to define the resolution of the raster
entity = c("n_locations","n_individuals","n_species","n_studies") #the user defines which information they want summarized (rasterized) in the final map
entity=entity[3]

SP <- SpatialPointsDataFrame(coords=as.data.frame(data)[,c("location_long","location_lat")], 
                             data=as.data.frame(data), 
                             proj4string=CRS("+proj=longlat +ellps=WGS84 +no_defs"))
# maxExtDeg <- max(c(diff(range(coordinates(SP)[,"location_long"])), diff(range(coordinates(SP)[,"location_lat"]))))
# mid <- colMeans(coordinates(SP)) #this is to obtain a projection centered around the data points
# rr <- raster(ext=extent(SPT), resolution=input$grid, crs = paste0("+proj=aeqd +lat_0=",mid[2]," +lon_0=",mid[1]," +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"), vals=NULL)
rr <- raster(ext=extent(SP), resolution=pxSize, crs =CRS("+proj=longlat +ellps=WGS84 +no_defs"), vals=NULL)

if(entity=="n_locations"){
  SPr <- rasterize(SP, rr, field="event_id", fun="count", update=TRUE) #why do we need update=T?
  legendTitle <- "N. of GPS locations"
  #plot(SPr)
}else if(entity=="n_individuals"){
  SPr <- rasterize(SP, rr, field="local_identifier", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
  legendTitle <- "N. of individuals"
  #plot(SPr)
}else if(entity=="n_species"){
  SPr <- rasterize(SP, rr, field="taxon_canonical_name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
  legendTitle <- "N. of species"
  #plot(SPr)
}#else if(entity=="n_studies"){
#   # we could do the same with number of studies but using getMovebankData study.id doesn't get included in the dataframe?
#   SPr <- rasterize(SP, rr, field="study_id", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
#   legendTitle <- "N. of Movebank studies"
#   #plot(SPr)
# }


#mmap <- reactive({
  bounds <- as.vector(bbox(extent(data)))
  SPr_l <- projectRasterForLeaflet(SPr, method = "ngb")
  brewCol <- brewer.pal(7, name = "YlGnBu")
  rPal <- colorNumeric(brewCol, values(SPr_l), na.color = "transparent", reverse = F)

  outl <- leaflet() %>% 
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
    addTiles() %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE) %>%
    addLegend(position="topright", opacity = 0.6, bins = 7, #colors = rPal, 
              pal = rPal, values = values(SPr_l), title = legendTitle) %>%
    addScaleBar(position="bottomright",
                options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE))
    # addLayersControl(
    #   baseGroups = c("TopoMap", "Aerial"),
    #   overlayGroups = ids,
    #   options = layersControlOptions(collapsed = FALSE)
    # )
  outl   
#})