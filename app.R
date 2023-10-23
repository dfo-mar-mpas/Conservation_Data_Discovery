library(shiny)
library(leaflet)
library(arcpullr)
library(dplyr)

# data prep
if(!file.exists("MPAs.rds")){
  url <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Oceans_Act_Marine_Protected_Areas/MapServer/0"
  MPAs <- get_spatial_layer(url) %>% 
    st_make_valid()%>% 
    rowwise() %>% 
    mutate(EDH=paste0("https://edh.intra.azure.cloud.dfo-mpo.gc.ca/catalogue/srv/eng/catalog.search#/search?resultType=details&sortBy=relevance&fast=index&_content_type=json&from=1&to=20&geometry=",
                      st_as_text(st_as_sfc(st_bbox(geoms))) %>% 
                        gsub(", ",",",.) %>% 
                        gsub(" ","%20",.)),
           OBIS=paste0("https://mapper.obis.org/?geometry=",
                       st_as_text(st_as_sfc(st_bbox(geoms))) %>% 
                         gsub(", ",",",.) %>% 
                         gsub(" ","%20",.),"#"),
           CIOOS=paste0("https://catalogue.cioos.ca/dataset/?sort=score+desc%2C+metadata_modified+desc&ext_bbox=",
                        paste0(st_bbox(geoms),collapse = "%2C"),
                        "&ext_year_begin=&ext_year_end=&ext_show_empty_range="))
  saveRDS(MPAs,"MPAs.rds")
} else {
  MPAs <- readRDS("MPAs.rds")
}


# custom functions
## leaflet clickable polygons
jsCode <- paste0('
 function(el, x, data) {
  var marker = document.getElementsByClassName("leaflet-interactive");
  for(var i=0; i < marker.length; i++){
    (function(){
      var v = data.link[i];
      marker[i].addEventListener("click", function() { window.open(v);}, false);
  }()); 
  }
 }
')

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Proof of concept - Data discovery tool for Maritimes Conservation Network"),
  
  # Sidebar with a dropwdown down menu for data source 
  sidebarLayout(
    sidebarPanel(
      selectInput("source",
                  "Data Source:",
                  c("EDH",
                    "OBIS",
                    "CIOOS")
      )
    ),
    
    # Show the map with links
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  reactMPA <- reactive({
    MPAs %>% 
      mutate(link=switch(input$source,
                         "EDH"=EDH,
                         "OBIS"=OBIS,
                         "CIOOS"=OBIS))
  })
  
  
  output$map <- renderLeaflet({
    leaflet(MPAs) %>% 
      addTiles() %>% 
      addPolygons(label = ~NAME_E)  %>%
      htmlwidgets::onRender(jsCode, data=reactMPA())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
