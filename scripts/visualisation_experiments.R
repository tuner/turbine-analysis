library(rgdal)
library(leaflet)
library(sp)
library(shiny)

# Read districts shapes
# Data are read for 2015 as an actual poll was done that year
helsinki_districts <- spTransform(
  readOGR(file.path("..","raw_data","piirialuejako-1995-2016.gpkg"), layer="perus_2015"),
  CRS("+proj=longlat")
)

# Prepare districts data set for merge
names(helsinki_districts)[names(helsinki_districts)=="PERUS"] <- "District id"

# Read "census" data
census_data <- read.csv(file.path("..", "derived_data", "combined_data.csv"), sep="\t", fileEncoding = "utf8", encoding = "utf8", check.names = FALSE)

rb_options <- list()

for (i in 2:17){
  rb_options[names(census_data)[i]] <- i + 6
}

# Merge census data and districts shapes
combined_data <- merge(helsinki_districts, census_data, by="District id")

factors <- 24:length(names(combined_data@data))
plots_id <- lapply(factors, function(x){ paste0(names(combined_data@data)[x], "plot") })
tabs <- lapply(factors, function(x){
  tabPanel(names(combined_data@data)[x], plotOutput(plots_id[x - min(factors)]))
})

ui <- fluidPage(
  titlePanel("Helsinki map"),
  sidebarLayout(
                sidebarPanel("Questions",
                  radioButtons("question", label="", choices = rb_options)
                ),
                mainPanel("Helsinki map",
                          leafletOutput("helsinki_map"),
                          uiOutput("Factors")
                )
  )
)

server <- function(input, output, session){
  output$helsinki_map <- renderLeaflet({
    column_id<-strtoi(input$question)
    leaflet(combined_data) %>%
        addTiles() %>%
        fitBounds(24.78516,60.09772, 25.27679, 60.31403) %>%
        addPolygons(
          weight=1,
          fillColor=~colorNumeric("PiYG", -2:2)(combined_data@data[,column_id]),
          fillOpacity = 0.5,
          layerId = ~`District id`
        ) %>%
      addLegend(
        position="bottomright", 
        pal=colorNumeric("PiYG", -2:2),
        values=~combined_data@data[,column_id],
        title=names(combined_data@data)[column_id]
      )
  }) 
  
  output$Factors <- renderUI({
    do.call(tabsetPanel, tabs)
  })
  
  lapply(plots_id, function(x) { print(output[x])})

  observeEvent(input$helsinki_map_shape_click, {
    click <- input$helsinki_map_shape_click
  })
}

shinyApp(ui, server)