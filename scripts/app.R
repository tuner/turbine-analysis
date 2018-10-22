library(rgdal)
library(leaflet)
library(sp)
library(shiny)
library(plotly)

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

factors <- 26:length(names(combined_data@data))
plots_id <- lapply(factors, function(x){ paste0(names(combined_data@data)[x], "plot") })
tabs <- lapply(factors, function(x){
  tabPanel(names(combined_data@data)[x], plotlyOutput(plots_id[x - min(factors) + 1]))
})

ui <- fluidPage(
  titlePanel("Helsinki map"),
  sidebarLayout(
                sidebarPanel("Questions",
                  radioButtons("question", label="", choices = rb_options)
                ),
                mainPanel("Factors information",
                          uiOutput("Factors"),
                          leafletOutput("helsinki_map")
                )
  )
)

server <- function(input, output, session){
  output$Factors <- renderUI({
    do.call(tabsetPanel, tabs)
  })
  
  lapply(factors, function(f) { output[[plots_id[[f - min(factors) + 1]]]] <- renderPlotly({
      column_id<-strtoi(input$question)
      plot_ly(
        type="scatter",
        mode="markers",
        y = combined_data@data[,column_id], 
        x = combined_data@data[,f],
        size = combined_data$n,
        symbol = "circle"
        #symbols = combined_data$Nimi
        #sizes = c(min(combined_data$n), max(combined_data$n))
      )
    }) 
  })

  # observeEvent(input$helsinki_map_shape_click, {
  #   click <- input$helsinki_map_shape_click
  # })
  
  observe({
    plot_data <- event_data("plotly_click")
    selected_row <- combined_data[plot_data[["pointNumber"]],]
    output$helsinki_map <- renderLeaflet({
      column_id<-strtoi(input$question)
      leaflet(selected_row) %>%
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
  })
}

shinyApp(ui, server)