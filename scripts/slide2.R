# The script generates a map for slide 2 of the presentation
library(rgdal)
library(leaflet)
library(shiny)
library(sp)

postareas <- spTransform(
  readOGR(file.path("..","raw_data","PKS_postinumeroalueet_2017_shp.shp")),
  CRS("+proj=longlat")
)
postareas$Posno <- as.numeric(as.character(postareas$Posno))
postareas <- postareas[postareas$Posno>=2&postareas$Posno<=990,]

turbine_zip_data <- read.csv(file.path("..","derived_data","zip_aggregated_turbine.csv"), sep="\t")

names(postareas)[names(postareas)=="Posno"] <- "Zip"

postareas_and_aggregated_data <- merge(postareas, turbine_zip_data, by="Zip")

turbine_poll_questions <- list()

for (i in 8:23){
  turbine_poll_questions[gsub("\\."," ", names(postareas_and_aggregated_data)[i])] <- i
}

ui <- fluidPage(
  titlePanel("Helsinki map"),
  sidebarLayout(position = "right",
                sidebarPanel("Questions",
                             radioButtons("question", label="", choices = turbine_poll_questions, selected=8)
                ),
                mainPanel("Helsinki map",
                          leafletOutput("helsinki_map"),
                          textOutput("selected_region")
                )
  )
  
)

server <- function(input, output, session){
  output$helsinki_map <- renderLeaflet({
    column_id<-strtoi(input$question)
    leaflet() %>%
      fitBounds(24.78516,60.09772, 25.27679, 60.31403) %>%
      addPolygons(
        postareas_and_aggregated_data,
        weight=1,
        fillColor=~colorNumeric("PiYG", postareas_and_aggregated_data@data[,column_id])(postareas_and_aggregated_data@data[,column_id]),
        fillOpacity = 1,
        layerId = ~Zip
      )
  })
}

shinyApp(ui, server)