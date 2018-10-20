library(rgdal)
library(leaflet)
library(sp)
library(shiny)

# Read districts shapes
helsinki_districts <- spTransform(
  readOGR(file.path("..","raw_data","piirialuejako-1995-2016.gpkg"), layer="perus_2016"),
  CRS("+proj=longlat")
)

names(helsinki_districts)[names(helsinki_districts)=="PERUS"] <- "District.id"

data <- read.csv(file.path("..", "derived_data", "combined_data.csv"), sep="\t", fileEncoding = "utf8", encoding = "utf8")

districts <- merge(helsinki_districts, data, by="District.id")

rb_options <- list()

for (i in 8:23){
  rb_options[gsub("\\."," ", names(districts)[i])] <- i
}

ui <- fluidPage(
  titlePanel("Helsinki map"),
  sidebarLayout(position = "right",
                sidebarPanel("Questions",
                radioButtons("question", label="", choices = rb_options)
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
    leaflet(districts) %>%
        addTiles() %>%
        fitBounds(24.78516,60.09772, 25.27679, 60.31403) %>%
        addPolygons(
          weight=1,
          fillColor=~colorNumeric("PiYG", districts@data[,column_id])(districts@data[,column_id]),
          fillOpacity = 0.5,
          layerId = ~District.id
        ) %>%
      addLegend(
        position="bottomright", 
        pal=colorNumeric("PiYG", districts@data[,column_id]),
        values=~districts@data[,column_id],
        title=gsub("\\.", " ", names(districts@data)[column_id])
      )
  })

  observeEvent(input$helsinki_map_shape_click, {
    click <- input$helsinki_map_shape_click
    output$selected_region <- renderText(
		as.character.factor(districts[districts$District.id==click$id,]$Nimi)
    )
  })
}

shinyApp(ui, server)