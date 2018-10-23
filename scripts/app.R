library(rgdal)
library(leaflet)
library(sp)
library(shiny)
library(plotly)
library(weights)

source("common.R")

# Loading projects data
project.data <- load_data()

# Loading geospatial data (Helsinki district shapes)
helsinki.districts.shapes <- spTransform(
  readOGR(file.path("..", "raw_data", "piirialuejako-1995-2016.gpkg"), layer="perus_2015"),
  CRS("+proj=longlat")
)

question.select.choices <- list()
for (i in 1:length(names(project.data$turbine))){
  question.select.choices[[names(project.data$turbine)[i]]] <- i
}

factor.select.choices <- list()
for (i in 1:length(names(project.data$census))){
  factor.select.choices[[names(project.data$census)[i]]] <- i
}

leaflet.indicies <- sapply(project.data$district_id, function(x) which(helsinki.districts.shapes$PERUS == x))
census.indices <- sapply(helsinki.districts.shapes$PERUS, function(x) which(project.data$district_id == x))

ui.main <- navbarPage(
  "Poll factors!",
  tabPanel("Compare",
    fluidPage(
      h3("INSTRUCTIONS"),
      fluidRow(
        p(style="font-size:14pt", "This page provides some visualtions for correlation between responses and 
          population/districts characteristics. You can choose one of questions and some factor, which
          might affect it (for example, percent of Finnish-speaking inhabitants). The page will show 
          some information about correlation and linear regression model. It will also show a scatter plot
          showing how question and factor values are related. If you click a point at scatter plot,
          a corresponding district will be highlited on the map. The map can show geographical distributions
          for responses and factor value.")
      ),
      fluidRow(
        column(4, mainPanel(
            selectInput(
              "question", 
              h3("Select a question"), 
              choices = question.select.choices, 
              selected = 1, 
              width = 700
            ),
            selectInput(
              "factor",
              h3("Select a factor"), 
              choices = factor.select.choices, 
              selected = 1,
              width = 700
            )
          )
        ),
        column(8, mainPanel(
            h3("Regression model summary"),
            tableOutput("model.summary"),
            h3("Correlation summary"),
            tableOutput("correlation.summary"),
            uiOutput("correlation.summary.text")
          )
        )
      ),
      hr(style="border:2px solid black"),
      fluidRow(
        column(6, mainPanel(
            plotlyOutput("scatter.plot")
          )
        ),
        column(6, mainPanel(
            radioButtons(
              "map.mode", 
              label="Map mode", 
              choices = list(
                "Show responses distribution" = 1,
                "Show factor distribution" = 2
              ), 
              selected=1, 
              inline = TRUE
            ),
            leafletOutput("helsinki.map")
          )
        )
      )
    )
  ),
  tabPanel("Correlation matrix", fluidPage(
      h3("INSTRUCTIONS"),
      fluidRow(
        p(style="font-size:14pt", "This page shows joint information about correlations between factors and 
          responses. This page just shows the information.")
      ),
      plotOutput("corr.plot")
    )
  ),
  tabPanel("Factors", mainPanel(
    h3("INSTRUCTIONS"),
    fluidRow(
      p(style="font-size:14pt", "This page shows joint information about how different factors contribute to
        responses. Size of a bar shows how strong a correlation between responses and a
        factor is. To see the information for a certain question,
        this question should be selected from the dropdown list.")
    ),
    selectInput(
      "question1", 
      h3("Select a question"), 
      choices = question.select.choices, 
      selected = 1, 
      width = 800
    ),
    plotOutput("reduced.factors.plot")
  )),
  tabPanel("About")
)

selected_row <- NULL

draw_selected_district <- function(selected_point, proxy){
  if (is.null(selected_point) == FALSE && selected_point[["curveNumber"]] == 0){
    proxy %>% removeShape("selected_district")
    selected_row <- selected_point[["pointNumber"]] + 1
    selected_polygon <- helsinki.districts.shapes@polygons[[leaflet.indicies[[selected_row]]]]
    polygon_labelPt <- selected_polygon@labpt
    proxy %>%
      setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=12) %>%
      addPolylines(weight=10, color="red", data=selected_polygon, layerId = "selected_district")
  }
}

get_correlation_text <- function(p.value){
  if (p.value < 0.05){
    strong("significant", style="background:rgb(255,128,169)")
  }
  else{
    em("insignificant")
  }
}

get_correlation_info <- function(correlation){
  if (correlation >= 0){
    span("positive", style="color:green")
  }
  else{
    span("negative", style="color:red")
  }
}

server <- function(input,output,session){
  vars <- reactiveValues()

  proxy <- leafletProxy("helsinki.map")

  observe({
    # Fill reactive varibale (to avoid recalculations)
    x <- as.list(project.data$census[,strtoi(input$factor)])[[1]]
    y <- as.list(project.data$turbine[,strtoi(input$question)])[[1]]
    vars$x <- x
    vars$y <- y
    vars$model <- lm(y ~ x, weights=project.data$n)
    # names(vars$model) <- c("Estimate", "Standard error", "t-value", "p-value")
    vars$corr <- wtd.cor(y, x, project.data$n)
    # Handle plot click or select event
    draw_selected_district(event_data("plotly_click"), proxy)
    
    if (input$map.mode==1){
      column.id <- strtoi(input$question)
      shapes.values = project.data$turbine[census.indices, column.id][[1]]
      domain = -2:2
      map.name <- names(project.data$turbine)[column.id]
      proxy %>% removeShape(helsinki.districts.shapes$PERUS) %>% clearControls()
      proxy %>%
        addPolygons(
          data=helsinki.districts.shapes,
          weight=1,
          fillColor=~colorNumeric("PiYG", domain)(shapes.values),
          fillOpacity = 0.5,
          layerId = ~PERUS
        ) %>%
        addLegend(
          position="bottomright",
          pal=colorNumeric("PiYG", domain),
          values=shapes.values,
          title=substr(map.name,1,10)
        )
    }
    else{
      column.id <- strtoi(input$factor)
      shapes.values = project.data$census[census.indices, column.id][[1]]
      domain = 0:max(project.data$census[census.indices, column.id][[1]])
      map.name <- names(project.data$census)[column.id]
      proxy %>% removeShape(helsinki.districts.shapes$PERUS) %>% clearControls()
      proxy %>%
        addPolygons(
          data=helsinki.districts.shapes,
          weight=1,
          fillColor=~colorNumeric("PiYG", domain)(shapes.values),
          fillOpacity = 0.5,
          layerId = ~PERUS
        ) %>%
        addLegend(
          position="bottomright",
          pal=colorNumeric("PiYG", domain),
          values=shapes.values,
          title=substr(map.name,1,10)
        )
    }
    
    
  })

  output$scatter.plot <- renderPlotly({
    colors <- rep("green",nrow(project.data$census))
    plot_ly() %>%
      add_trace(
        name="Districts",
        x=vars$x,
        y=vars$y,
        text = helsinki.districts.shapes$Nimi[leaflet.indicies],
        mode="markers",
        marker=list(
          color=colors,
          size = sqrt(project.data$n)
        )
      ) %>%
      add_trace(
        name="Regression line",
        mode="lines",
        x=vars$x,
        y=fitted(vars$model),
        marker=list()
      )
  })

  output$helsinki.map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(24.78516, 60.09772, 25.27679, 60.31403)
  })

  output$model.summary <- renderTable(
    {
      tbl<-as.data.frame(coef(summary(vars$model)))
      names(tbl)<-c("Estimate","Standard error","t-value","p-value")
      row.names(tbl) <- c("Intercept","Slope")
      tbl
    }, 
    digit=5, 
    rownames = TRUE
  )
  
  output$correlation.summary <- renderTable(
    {
      tbl<-as.data.frame(vars$corr)
      names(tbl)<-c("Correlation","Standard error","t-value","p-value")
      row.names(tbl) <- c("Corr(Response, Factor)")
      tbl
    }, 
    digit=5, 
    rownames = TRUE
  )
  
  output$correlation.summary.text <- renderUI({
    mainPanel(
      h3("Results"),
      p("Correlation between responses to question \"",
        names(project.data$turbine)[strtoi(input$question)],
        "\" and factor",
        names(project.data$census)[strtoi(input$factor)],
        "is statistically",
        get_correlation_text(vars$corr[4]),
        "and",
        get_correlation_info(vars$corr[1]),
        style="font-size:14pt"
      ),
      width=1500
    )
  })
  
  output$corr.plot <- renderPlot(plot_correlations_1(project.data))
  
  output$reduced.factors.plot <- renderPlot({
    question.id <- strtoi(input$question1)
    explain_variable_1(project.data$turbine[,question.id][[1]], project.data)
  })
}

shinyApp(ui.main, server)