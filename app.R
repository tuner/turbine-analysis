library(rgdal)
library(leaflet)
library(sp)
library(shiny)
library(plotly)
library(weights)

setwd("scripts")
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
  "Poll explainer!",
  tabPanel("Compare",
    fluidPage(
      fluidRow(
      ),
      fluidRow(
        column(6, verticalLayout(
          p(
            "This page visualizes correlations between poll responses and 
            population/districts characteristics based on geographical districts.
            To compare, choose a question and a
            census variable. The scatter plot provides a visual overview and
            more exact results are provided as model summaries. The point size in the
            plot encodes the number of responders in the district.
            If you click a point in the scatter plot, a corresponding district will be highlited on the map.
            The map can show geographical distributions for either poll response or the census variable.
            The domain of poll responses is from -2 (disagree) to 2 (agree).
            "),
            selectInput(
              "question", 
              label = h4("Poll question"), 
              choices = question.select.choices, 
              selected = 3, 
              width = "100%"
            ),
            selectInput(
              "factor",
              label = h4("Census variable (a factor)"), 
              choices = factor.select.choices, 
              selected = 10,
              width = "100%"
            )
          )
        ),
        column(6, verticalLayout(
            h4("Regression model summary"),
            tableOutput("model.summary"),
            h4("Correlation summary"),
            tableOutput("correlation.summary"),
            uiOutput("correlation.summary.text")
          )
        )
      ),
      
      hr(style="border: 0; border-bottom: 1px dashed gray"),
      
      fluidRow(
        style="margin-bottom: 20px",
        column(6,
               plotlyOutput("scatter.plot")
        ),
        column(6, verticalLayout(
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
    p("Correlation matrix visualizes correlations between all
      variable pairs. Only correlations that have statistical significance,
      i.e. p-value < 0.5, are shown. This plot is static;
      no fancy interactivity here, sorry!"),
      plotOutput("corr.plot")
    )
  ),
  
  tabPanel("Factors", verticalLayout(
    p("This page shows the coefficients of a regularized multiple linear regression.
      The data has first been normalized (centered and unit variance).
      The bars show a combination of census variables that best explain
      the response to the chosen question."),
    selectInput(
      "question1", 
      h4("Poll question"), 
      choices = question.select.choices, 
      selected = 3,
      width = "100%"
    ),
    plotOutput("reduced.factors.plot")
  )),
  tabPanel("About",
           h3("What is this?"),
           p("With this application, you can study which census variables correlate with 
             responses to a poll about wind turbine construction."),
           p("The application is a part of a project on the Introduction to Data Science
             course provided by the University of Helsinki."),
           
           h3("Who made this?"),
           p("The project team consists of Kari Lavikka and Vladimir Dobrodeev."),
           
           h3("Where did you get the data?"),
           p("The data are publicly available. Their sources are as follows:"),
           HTML("
<ul>
  <li>Helsingin tuulivoimakysely, <a href=\"https://www.avoindata.fi/data/fi/dataset/helsingin-tuulivoimakysely-2015\">https://www.avoindata.fi/data/fi/dataset/helsingin-tuulivoimakysely-2015</a></li>
  <li>Helsinki alueittain, <a href=\"https://www.avoindata.fi/data/fi/dataset/helsinki-alueittain\">https://www.avoindata.fi/data/fi/dataset/helsinki-alueittain</a></li>
  <li>Helsingin piirijako, <a href=\"https://www.avoindata.fi/data/fi/dataset/helsingin-piirijako\">https://www.avoindata.fi/data/fi/dataset/helsingin-piirijako</a></li>
  <li>OpenAddresses, The free and open global address collection, <a href=\"https://openaddresses.io/\">https://openaddresses.io/</a></li>
</ul>
                "),
           
           h3("Anything else?"),
           HTML("
                <p>Yeah, the source code and everything related to the project is available on GitHub:
                <a href=\"https://github.com/tuner/turbine-analysis\">https://github.com/tuner/turbine-analysis</a></p>")
           
  )
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
    strong("significant", style="color: green")
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
          fillColor=~colorNumeric("YlOrRd", domain)(shapes.values),
          fillOpacity = 0.5,
          layerId = ~PERUS
        ) %>%
        addLegend(
          position="bottomright",
          pal=colorNumeric("YlOrRd", domain),
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
          size = sqrt(project.data$n / max(project.data$n)) * 20
        )
      ) %>%
      add_trace(
        name="Regression line",
        mode="lines",
        x=vars$x,
        y=fitted(vars$model),
        marker=list()
      ) %>%
      layout(
        showlegend = FALSE, # Meanings are pretty obvious anyway
        xaxis = list(title = colnames(project.data$census)[strtoi(input$factor)]),
        yaxis = list(title = colnames(project.data$turbine)[strtoi(input$question)])
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
    verticalLayout(
      h4("Results"),
      p(
        paste0("Correlation between responses to question \"",
               names(project.data$turbine)[strtoi(input$question)],
               "\" and factor \"",
               names(project.data$census)[strtoi(input$factor)],
               "\" is "),
        get_correlation_info(vars$corr[1]),
        "and statistically",
        get_correlation_text(vars$corr[4]),
        "."
      )
    )
  })
  
  output$corr.plot <- renderPlot(plot_correlations(project.data, cex = 0.9))
  
  output$reduced.factors.plot <- renderPlot({
    question.id <- strtoi(input$question1)
    explain_variable(project.data$turbine[,question.id][[1]], project.data, cex = 0.9)
  })
}

shinyApp(
  tagList(
    tags$head(tags$style(
      HTML("
p, table {
  font-size: 13px;
}

h4 {
  margin-top: 8px;
  margin-bottom: 4px;
}
        "))),
    ui.main),
  server)
