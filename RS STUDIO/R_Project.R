library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(maps)
library(leaflet)
library(plotrix)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "MY DASHBOARD", titleWidth = 800,
                    tags$li(class = "dropdown", tags$a(icon("github"), "SHRIKRISHNA NAYAK"))
                    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("DATA vISUALIZATION", tabName = "dataVisualization", icon = icon("clipboard")),
        menuItem("DATA MAP", tabName = "map", icon = icon("globe")),
        menuItem("DATA TABLE", tabName = "table", icon = icon("table"))
        
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
        tabItem( tabName = "dataVisualization",
                  fluidRow(
                   box( title = "INPUT WIDGET",background = "black", solidHeader = T,
                     selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                                 selected = "Blue",multiple = F),
                   
                   radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                   
                     selectInput(inputId="channel1",label="Choose Channel",choices = c("MPG"="MPG",
                                                                                       "Cylinders"="Cylinders",
                                                                                       "Displacement"="Displacement",
                                                                                       "Horsepower"="Horsepower",
                                                                                       "Weight"="Weight",
                                                                                       "Acceleration"="Acceleration",
                                                                                       "Model"="Model"),
                                 selected = "BC6",multiple = F
                     ),
                   
                   sliderInput("bins","Number Of Breaks",1,400,16)),
                   box(title = "BOX WITH PLOT",plotOutput("histogram", height = 340),status = "danger",solidHeader= T, collapsible = T)),
                   fluidRow(box(dataTableOutput("shortData")),
                   box(title = "BOX WITH PLOT",plotOutput("line"),width = 6, status = "primary", solidHeader = T, collapsible = T))
                   
                 
        ),
        tabItem(tabName = "table",
                dataTableOutput("carTable")
                ),
        tabItem(tabName = "map",
                  box( title = "REPRESENTATION OF DATA IN MAP", status = "primary",solidHeader = T, collapsible = T,
                  leafletOutput("mymap", height = 400), width = 8),
                  fluidRow(
                  box( title = "INPUT WIDGET", background = "black", solidHeader = T,
                  selectInput("bmap", "CHOOSE THE TYPE OF MAP", choices = c("Stamen.Toner", 
                                                                            "CartoDB.Positron",
                                                                            "Esri.WorldImagery",
                                                                            "Stamen.Watercolor"), 
                              selected = "CartoDB.Positron"),
                  actionButton("update", "Update Map!")
                ),
               box( title = "3D PIE CHAR", status = "danger", solidHeader = T, collapsible = T,
                 plotOutput("pieChart")
               )
                  )
                ))
      
    )
  ) 
)

server <- function(input, output, session) {
  
  output$histogram <- renderPlot({
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    if(input$channel1 == "MPG"){
      hist(cars$MPG,col = sColor, density = 70, angle = 45, breaks = input$bins, border = input$border1)
    }else if(input$channel1 == "Cylinders"){
      hist(cars$Cylinders,col = sColor, density = 70, angle = 45, breaks = input$bins, border = input$border1)
    }else if(input$channel1 == "Displacement"){
      hist(cars$Displacement,col = sColor, density = 70, angle = 45, breaks = input$bins, border = input$border1)
    }else if(input$channel1 == "Horsepower"){
      hist(cars$Horsepower,col = sColor, density = 70, angle = 45, breaks = input$bins,border = input$border1)
    }else if(input$channel1 == "Weight"){
      hist(cars$Weight,col = sColor, density = 70, angle = 45, breaks = input$bins, border = input$border1)
    }else if(input$channel1 == "Acceleration"){
      hist(cars$Acceleration,col = sColor, density = 70, angle = 45, breaks = input$bins, border = input$border1)
    }else if(input$channel1 == "Model"){
      hist(cars$Model,col = sColor, density = 70, angle = 45, breaks = input$bins,border = input$border1)
    }
  })
  output$carTable <- renderDataTable({
    cars
  })
  output$shortData <- renderDataTable({
    if(input$channel1 == "MPG"){
      cars %>% select(Car,MPG,Origin)
    }else if(input$channel1 == "Cylinders"){
      cars %>% select(Car,Cylinders,Origin)
    }else if(input$channel1 == "Displacement"){
      cars %>% select(Car,Displacement,Origin)
    }else if(input$channel1 == "Horsepower"){
      cars %>% select(Car,Horsepower,Origin)
    }else if(input$channel1 == "Weight"){
      cars %>% select(Car,Weight,Origin)
    }else if(input$channel1 == "Acceleration"){
      cars %>% select(Car,Acceleration,Origin)
    }else if(input$channel1 == "Model"){
      cars %>% select(Car,Model,Origin)
    }
  },
  options = list(pageLength = 10)
  )
  output$line <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    if(input$channel1 == "MPG"){
      plot(cars$MPG, type = "l", col = sColor,main = "GRAPH OF MPG" )
    }else if(input$channel1 == "Cylinders"){
      plot(cars$Cylinders, type = "l", col = sColor,main = "GRAPH OF CYLINDERS" )
    }else if(input$channel1 == "Displacement"){
      plot(cars$Displacement, type = "l", col = sColor, main = "GRAPH OF DISPLACEMENT" )
    }else if(input$channel1 == "Horsepower"){
      plot(cars$Horsepower, type = "l", col = sColor, main = "GRAPH OF HORSEPOWER" )
    }else if(input$channel1 == "Weight"){
      plot(cars$Weight, type = "l", col = sColor, main = "GRAPH OF WEIGHT" )
    }else if(input$channel1 == "Acceleration"){
      plot(cars$Acceleration, type = "l", col = sColor, main = "GRAPH OF ACCELERATION" )
    }else if(input$channel1 == "Model"){
      plot(cars$Model, type = "l", col = sColor, main = "GRAPH OF MODEL" )
    }
  })
  output$mymap <- renderLeaflet({
    input$update   # catching the action button event
    isolate(
    leaflet()%>%addProviderTiles(input$bmap))
      
    
  })
  output$pieChart <- renderPlot({
    pie3D(table(cars$Origin), labels = paste(names(table(cars$Origin))), explode = 0.5, col =c("red", "green","yellow"), main = "PIE CHART OF COUNTRIES WITH REPECT TO NUMBER OF CARS")
  })
}

shinyApp(ui, server)