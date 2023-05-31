library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)


setwd("D:/rworks")
load("przejazdy.Rdata")
przejazdy$Data <- as.Date(przejazdy$Data, format = "%Y-%m-%d")

ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Strona Glowna", tabName = "homepage", icon = icon("home")),
      menuItem("zadanie 1", tabName = "zad1", icon = icon("bar-chart-o")),
      menuItem("zadanie 2", tabName = "zad2", icon = icon("table")),
      menuItem("zadanie 3", tabName = "zad3", icon = icon("line-chart")),
      menuItem("zadanie 4", tabName = "zad4", icon = icon("area-chart"))
      #menuItem("zadanie 5", tabName = "zad5", icon = icon("dashboard")),
      #menuItem("zadanie 6", tabName = "zad6", icon = icon("pie-chart")),
      #menuItem("zadanie 7", tabName = "zad7", icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "homepage",
              fluidRow(
                box(width = 12, textOutput("homeText"))
              )),
      tabItem(tabName = "zad1",
              fluidRow(
                box(width = 12, textOutput("zad1Text"), plotOutput("zad1Plot"))
              )),
      tabItem(tabName = "zad2",
              fluidRow(
                box(width = 6, textOutput("zad2Text"), selectInput("stacja2", "Select Stacja:", choices = unique(data$Stacja)), dateRangeInput("dateRange2", "Select Date Range:", min = min(data$Data), max = max(data$Data))),
                box(width = 6, plotOutput("zad2Plot1"), plotOutput("zad2Plot2"))
              )),
      tabItem(tabName = "zad3",
              fluidRow(
                box(width = 6, textOutput("zad3Text"), selectInput("stacja3", "Select Stacja:", choices = unique(data$Stacja), multiple = TRUE), dateRangeInput("dateRange3", "Select Date Range:", min = min(data$Data), max = max(data$Data))),
                box(width = 6, plotOutput("zad3Plot1"), plotOutput("zad3Plot2"))
              )),
      tabItem(tabName = "zad4",
              fluidRow(
                box(width = 6, textOutput("zad4Text"), selectInput("stacja4", "Select Stacja:", choices = unique(data$Stacja)), dateRangeInput("dateRange4", "Select Date Range:", min = min(data$Data), max = max(data$Data))),
                box(width = 6, plotOutput("zad4Plot1"), plotOutput("zad4Plot2"), plotOutput("zad4Plot3"), plotOutput("zad4Plot4"), plotOutput("zad4Plot5"), plotOutput("zad4Plot6"))
              ))
      # Here are the placeholders for "zadanie 5", "zadanie 6", and "zadanie 7". Complete these accordingly.
    )
  )
)

server <- function(input, output) {
  output$homeText <- renderText({
    paste("XDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD", "XDDDDDDDDDDDD", sep = "\n")
  })
  
  output$zad1Text <- renderText({
    "Przedstawić rozkład liczby dni pomiarowych w poszczególnych punktach"
  })
  
  output$zad1Plot <- renderPlot({
    data %>% 
      group_by(Stacja) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = Stacja, y = n)) + 
      geom_col() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$zad2Text <- renderText({
    "Dla wybranego punktu przedstawić rozkład liczby przejazdów"
  })
  
  output$zad2Plot1 <- renderPlot({
    req(input$stacja2)
    data_subset <- data %>%
      filter(Stacja == input$stacja2, Data >= input$dateRange2[1], Data <= input$dateRange2[2])
    ggplot(data_subset, aes(x = Licznik)) + 
      geom_boxplot()
  })
  
  output$zad2Plot2 <- renderPlot({
    req(input$stacja2)
    data_subset <- data %>%
      filter(Stacja == input$stacja2, Data >= input$dateRange2[1], Data <= input$dateRange2[2])
    ggplot(data_subset, aes(x = Data, y = Licznik)) + 
      geom_line()
  })
  
  output$zad3Text <- renderText({
    "Porównać punkty pod względem natężenie/rozkładu przejazdów"
  })
  
  output$zad3Plot1 <- renderPlot({
    req(input$stacja3)
    data_subset <- data %>%
      filter(Stacja %in% input$stacja3, Data >= input$dateRange3[1], Data <= input$dateRange3[2])
    ggplot(data_subset, aes(x = Stacja, y = Licznik)) + 
      geom_boxplot()
  })
  
  output$zad3Plot2 <- renderPlot({
    req(input$stacja3)
    data_subset <- data %>%
      filter(Stacja %in% input$stacja3, Data >= input$dateRange3[1], Data <= input$dateRange3[2])
    ggplot(data_subset, aes(x = Data, y = Licznik, color = Stacja)) + 
      geom_line()
  })
  
  output$zad4Text <- renderText({
    "Przedstawić natężenie / rozkład przejazdów dla wybranej stacji w zależności od miesiąca / dnia tygodnia / dni powszednich/weekendowych"
  })
  
  output$zad4Plot1 <- renderPlot({
    req(input$stacja4)
    data_subset <- data %>%
      filter(Stacja == input$stacja4, Data >= input$dateRange4[1], Data <= input$dateRange4[2])
    data_subset$weekday <- wday(data_subset$Data, label = TRUE)
    ggplot(data_subset, aes(x = weekday, y = Licznik)) + 
      geom_boxplot()
  })
  
  output$zad4Plot2 <- renderPlot({
    req(input$stacja4)
    data_subset <- data %>%
      filter(Stacja == input$stacja4, Data >= input$dateRange4[1], Data <= input$dateRange4[2])
    data_subset$weekday <- wday(data_subset$Data, label = TRUE)
    ggplot(data_subset, aes(x = Data, y = Licznik, color = weekday)) + 
      geom_line()
  })
  # Similar plots for the rest of "zadanie 4"
}

shinyApp(ui, server)
