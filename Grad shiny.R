
library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(maps)
library(gridExtra)
library(GGally)
library(ggthemes)
library(shiny)

load("GradProject.RData")


# Define UI
ui <- fluidPage(
  titlePanel("Financial and Employment Trends by Gender"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choose a plot:", 
                  choices = list("Account Ownership" = "acc", 
                                 "Savings" = "sav", 
                                 "Loan Accessibility" = "loan", 
                                 "Employment Rate" = "emp", 
                                 "GDP" = "gdp")),
      sliderInput("year_range", "Select Year Range:", 
                  min = 2011, max = 2021, value = c(2011, 2021), sep = "")
    ),
    
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$linePlot <- renderPlot({
    data <- Yearly_FIGG %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    if (input$plot_type == "acc") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Acc, color = "avg_F_Acc")) +
        geom_line(aes(x = Year, y = avg_M_Acc, color = "avg_M_Acc")) +
        labs(x = "Year", y = "", color = "Average Account Ownership", 
             title = "Account Ownership by Gender (Ages 15+)", 
             caption = "Fig.1: Proportion of males and females aged 15+ who own account at a formal financial institution. \n Source: Author based on data from the Global Findex Database, 2021") +
        scale_color_manual(values = c("avg_M_Acc" = "blue", "avg_F_Acc" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 7), legend.text = element_text(size = 7), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 40)
    } else if (input$plot_type == "sav") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Save, color = "avg_F_Save")) +
        geom_line(aes(x = Year, y = avg_M_Save, color = "avg_M_Save")) +
        labs(x = "Year", y = "", color = "Average Savings", 
             title = "Savings by Gender (Ages 15+)", 
             caption = "Fig.2: Proportion of males and females aged 15+ who saved some money at least in the past year at a formal financial institution. \n Source: Author based on data from the Global Findex Database, 2021") +
        scale_color_manual(values = c("avg_M_Save" = "blue", "avg_F_Save" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 7), legend.text = element_text(size = 7), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 20)
    } else if (input$plot_type == "loan") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Loan, color = "avg_F_Loan")) +
        geom_line(aes(x = Year, y = avg_M_Loan, color = "avg_M_Loan")) +
        labs(x = "Year", y = "", color = "Average Loan accessibility", 
             title = "Loan Accessibility by Gender (Ages 15+)", 
             caption = "Fig.3: Proportion of males and females aged 15+ who accessed loan at a formal financial institution at least in the past year. \n Source: Author based on data from the Global Findex Database, 2021") +
        scale_color_manual(values = c("avg_M_Loan" = "blue", "avg_F_Loan" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 7), legend.text = element_text(size = 7), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 12)
    } else if (input$plot_type == "emp") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Emp, color = "avg_F_Emp")) +
        geom_line(aes(x = Year, y = avg_M_Emp, color = "avg_M_Emp")) +
        labs(x = "Year", y = "", title = "Employment Rate by Gender (Ages 15+)", 
             caption = "Fig.4: Proportion of males and females that were engaged in an economic activity for income (2011-2021). \n Source: Author based on data from World Bank World Development Indicators, 2023", 
             color = "Average Employment rate") +
        scale_color_manual(values = c("avg_M_Emp" = "blue", "avg_F_Emp" = "pink"), labels = c("Female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.text = element_text(size = 7), legend.title = element_text(size = 7), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 80)
    } else if (input$plot_type == "gdp") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_GDP, color = "avg_GDP")) +
        geom_line(aes(x = Year, y = med_GDP, color = "med_GDP")) +
        labs(x = "Year", y = "GDP", title = "Average Yearly GDP in SSA (2011 - 2021)", 
             caption = "Fig.5 \n Source: Author based on data from the World Bank World Development Indicators, 2023", 
             color = "GDP in US$") +
        scale_color_manual(values = c("avg_GDP" = "darkgreen", "med_GDP" = "darkred"), labels = c("Mean", "Median")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 7), legend.text = element_text(size = 7), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, NA)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
