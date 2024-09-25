
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
library(mapproj)

load("GradProject.RData")



ui <- fluidPage(
  titlePanel("Fintech and Financial Inclusion Gender Gap in Sub-Saharan Africa"),
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.tabs == 'Line Plots'",
      selectInput("plot_type", "Choose a line plot:", 
                  choices = list("Account Ownership" = "acc", 
                                 "Savings" = "sav", 
                                 "Loan Accessibility" = "loan", 
                                 "Employment Rate" = "emp", 
                                 "GDP" = "gdp")),
      sliderInput("year_range", "Select Year Range:", 
                  min = 2011, max = 2021, value = c(2011, 2021), sep = "")),
      
      conditionalPanel(
        condition = "input.tabs == 'Chloropleth Maps'",
      selectInput("map_type", "Choose a map:", 
                  choices = list("Account Gender Gap" = "account", 
                                 "Savings Gender Gap" = "savings", 
                                 "Loan Gender Gap" = "loan",
                                 "Fintech Development" = "Fint_Dev",
                                 "Employment Gender Gap" = "Empt"))),
      
      conditionalPanel(
        condition = "input.tabs == 'Scatter Plots'",
      selectInput("scat_type", "Choose a scatter plot:", 
                    choices = list("Fintech vs Account G.Gap" = "scat1", 
                                   "Fintech vs Savings G.Gap" = "scat2", 
                                   "Fintech vs Loan GGap" = "scat3"))  
      ),
      
      # Add download button for RData file
      conditionalPanel(
        condition = "input.tabs == 'Data'",
      downloadButton("downloadData", "Download RData File"))
    ),
    
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("Introduction",
                           h2("Welcome to the FIGG Visualization App!"), 
                           p("These plots explore Financial inclusion gender gap in the Sub-Saharan African region.
                           26 Countries were examined due to availability of data for the period 2011 - 2021."),
                           p("Use the tabs above to navigate through different plots and analyses."),
                           br(),
                           strong("Data Sources"),
                           p(
                             "1.", 
                             tags$a(href="https://databank.worldbank.org/source/world-development-indicators",
                                    "World Development Indicators")),
                           p(
                             "2.", 
                             tags$a(href="https://www.fraserinstitute.org/economic-freedom/dataset?geozone=world&page=dataset&min-year=2&max-year=0&filter=0",
                                    "fraserinstitute Economic Freedom Data")),
                           p(
                             "3.", 
                             tags$a(href="https://www.heritage.org/index/pages/all-country-scores",
                                    "Heritage Economic Freedom Data")),
                           p(
                             "4.", 
                             tags$a(href="https://www.worldbank.org/en/publication/globalfindex",
                                    "Findex Data"))
                             
                           ),
                           uiOutput("definition_introduction"),
                  
                  tabPanel("Line Plots", plotOutput("linePlot")),
                  tabPanel("Chloropleth Maps", plotOutput("mapPlot")),
                  tabPanel("Scatter Plots", plotOutput("ScatterPlot")),
                  tabPanel("Data", tableOutput("dataView"))
        
      )
    )
  )
)


server <- function(input, output) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "GradProject.RData"  # Name of the RData file that the user will download
    },
    content = function(file) {
      file.copy("GradProject.RData", file)  # Path to the RData file
    }
  )
  
  output$linePlot <- renderPlot({
    data <- Yearly_FIGG %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    if (input$plot_type == "acc") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Acc, color = "avg_F_Acc")) +
        geom_line(aes(x = Year, y = avg_M_Acc, color = "avg_M_Acc")) +
        labs(x = "Year", y = "", color = "Average Account Ownership", 
             title = "Account Ownership by Gender (Ages 15+)", 
             caption = "Proportion of males and females aged 15+ who own account at a formal financial institution.") +
        scale_color_manual(values = c("avg_M_Acc" = "blue", "avg_F_Acc" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 40)
    } 
    
    else if (input$plot_type == "sav") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Save, color = "avg_F_Save")) +
        geom_line(aes(x = Year, y = avg_M_Save, color = "avg_M_Save")) +
        labs(x = "Year", y = "", color = "Average Savings", 
             title = "Savings by Gender (Ages 15+)", 
             caption = "Proportion of males and females aged 15+ who saved some money at least in the past year at a formal financial institution.") +
        scale_color_manual(values = c("avg_M_Save" = "blue", "avg_F_Save" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 20)
    } 
    
    else if (input$plot_type == "loan") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Loan, color = "avg_F_Loan")) +
        geom_line(aes(x = Year, y = avg_M_Loan, color = "avg_M_Loan")) +
        labs(x = "Year", y = "", color = "Average Loan accessibility", 
             title = "Loan Accessibility by Gender (Ages 15+)", 
             caption = "Proportion of males and females aged 15+ who accessed loan at a formal financial institution at least in the past year.") +
        scale_color_manual(values = c("avg_M_Loan" = "blue", "avg_F_Loan" = "pink"), labels = c("female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 12)
    } 
    
    else if (input$plot_type == "emp") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_F_Emp, color = "avg_F_Emp")) +
        geom_line(aes(x = Year, y = avg_M_Emp, color = "avg_M_Emp")) +
        labs(x = "Year", y = "", title = "Employment Rate by Gender (Ages 15+)", 
             caption = "Proportion of males and females that were engaged in an economic activity for income (2011-2021).", 
             color = "Average Employment rate") +
        scale_color_manual(values = c("avg_M_Emp" = "blue", "avg_F_Emp" = "pink"), labels = c("Female", "male")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.text = element_text(size = 10), legend.title = element_text(size = 10), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, 80)
    } 
    
    else if (input$plot_type == "gdp") {
      ggplot(data) +
        geom_line(aes(x = Year, y = avg_GDP, color = "avg_GDP")) +
        geom_line(aes(x = Year, y = med_GDP, color = "med_GDP")) +
        labs(x = "Year", y = "GDP", title = "Average Yearly GDP in SSA (2011 - 2021)", 
             caption = "Trend of average GDP in the SSA Region", 
             color = "GDP in US$") +
        scale_color_manual(values = c("avg_GDP" = "darkgreen", "med_GDP" = "darkred"), labels = c("Mean", "Median")) +
        theme_classic() +
        theme(legend.position = "right", legend.key.size = unit(0.5, "cm"), 
              legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
              legend.direction = "vertical", plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(hjust = 0.5)) +
        ylim(0, NA)
    }
  })
  
  
  output$mapPlot <- renderPlot({
    if (input$map_type == "account") {
      ggplot() + 
        geom_polygon(aes(x=long, y=lat, 
                         group=group, 
                         fill = Acc_GGap), 
                     color = "black", # Add border color
                     size = 0.5,
                     data=FIGG_SSA)+
        labs(title = "Account Gender Gap",
             caption = "Average gender gap in relation to account ownership in SSA Countries (2011-2021).",
             fill="Account ownership Gender gap")+
        theme_minimal()+
        theme(
          legend.position = "right",
          legend.key.size = unit(0.9, "cm"),
          legend.title = element_text(size = 10),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()
        )+
        coord_map(projection = "mercator")+
        scale_fill_gradient(low = "white", high = "blue")+
        geom_text(aes(x=mid.long,y=mid.lat,
                      label=paste0(Ctry_code)),
                  size=2,
                  fontface="bold",
                  color="black",
                  data=My_labels)
    } 
    
    else if (input$map_type == "savings") {
      ggplot() + 
        geom_polygon(aes(x=long, y=lat, 
                         group=group, 
                         fill = Sav_GGap), 
                     color = "black", # Add border color
                     size = 0.5,
                     data=FIGG_SSA)+
        labs(title = "Savings Gender Gap",
             caption = "Average gender gap in savings, at least for the past year at a formal financial institution in SSA (2011-2021).", 
             fill="Savings Gender gap")+
        theme_minimal()+
        theme(
          legend.position = "right",
          legend.key.size = unit(0.9, "cm"),
          legend.title = element_text(size = 10),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()
        )+
        coord_map(projection = "mercator")+
        scale_fill_gradient(low = "white", high = "darkgreen")+
        geom_text(aes(x=mid.long,y=mid.lat,
                      label=paste0(Ctry_code)),
                  size=2,
                  fontface="bold",
                  color="black",
                  data=My_labels)
    } 
    
    else if (input$map_type == "loan") {
      ggplot() + 
        geom_polygon(aes(x=long, y=lat, 
                         group=group, 
                         fill = Loan_GGap), 
                     color = "black", # Add border color
                     size = 0.5,
                     data=FIGG_SSA)+
        labs(title = "Loan Gender Gap",
             caption = "Average gender gap in terms of loan accessibility in SSA Countries (2011-2021)",
             fill= "Loan Gender Gap")+
        theme_minimal()+
        theme(
          legend.position = "right",
          legend.key.size = unit(0.9, "cm"),
          legend.title = element_text(size = 10),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()
        )+
        coord_map(projection = "mercator")+
        scale_fill_gradient(low = "white", high = "red")+
        geom_text(aes(x=mid.long,y=mid.lat,
                      label=paste0(Ctry_code)),
                  size=2,
                  fontface="bold",
                  color="black",
                  data=My_labels)
    }
    
    else if (input$map_type == "Empt"){
      ggplot() + 
        geom_polygon(aes(x=long, y=lat, 
                         group=group, 
                         fill = avg_Emp_GGap), 
                     color = "black", # Add border color
                     size = 0.5,
                     data=FIGG_SSA)+
        labs(title = "Employment Gender Gap",
             caption = "Gender Gap in terms employment rate in SSA Countries (2011-2021).",
             fill="employment gender gap")+
        theme_minimal()+
        theme(
          legend.position = "right",
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_text(size = 7),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()
        )+
        coord_map(projection = "mercator")+
        scale_fill_gradient(low = "white", high = "darkred")+
        geom_text(aes(x=mid.long,y=mid.lat,
                      label=paste0(Ctry_code)),
                  size=2,
                  fontface="bold",
                  color="black",
                  data=My_labels)
    }
    
    else if (input$map_type == "Fint_Dev"){
      ggplot() + 
        geom_polygon(aes(x=long, y=lat, 
                         group=group, 
                         fill = avg_fint),
                     color = "black", # Add border color
                     size = 0.5,
                     data=FIGG_SSA)+
        labs(title = "Fintech development",
             caption = "Fintech Development in SSA Countries",
             fill=" Fintech Development")+
        theme_minimal()+
        theme(
          legend.position = "right",
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_text(size = 7),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()
        )+
        coord_map(projection = "mercator")+
        scale_fill_gradient(low = "white", high = "darkorchid")+
        geom_text(aes(x=mid.long,y=mid.lat,
                      label=paste0(Ctry_code)),
                  size=2,
                  fontface="bold",
                  color="black",
                  data=My_labels)
    }
  }, width = 600, height = 800)
  
  
  output$ScatterPlot <- renderPlot({
    if (input$scat_type == "scat1"){
    ggplot(data = Avg_FIGG) +
      geom_point(aes(x = avg_fint, y = Acc_GGap), color = "red", shape=16) +
      geom_smooth(aes(x = avg_fint, y = Acc_GGap), color = "blue", method="lm", se=TRUE)+
      labs(x = "Fintech Development", 
           y = "Account Ownership G-Gap", 
           title = "Fintech Development Vrs Account Ownership Gender Gap",
           caption = "Scatter plot showing the relationship between Fintech \n development and gender gap in account ownership in SSA.") +
      theme_classic() +
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5),
            plot.caption = element_text(hjust = 0.5))
    }
      
    else if (input$scat_type == "scat2"){
    ggplot(data = Avg_FIGG) +
      geom_point(aes(x = avg_fint, y = Sav_GGap), color = "blue", shape=17) +
      geom_smooth(aes(x = avg_fint, y = Sav_GGap), color = "green", method="lm", se=TRUE)+
      labs(x = "Fintech Development",
           y = "G-Gap in Savings",
           title = "Fintech Development Vrs Savings Gender Gap",
           caption = "Scatter plot showing the relationship between Fintech development and \n gender gap in ability to save at a formal finacial institution in SSA.") +
      theme_classic() +
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5),
            plot.caption = element_text(hjust = 0.5))+
      ylim(0, NA)
    }
    
    else if (input$scat_type == "scat3"){
    ggplot(data = Avg_FIGG) +
      geom_point(aes(x = avg_fint, y = Loan_GGap) , color = "green", shape=15) +
      geom_smooth(aes(x = avg_fint, y = Loan_GGap), color = "red", method="lm", se=TRUE)+
      labs(x = "Fintech Development",
           y = "Loan Accessibility G-Gap",
           title = "Fintech Development Vrs Gender Gap in access to Loan",
           caption = "Scatter plot showing the relationship between Fintech development and \n gender gap in accessibility of loan from a formal financial institution in SSA.") +
      theme_classic() +
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5),
            plot.caption = element_text(hjust=0.5))+
      ylim(0, NA)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

