
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(pageWithSidebar(
        headerPanel(""),
        sidebarPanel(
                a("Select the data frame in the Data set list. If you import data from a CSV file, select before the file and then select *Data from csv file* in the data set list. Finally select the Y and X whose correlation you want to analyse."),
                
                fileInput("file1", "Choose CSV File",
                                   accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                ),
                checkboxInput("header", "Header", TRUE),
                uiOutput("choose_dataset"),
                uiOutput("choose_Y"),
                uiOutput("choose_X"),
                a(h4("User manual", class = "btn btn-default action-button" , 
                     style = "fontweight:600"), target = "_blank",
                  href ="http://rpubs.com/ThierryFauret/283714")
              
        ),
        
        
        mainPanel(
                h4("Graphic representation:"),
                plotOutput("plot_reg"),
                h4("Regression results:"),
                verbatimTextOutput("results_reg"),
                tableOutput("contents")
                
                
        )
))