
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

data_sets <- c("mtcars", "morley", "rock","iris","Data from csv file")

shinyServer(function(input, output) {
        
        # Drop-down selection box for which data set
        output$choose_dataset <- renderUI({
                selectInput("dataset", "Data set", as.list(data_sets))
        })
       
        output$choose_Y <- renderUI({
                # If missing input, return to avoid error later in function
                if(is.null(input$dataset))
                        return()
                
                # Get the data set with the appropriate name
                inFile <- input$file1
                
                 if (input$dataset=="Data from csv file"){
                         if (is.null(inFile))
                                 return(NULL)
                     dat<-read.csv(inFile$datapath, header = input$header)
                 }else{
                dat <- get(input$dataset)}
                colnames <- names(dat)
                
                # Create the checkboxes and select them all by default
                selectInput("Y", "Choose the Y:", 
                            choices  = colnames,
                            selected = colnames)
                
        })
         
        # Check boxes
        output$choose_X <- renderUI({
                # If missing input, return to avoid error later in function
                if(is.null(input$dataset))
                        return()
                
                # Get the data set with the appropriate name
                inFile <- input$file1
                
                if (input$dataset=="Data from csv file"){
                        if (is.null(inFile))
                                return(NULL)
                        dat<-read.csv(inFile$datapath, header = input$header)
                  }else{
                        dat <- get(input$dataset)}
                colnames <- names(dat)
                
                # Create the checkboxes and select them all by default
                selectInput("X", "Choose the predictor X:", 
                                   choices  = colnames,
                                   selected = colnames)
              
                
        })
        
        
        # Chart presentation
        # Y vs X
        # if X numeric => linear regression 
        # if X is not numeric => box plot
        
        output$plot_reg <- renderPlot({
                # If missing input, return to avoid error later in function
                if(is.null(input$dataset))
                        return()
                
                # Get the data set
                inFile <- input$file1
                
                if (input$dataset=="Data from csv file"){
                        if (is.null(inFile))
                                return(NULL)
                         dat<-read.csv(inFile$datapath, header = input$header)
                 }else{
                        dat <- get(input$dataset)}
                colnames <- names(dat)
                
                # Make sure columns are correct for data set (when data set changes, the
                # columns will initially be for the previous data set)
                if (is.null(input$Y) || !(input$Y %in% names(dat)))
                        return()
                
                if (is.null(input$X) || !(input$X %in% names(dat)))
                        return()
                
                
                # Keep the selected columns
                datX <- dat[, input$X]
                datX<-as.data.frame(datX)
                colnames(datX)<-input$X
                
                datY <- dat[, input$Y]
                datY<-as.data.frame(datY)
                colnames(datY)<-input$Y
                data<-cbind(datX,datY)
                if (is.numeric(data[,1])== TRUE){
                        g<-ggplot(data=data,aes(data[,1],data[,2]))
                        g<-g+geom_point(size=2,alpha=0.2)+geom_smooth(method="lm")+ labs(x=input$X,y=input$Y)
                        g<-g+theme(axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),axis.text=element_text(size=12))
                }else{
                        g<-ggplot(data=data,aes(data[,1],data[,2]))
                        g<-g+geom_boxplot()+ labs(x=input$X,y=input$Y)
                        g<-g+theme(axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),axis.text=element_text(size=12))
                }
                g
        })
        
        # Output the data
        
        output$results_reg <- renderPrint({
                # If missing input, return to avoid error later in function
                if(is.null(input$dataset))
                        return()
                
                # Get the data set
                inFile <- input$file1
                
                if (input$dataset=="Data from csv file"){
                        if (is.null(inFile))
                                return(NULL)
                        dat<-read.csv(inFile$datapath, header = input$header)
                }else{
                        dat <- get(input$dataset)}
                colnames <- names(dat)
                
                # Make sure columns are correct for data set (when data set changes, the
                # columns will initially be for the previous data set)
                if (is.null(input$Y) || !(input$Y %in% names(dat)))
                        return()
                
                if (is.null(input$X) || !(input$X %in% names(dat)))
                        return()
                
                
                # Keep the selected columns
                datX <- dat[, input$X]
                datX<-as.data.frame(datX)
                colnames(datX)<-input$X
                
                datY <- dat[, input$Y]
                datY<-as.data.frame(datY)
                colnames(datY)<-input$Y
                data<-cbind(datX,datY)
               # data
                 #summary(lm(data[,2]~data[,1]))
               summary(lm(as.formula(paste(input$Y,"~",input$X)),data=data))
               
        })
        
       
        
        
})