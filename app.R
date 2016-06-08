# Libraries
library(shiny)
library(shinythemes)
library(shinyURL) # devtools::install_github("aoles/shinyURL")
library(knitr)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(sp)
library(leaflet)
library(RColorBrewer)

# UI --------------------------------------------------------------
ui <-
        navbarPage(
                title = 'Clustering ',
                theme = shinytheme("flatly"),
                
                # tabsetPanel(
                tabPanel('Tool',
                         fluidRow(
                                 column(4,
                                        h3("Upload your data here"),
                                        fileInput('file1', 'A .csv file with columns "id", "lat", "lon"',
                                                  accept=c('text/csv', 
                                                           'text/comma-separated-values,text/plain', 
                                                           '.csv')),
                                        
                                        # radioButtons('sep', 'Separator',
                                        #              c(Comma=',',
                                        #                Semicolon=';',
                                        #                Tab='\t'),
                                        #              ','),
                                        # checkboxInput('header', 'Header', TRUE),
                                        tags$hr(),
                                        textInput('hq_coord',
                                                  'Enter the coordinates of your headquarters',
                                                  value = "-32.87, 24.78"),
                                        sliderInput('size',
                                                  'Select your desired cluster size',
                                                  min=1,
                                                  max=50,
                                                  step=1,
                                                  value = 5),
                                        selectInput("first", "Chose the strategy for the selection of the first point:",
                                                    c("Furthest" = "far",
                                                      "Closest" = "close",
                                                      "Random" = "random")),
                                        selectInput("rest", "Choose the strategy for the selection of subsequent points:",
                                                    c("Furthest" = "far",
                                                      "Closest" = "close",
                                                      "Random" = "random")),
                                       
                                         downloadLink('download_csv', 'Download raw data'),
                                        br(), br(),
                                        
                                        br(), br(),
                                        downloadButton('download_pdf_from_rmd',
                                                       'Download PDF from Rmd'),
                                        br(), br()
                                      
                                 ),
                                 
                                 column(8,
                                        h3('Please, have a look at your points'),
                                        leafletOutput("interactive_map"),
                                        includeMarkdown("includes/include.md"),
                                        # plotOutput('user_plot'),
                                        h3('Your data'),
                                        # dataTableOutput('user_table'),
                                        p('This is a section written using shiny helpers.'),
                                        dataTableOutput("point_table")
                                       
                                 ))),
                tabPanel('About',
                         includeMarkdown('includes/about.md')))


# SERVER ----------------------------------------------------------
server <-
        
        function(input, output){
                
                source("cluster_optimize.R")
                
                # Create a reactive data frame from the user upload 
                df <- reactive({
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        
                        read.csv(inFile$datapath, header=TRUE)
                })
                
                ## Running our cluster algorithm on the uploaded data
                df_out <- reactive({
                        require(sp)
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        raw_data <- read.csv(inFile$datapath, 
                                             header=TRUE)
                        ## Remove coordinates with NA
                        raw_data <- raw_data[!is.na(raw_data$lon) & !is.na(raw_data$lat),]
                        
                        # Converting to spatial points dataframe
                        raw_data$x <- raw_data$lon
                        raw_data$y <- raw_data$lat
                        coordinates(raw_data) <- ~x+y
                        proj4string(raw_data) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
                        y <- cluster_optimize(raw_data, 
                                              cluster_size=input$size,
                                              plot_map=FALSE,
                                              sleep=0,
                                              start=input$first,
                                              rest=input$rest,
                                              messaging=TRUE
                                              )
                        return(y)
                        
                        
                })
                
                output$point_table <- renderDataTable({
                        x <- df_out()
                        x
                })
                
               
                
                output$interactive_map <- renderLeaflet({
                        z <- df_out()
                        if (is.null(z)) {
                                return(NULL)
                        } else {
                                colspal <- brewer.pal(n=9,name="Spectral")
                                colspal <- colorRampPalette(colspal)(length(unique(z$cluster)))
                                colspal <- colspal[sample(1:length(colspal))]
                                cols <- colspal[z$cluster]
                                x <- leaflet() %>%
                                        addProviderTiles("Stamen.TonerLite") %>%
                                        addCircleMarkers(lng=z$lon,
                                                         lat=z$lat,
                                                         popup=paste0(z$id, 
                                                                      " assigned to cluster ", 
                                                                      z$cluster),
                                                         color=cols,
                                                         radius=3,
                                                         opacity=0,
                                                         fillOpacity=1)
                        }
                })
                
               
                output$download_csv <- downloadHandler(
                        filename = paste('data-', Sys.Date(), '.csv', sep=''),
                        content = function(file) {
                                x <- df_out()
                                write.csv(x, file)
                        }
                )
               
                # # Render PDF from Rmd
                # output$download_pdf_from_rmd <- downloadHandler(
                #         filename = function() {
                #                 paste0('Report_.pdf')
                #         },
                #         content = function(file) {
                #                 src <- normalizePath('includes/report.Rmd')
                #                 owd <- setwd(tempdir())
                #                 on.exit(setwd(owd))
                #                 file.copy(src, 'report.Rmd', overwrite = TRUE)
                #                 library(rmarkdown)
                #                 out <- render('report.Rmd',pdf_document())
                #                 file.rename(out, file)
                #         }
                # )

        }


# Run the app
shinyApp(ui = ui, server = server)