# Libraries
library(shiny)
library(shinythemes)
library(shinyURL) # devtools::install_github("aoles/shinyURL")
library(knitr)
library(rmarkdown)
library(dplyr)
library(ggplot2)


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
                                        
                                        radioButtons('sep', 'Separator',
                                                     c(Comma=',',
                                                       Semicolon=';',
                                                       Tab='\t'),
                                                     ','),
                                        # checkboxInput('header', 'Header', TRUE),
                                        tags$hr(),
                                        textInput('size',
                                                  'Enter your desire cluster size',
                                                  value = "size"),
                                        selectInput("first", "Chose the strategy for the selection of the first point:",
                                                    c("Furthest" = "far",
                                                      "Closest" = "cloase",
                                                      "Random" = "random")),
                                        selectInput("subsequents", "Choose the strategy for the selection of subsequent points:",
                                                    c("Furthest" = "far",
                                                      "Closest" = "cloase",
                                                      "Random" = "random")),
                                       
                                        radioButtons('quote', 'Quote',
                                                     c(None='',
                                                       'Double Quote'='"',
                                                       'Single Quote'="'"),
                                                     '"'),
                                        h5('Format of the dataset "fake_data":'),
                                        tableOutput('fake_data_head'),
                                        textInput('plot_code',
                                                  'Write some code for a plot',
                                                  value = "print(ggplot(data = fake_data, aes(x = b, y = c)) + geom_point() + geom_line())"),
                                        textInput('table_code',
                                                  'Write some code for manipulating the raw data:',
                                                  value = "fake_data %>% filter(d == 'y')"),
                                        downloadLink('download_csv', 'Download raw data'),
                                        br(), br(),
                                        
                                        br(), br(),
                                        downloadButton('download_pdf_from_rmd',
                                                       'Download PDF from Rmd'),
                                        br(), br(),
                                        
                                        # URL given current choices:
                                        shinyURL.ui(label = 'Share URL of current app state',
                                                    tinyURL = FALSE)
                                 ),
                                 
                                 column(8,
                                        h3('Please, have a look at your points'),
                                        plotOutput("point_plot"),
                                        includeMarkdown("includes/include.md"),
                                        plotOutput('user_plot'),
                                        h3('Your data'),
                                        dataTableOutput('user_table'),
                                        p('This is a section written using shiny helpers.'),
                                        tableOutput("point_table")
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
                        
                        read.csv(inFile$datapath, header=TRUE,
                        sep=input$sep,
                        quote=input$quote)
                })
                
                ## Running our cluster algorithm on the uploaded data
                df_out <- reactive({
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, 
                                 quote=input$quote)
                        # Converting to spatial points dataframe
                        raw_data$x <- raw_data$lon
                        raw_data$y <- raw_data$lat
                        coordinates(raw_data) <- ~x+y
                        proj4string(raw_data) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
                        y <- cluster_optimize(raw_data, 
                                              cluster_size=2,
                                              plot_map=FALSE,
                                              sleep=0,
                                              start="far",
                                              rest="close",
                                              messaging=TRUE
                                              )
                        return(y)
                        
                        
                })
                
                output$point_table <- renderTable({
                        x <- df_out()
                        x
                })
                
                ## Create a simple plot of the points
                output$point_plot<- renderPlot({
                        inFile <- input$file1
                        if(is.null(inFile)) {
                                ggplot()     
                        } else {
                                my_points <- df()
                                ggplot(my_points)+geom_point(aes(lon, lat), color="red")
                        }
                })
                
                
                
                # Create fake data
                fake_data <- data.frame(a = letters,
                                        b = 1:26,
                                        c = rnorm(n = 26))
                fake_data$d <- sample(fake_data$a[24:26], 
                                      size = 26,
                                      replace = TRUE)
                
                # Get a reactive object for the input
                # Make a downloadble csv
                dataset_input <- reactive({
                        eval(parse(text = input$table_code))
                })
                
                # Make a table of the head of fake_data
                output$user_table <- renderDataTable({
                        x <- dataset_input()
                        x
                })
                
                
                
                output$download_csv <- downloadHandler(
                        filename = paste('data-', Sys.Date(), '.csv', sep=''),
                        content = function(file) {
                                x <- dataset_input()
                                write.csv(x, file)
                        }
                )
                
                # Make a table of the head of fake_data
                output$fake_data_head <- renderTable({
                        head(fake_data, 3)
                })
                
                # Make a plot from the user-supplied code
                output$user_plot <- 
                        renderPlot({
                                eval(parse(text = input$plot_code))
                                title(main = 'This is the plot created by your code')
                        })
                
  
                # Render PDF from Rmd
                output$download_pdf_from_rmd <- downloadHandler(
                        filename = function() {
                                paste0('Report_.pdf')
                        },
                        content = function(file) {
                                src <- normalizePath('includes/report.Rmd')
                                owd <- setwd(tempdir())
                                on.exit(setwd(owd))
                                file.copy(src, 'report.Rmd', overwrite = TRUE)
                                library(rmarkdown)
                                out <- render('report.Rmd',pdf_document())
                                file.rename(out, file)
                        }
                )
                
                
                # Capture current state of app
                shinyURL.server()
        }


# Run the app
shinyApp(ui = ui, server = server)