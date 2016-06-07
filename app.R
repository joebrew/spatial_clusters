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
                                        fileInput('file1', 'Choose CSV File',
                                                  accept=c('text/csv', 
                                                           'text/comma-separated-values,text/plain', 
                                                           '.csv')),
                                        tags$hr(),
                                        checkboxInput('header', 'Header', TRUE),
                                        radioButtons('sep', 'Separator',
                                                     c(Comma=',',
                                                       Semicolon=';',
                                                       Tab='\t'),
                                                     ','),
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
                                        
                                        downloadButton("download_pdf_from_rnw",
                                                       "Download PDF from Rnw"),
                                        br(), br(),
                                        downloadButton('download_pdf_from_rmd',
                                                       'Download PDF from Rmd'),
                                        br(), br(),
                                        
                                        # URL given current choices:
                                        shinyURL.ui(label = 'Share URL of current app state',
                                                    tinyURL = FALSE)
                                 ),
                                 
                                 column(8,
                                        h3('Your plot'),
                                        includeMarkdown("includes/include.md"),
                                        plotOutput('user_plot'),
                                        h3('Your data'),
                                        dataTableOutput('user_table'),
                                        p('This is a section written using shiny helpers.')
                                 ))),
                tabPanel('About',
                         includeMarkdown('includes/about.md')))


# SERVER ----------------------------------------------------------
server <-
        
        function(input, output){
                
                ### csv upload
                output$contents <- renderTable({
                        
                        # input$file1 will be NULL initially. After the user selects
                        # and uploads a file, it will be a data frame with 'name',
                        # 'size', 'type', and 'datapath' columns. The 'datapath'
                        # column will contain the local filenames where the data can
                        # be found.
                        
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        
                        read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                                 quote=input$quote)
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
                
                # Render PDF from Rnw
                output$download_pdf_from_rnw <-
                        downloadHandler(filename = "report_rnw.pdf",
                                        content = function(file){
                                                src <- normalizePath('includes/report_rnw.Rnw')
                                                owd <- setwd(tempdir())
                                                on.exit(setwd(owd))
                                                file.copy(src, 'report_rnw.Rnw')
                                                
                                                library(rmarkdown)
                                                knit2pdf('report_rnw.Rnw')
                                                
                                                # copy pdf to 'file'
                                                file.copy("report_rnw.pdf", file, overwrite = TRUE)
                                                
                                                # # delete generated files
                                                file.remove("report_rnw.pdf", "report_rnw.tex",
                                                            "report_rnw.aux", "report_rnw.log")
                                                
                                                # delete folder with plots
                                                unlink("includes/figure", recursive = TRUE)
                                        },
                                        contentType = "application/pdf"
                        )
                
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