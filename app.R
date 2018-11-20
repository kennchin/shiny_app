#note: the extension buttons only work on browser not on app in Rstudio

#load required libraries
library(shiny)                      #libray to use shiny app
library(DT)                         #library to create tables
library(ggplot2)                    #library for plotting
library(reshape2)                   #library for data manipulation
library(stringr)                    #library for string manipulation
library(dplyr)                      #library to rename columns
library(lubridate)                  #library to manipulate date 

#source helper functions
source('./R/get_functions.R')
get_functions()

#choices for protocol
protocol = c(12345,67890)

# Define UI for application that creating table
ui <- fluidPage(
 
    fluidRow(
      column(2,
           selectInput("template", "Choose a template:",
                         choices = c("",unique(as.character(protocol)))),  
           fileInput('datafile', 'Choose CSV file',
                     multiple = TRUE,
                     accept=c('text/csv','text/comma-separated-values,text/plain',".csv")),
          
           # Horizontal line ----
           tags$hr(),
           
           #button to load the report
           actionButton("go","Load")
            ),
      column(10,
           tabsetPanel(id="tabs2",
                       tabPanel("Overall",
                                fluidRow(
                                  column(5,
                                         plotOutput("overall")),
                                  column(6,
                                         plotOutput("overall_prog")),
                                  column(5,
                                         plotOutput("overall_br")),
                                  column(5,
                                         plotOutput("overall_vital"))
                                )
                           ),
                          tabPanel("Monitoring Report", DT::dataTableOutput("moni2")
                                   ),
                          tabPanel("Death within 30 days", DT::dataTableOutput("death2")
                                   ),
                          tabPanel("Demographic Report", DT::dataTableOutput("demo2"),
                                   fluidRow(
                                     column(5,
                                            plotOutput("age_bar2") ),
                                     column(6,
                                            plotOutput("eth_bar2")),
                                     column(5,
                                            plotOutput("gender_bar2")),
                                     column(5,
                                            plotOutput("race_bar2"))
                          ))))
           )
)
    
# Define server logic required to create table
server <- function(input, output) {
  ########################### upload files ##################################
    df <- reactive({
    req(input$datafile)
    
    #read all csv files uploaded
    files= lapply(input$datafile$datapath, read.csv,header=TRUE,stringsAsFactors=F,na.string="")
    
    #add names of element's list to respective supplied CSV files' names
    names(files)<-input$datafile$name
    
    #filter all elements of list by Active==1
    files_active = lapply(files,function(x) x[which(x$RecordActive==1),])

    files_active
  })
  
  #Reactive value for selected template for monitor report
  templateInput <- eventReactive(input$go ,{
    switch(input$template,
           "12345"= Report_12345nodb(df()),
           "67890"= Report_67890nodb(df())
          )
  })
  
  #Reactive value for selected template for death report
  deathInputnodb <- eventReactive(input$go,{
    switch(input$template,
           "12345"= Report_death_12345_nodb(df()),
           "67890"= Report_death_67890_nodb(df())
           )
  })
  
  #Reactive value for selected template for demo report
  demoInputnodb <- eventReactive(input$go,{
    switch(input$template,
           "12345" = Report_demo_12345_nodb(df()),
           "67890" = Report_demo_67890_nodb(df())
           )
    
  })
  
  ############### Plots for overall statistics #################
  output$overall <- renderPlot({
    the_data <- templateInput()
    plot_overall(the_data)
  })
  
  output$overall_prog <- renderPlot({
    the_data <- templateInput()
    plot_overall_prog(the_data)
    
  })
  
  output$overall_br <- renderPlot({
    the_data <-templateInput()
    plot_overall_best(the_data)
  })
  
  output$overall_vital <- renderPlot({
    the_data <-templateInput()
    plot_overall_vital(the_data)
    
  })
  
  ################# Barplots for demographic report #############################
  # barplot for demographic report #
  output$age_bar2 <- renderPlot({
    our_data <-demoInputnodb()
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,1:3,"Age","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,1:3,"Age","Freq")
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<11)
    {
      plot4(our_data,1:3,"Age","Freq")
    }
    else
    {
      plot3(our_data,1:3,"Age","Freq")
    }
    
  })
  
  # barplot for ethnicity #
  output$eth_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    print(our_data)
    print(dim(our_data))
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,12:14,"Ethnicity","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,12:14,"Ethnicity","Freq")
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<11)
    {
      plot4(our_data,12:14,"Ethnicity","Freq")
    }
    else
    {
      plot3(our_data,12:14,"Ethnicity","Freq")
    }
  })
  
  # barplot for Gender #
  output$gender_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,4:5,"Gender","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,4:5,"Gender","Freq")
      
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<11)
    {
      plot4(our_data,4:5,"Gender","Freq")
    }
    else
    {
      plot3(our_data,4:5,"Gender","Freq")
    }
  })
  
  # barplot for Race #
  output$race_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,6:11,"Race","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,6:11,"Race","Freq")
      
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<11)
    {
      plot4(our_data,6:11,"Race","Freq")
    }
    else
    {
      plot3(our_data,6:11,"Race","Freq")
    }
    
  })

  ######################## Rendering the table for monitoring,death, and demographic report ###############################################
  output$moni2 <- DT::renderDataTable({templateInput()},class = 'cell-border stripe',extensions = 'Buttons',options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")) ,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                             "}"),paging=FALSE, escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'A2',filename='file'))
  ))
  output$death2 <- DT::renderDataTable({deathInputnodb()},class = 'cell-border stripe',extensions = 'Buttons',options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                            "}"),escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'LEGAL',filename='file'))
  ))
  
  output$demo2 <- DT::renderDataTable({demoInputnodb()},class = 'cell-border stripe',extensions = 'Buttons',options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                            "}"),escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'LEGAL',filename='file'))
  ))
}

# Run the application 
shinyApp(ui = ui, server = server )

