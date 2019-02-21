ui <- fluidPage(
  fluidRow(
    downloadButton(outputId = "template_download", label = "Download template"),
    fileInput('data_input', 'Choose file to upload', accept = '.csv')
  ),
  dataTableOutput(outputId = "data_upload"),
  fluidRow(
    column(4, selectInput(inputId = "kp", label="Choose a key population", choices="FSW")),
    column(4, selectInput(inputId = "year", label = "Choose one or more survey years", choices=c(2013, 2018), multiple=TRUE, selected=2018)),
    column(4, selectInput(inputId = "subnat", label = "Choose a region", choices=c("list", "of", "subnational", "regions", "here")))
  ),
  selectInput(inputId = "cascade", label = "Choose cascade type", choices=c("90-90-90", "90-81-72")),
  tabsetPanel(              
    tabPanel(title = "Plots",
             plotOutput("cascade_plots")
    ),
    tabPanel(title = "Data",
             column(8, dataTableOutput("cascade_table"))
             
    )
  )
)