upload_data <- function() {
  
  div(
    fluidRow(
      HTML("
        <h2 style='margin-left:10%'>Upload data</h3>
        <br>
        <div style='font-size:16px; margin-left: 10%; margin-right:10%'>
          <p><b>KPCascade</b> has data preloaded from locations for FSW, MSM and PWID from several years - displayed below. If you would like to upload additional data, please download the default data below as a template and then upload the amended file.<p>
          <p>Please ensure that:</p>
            <ul>
              <li>Prevalence and treatment cascade data are entered as <b>decimals</b>, not percentages</li>
              <li>Treatment cascade data are of the form <b>90-90-90</b>, not 90-81-73</li>
              <li>Row names for the cascade status are <b>exact matches</b> to those given in the template</li>
              <li>There is only a single value for each unique combination of identification variables (e.g. No more than 1 size estimate for MSM in Pretoria in 2018)</li>
            </ul>
          </div>
          <br>
      ")
    ),
    fluidRow(
      column(3, offset = 1, downloadButton(outputId = "template_download", label = "Download default data")),
      column(3, fileInput('data_input', 'Upload data', accept = '.csv')),
      column(3, offset = 1 ,actionButton("resetToExampleData", label = "Reset to default data"))
    ),
    tabsetPanel(
      tabPanel(title = HTML("<b style='font-size:18px'>Size estimates</b>"),
          br(),
          fluidRow(
            column(10, offset=1, DTOutput(outputId = "data_upload_size"))
          )
      ),
      tabPanel(title = HTML("<b style='font-size:18px'>Cascade data</b>"),
        br(),
        column(10, offset=1, DTOutput(outputId = "data_upload_prop"))
      )
    )
  )
}