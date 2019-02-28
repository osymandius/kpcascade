about <- function() {
  div(
    fluidRow(
      h2(style="margin-left: 10%", "About"),
      br(),
      div(style='font-size:16px; margin-left: 10%; margin-right:10%',
          HTML("<p style='font-size: 14px'>This app was developed following the 3rd SANAC/UCSF Key Populations HIV Treatment Cascades Workshop in 2019.</p> 
               <p style='font-size: 14px'>If you have any difficulties using the app, or have any suggestions for improvements/changes, please email Oli Stevens (o.stevens@imperial.ac.uk)</p>")
      )
    )
  )
}