source("server/upload_data_server.R")
source("server/plots_server.R")

server <- function(input, output) {
  
  upload_data_server(input, output)

  ######## Output % plots & associated datatable #############
  
  plots_server(input, output)

  
  ########################################
  
}