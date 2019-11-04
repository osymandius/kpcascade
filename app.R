library(shiny)
library(tidyverse)
library(DT)
library(reshape2)
library(scales)
library(lemon)
library(shinyjs)
library(rmarkdown)
library(tinytex)
#library(plotly)
#library(stringr)

source("src/data_manip.R")

source("src/server/data_tables.R")
source("src/server/upload_data_server.R")
source("src/server/cascade_input_server.R")
source("src/server/plots_server.R")

source("src/ui/navigation.R")
source("src/ui/landing.R")
source("src/ui/upload_data.R")
source("src/ui/plots.R")
source("src/ui/about.R")


shiny::shinyAppDir("src")