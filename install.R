Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
install.packages(c("shiny", "shinyjs", "shinydashboard", 
                   "remotes", "dplyr", "googledrive"))
remotes::install_github("muschellij2/didactr")
