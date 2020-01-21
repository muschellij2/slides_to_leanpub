## app.R ##
# auth_file = "google_authorization.json"
# Sys.setenv(GL_AUTH = auth_file)
library(animation) #need for ffmpeg
library(shiny)
library(shinyjs)
library(shinydashboard)
library(text2speech)
library(ari)
library(ariExtra)
library(didactr)
library(googleAuthR)
library(googleLanguageR)
library(png)
library(dplyr)
library(googledrive)

source("helper_functions.R")

ari::ffmpeg_exec()
x = ari::ffmpeg_audio_codecs()
x = x[ x$encoding_supported, ]
# cat(file = stderr(), paste(x$codec, collapse = "\n"))
# cat(file = stderr(), paste(x$codec_name, collapse = "\n"))

ari::set_audio_codec("aac")

# need these for rechecks
gs_id = ""


##############################
# User Interface
##############################
ui <- dashboardPage(
    dashboardHeader(title = "Folder → Course"),
    ## Sidebar content
    dashboardSidebar(
        shinyjs::useShinyjs(),  # Set up shinyjs
        
        sidebarMenu(
            selectInput("service", label = "Voice Service", 
                        choices = c("google", "amazon", "microsoft"),
                        selected = "google"),
            textInput("voice", label = "Voice to use", value = 
                          "en-US-Standard-B"),
            menuItem("Google Folder", tabName = "gs", icon = icon("google-drive")),
            actionButton("clear_results", "Clear Previous Results",
                         icon = icon("trash"))
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "gs",
                fluidRow(
                    box(dataTableOutput("gs_df")),
                    box(
                        title = "Inputs",
                        textInput("gs_id", HTML(paste0(
                            "Google Folder ID ", "(e.g. ", 
                            "1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC )", "<br>", 
                            "This will open an authentication window"))
                        )
                    ),
                    box(
                        title = "Render Video",
                        shinyjs::disabled(
                            actionButton("gs_render", "Render Video",
                                         icon = icon("video"))
                        ),
                        shinyjs::disabled(
                            downloadButton("gs_download", "Download Video",
                                           icon = icon("download"))
                        )
                    ),
                    box(
                        HTML(
                            paste0(
                                "If any errors occur, please see the JavaScript log"
                            )
                        )
                    )                    
                    
                )
            )
        )
    )
)

##############################
# full server
##############################
server <- function(input, output) {
    cat_and_log = function(msg) {
        shinyjs::logjs(msg)
        cat(file = stderr(), msg)
    }
    
    vals = reactiveValues(gs_id = NULL)
    
    
    folder_df = reactive({
        if (!is.null(input$gs_id)) {
            check_didactr_auth()
            df = gs_folder_df(trimws(input$gs_id), slides_only = TRUE)
            print(df)
            df = df %>% 
                select(name, id)
            # df = as.data.frame(df)
            # df = df[, c("name", "id")]
            print(df)
        } else {
            df = tibble::tibble()
        }
        df
    } )    
    output$gs_df <- renderDataTable({
        validate(
            need(input$gs_id, "Need Google Folder ID")
        )
        if (!is.null(input$gs_id)) {
            df = folder_df()
        } else {
            df = tibble::tibble()
        }
        df
        # as.data.frame(df)
    }, escape = FALSE)

    
    observeEvent(input$gs_render, {
        if (!exists("gs_ari_result")) {
            validate(
                need(input$gs_id, "Need Google Folder ID")
            )
            
            gs_ari_result <<- folder_df()
        }
        cat_and_log("Running ari for GS")
        withCallingHandlers(
            {            
                video = run_ari(gs_ari_result, 
                                voice = input$voice,
                                service = input$service)
            },
            message = function(m) {
                shinyjs::logjs(m$message)
            },
            warning = function(w) {
                shinyjs::logjs(w$message)
            }            
        )            
        shinyjs::enable("gs_download") 
        gs_video <<- attr(video, "outfile")
    })
    
    ################################
    # Rendering Video for Downlaod
    ################################
    output$gs_download <- downloadHandler(
        filename = function() {
            name = paste0(
                "gs_", 
                input$gs_id,
                "_",
                input$voice,
                "_",
                input$service,
                ".mp4")
            cat_and_log(paste0("output file is ", name))
            return(name)
        },
        content = function(output) {
            file.copy(gs_video, output, overwrite = TRUE)
        }
    )
    
    
}

shinyApp(ui, server)


