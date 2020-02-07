## app.R ##
# auth_file = "google_authorization.json"
# Sys.setenv(GL_AUTH = auth_file)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(didactr)
library(dplyr)
library(googledrive)

# library(animation) #need for ffmpeg
# library(text2speech)
# library(ari)
# library(ariExtra)
# library(googleAuthR)
# library(googleLanguageR)
# library(png)
# ari::ffmpeg_exec()
# x = ari::ffmpeg_audio_codecs()
# x = x[ x$encoding_supported, ]
# ari::set_audio_codec("aac")

source("helper_functions.R")



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
                            "This will open an authentication window",
                            "<br>", 
                            "Warning: slides in this folder will be published"))
                        ),
                        checkboxInput(
                            "add_number", 
                            HTML(
                                paste0(
                                    "Add a prefix to the file names",
                                    "(e.g. slide_deck becomes 00_slide_deck)")
                            )),
                        textInput("course_name", "Course Name")
                    ),
                    box(
                        title = "Render Video",
                        shinyjs::disabled(
                            actionButton("gs_render", "Render Video",
                                         icon = icon("video"))
                        ),
                        shinyjs::disabled(
                            actionButton("gs_create", "Create Course",
                                         icon = icon("video"))
                        ),  
                        shinyjs::disabled(
                            downloadButton("gs_download", "Download Course",
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
    
    createLink <- function(href, name) {
        sprintf(
            '<a href="%s" target="_blank" class="btn btn-primary">%s</a>', 
            href, name)
    }
    createImg <- function(link, height = 100, width = height*9/6) {
        height = round(height)
        width = round(width)
        sprintf(
            '<img src="%s" height="%d" width="%d">',
            link, height, width)
    }
    
    
    folder_df = reactive({
        if (!is.null(input$gs_id)) {
            check_didactr_auth()
            df = gs_folder_df(trimws(input$gs_id), slides_only = TRUE)
            df$link = sapply(df$drive_resource, function(x) {
                createLink(name = "Link", href = x$webViewLink)
            })
            df$thumbnail = sapply(df$drive_resource, function(x) {
                createImg(x$thumbnailLink)
            })            
            # print(df)
            df = df %>% 
                select(name, link, thumbnail)
            # df = as.data.frame(df)
            # df = df[, c("name", "id")]
            print("Course name")
            print(input$course_name)
            if (!is.null(input$course_name) && 
                input$course_name != "") {
                shinyjs::enable("gs_create") 
            }
            
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
    }, escape = FALSE, options = list(searching = FALSE))
    
    
    observeEvent(input$gs_create, {
        validate(
            need(input$gs_id, "Need Google Folder ID"),
            need(input$course_name, "Need Course Name")
        )
        df = folder_df()
        root_path = tempfile()
        vals$root_path = root_path
        
        cat_and_log("Running course creation for GS")
        withCallingHandlers(
            {            
                course_result = create_course(course_name = input$course_name,
                                              root_path = vals$root_path,
                                              folder_id = input$gs_id,
                                              add_number = input$add_number)
            },
            message = function(m) {
                shinyjs::logjs(m$message)
            },
            warning = function(w) {
                shinyjs::logjs(w$message)
            }            
        )            
        vals$course_result = course_result
        shinyjs::enable("gs_download") 
    })
    
    
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
    # output$gs_download <- downloadHandler(
    #     filename = function() {
    #         name = paste0(
    #             "gs_", 
    #             input$gs_id,
    #             "_",
    #             input$voice,
    #             "_",
    #             input$service,
    #             ".mp4")
    #         cat_and_log(paste0("output file is ", name))
    #         return(name)
    #     },
    #     content = function(output) {
    #         file.copy(gs_video, output, overwrite = TRUE)
    #     }
    # )
    
    output$gs_download <- downloadHandler(
        filename = function() {
            cn = vals$course_result$course_name
            name = paste0(cn, ".zip")
            cat_and_log(paste0("output file is ", name))
            return(name)
        },
        content = function(output) {
            owd = getwd()
            cdir = vals$course_result$course_dir
            setwd(dirname(cdir))
            zip(output, files = basename(cdir))
            setwd(owd)
        }
    )
    
    
}

shinyApp(ui, server)


