library(didactr)
library(googledrive)
library(dplyr)

check_didactr_auth()
gs_id = "https://drive.google.com/drive/u/0/folders/1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC"
id = get_folder_id(gs_id)

course_name = "Temporary"
tfile = tempfile()
result = create_course(course_name = course_name, 
              folder_id = gs_id,
              root_path = tfile,
              open = FALSE, 
              git = FALSE,
              rstudio = TRUE)

