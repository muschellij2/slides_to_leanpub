
##############################
# create thumbnails
##############################
thumbnail_args = function(res, max_seq = 4) {
  images = res$images
  max_seq = min(length(images), max_seq)
  images = images[seq(max_seq)]
  args = lapply(res$images, function(img) {
    grid::rasterGrob(as.raster(
      png::readPNG(img)), interpolate = FALSE)
  })
  args$ncol = floor(sqrt(max_seq))
  return(args)
}

##############################
# run ari
##############################
run_ari = function(result, 
                   divisible_height = TRUE,
                   voice = "en-US-Standard-B", service = "google") {
  video = ari::ari_spin(
    images = result$images,
    paragraphs = result$script,
    divisible_height = divisible_height,
    service = service,
    voice = voice,
    verbose = 2)
  if (!video) {
    warning(
      paste0(
        "Video doesn't seem to have generated correctly,",
        " look at logs"
      )
    )
  }
  return(video)
}



is_language_auth = function() {
  inherits(googleAuthR::Authentication$public_fields$token, "Token")
}
check_gl_auth = function() {
  if (!is_language_auth()) {
    needed <- c(
      "https://www.googleapis.com/auth/cloud-language",
      "https://www.googleapis.com/auth/cloud-platform")
    
    googleAuthR::gar_attach_auto_auth(needed,
                                      environment_var = "GL_AUTH")
  }
  is_language_auth()
}

pptx_mime_type = function() {
  paste0(
    "application/",  
    "vnd.openxmlformats-officedocument", 
    ".presentationml.presentation")
}
