# mo_fdir_facc.R


# UI function
#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
fdir_facc_ui <- function(id) {
  ns <- NS(id)
  tagList(
  fileInput(ns("dem"), "Upload DEM")
  )
  
}

# server function
#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
fdir_facc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues(facc_path = NULL)

    observeEvent(input$dem, {
      req(input$dem)
      # read filled sink dem file
      dem <- raster(input$dem$datapath)
      temp_dir <- getwd()
      print(dem)
      output_path = paste0(temp_dir, "/output/")
      if (!dir.exists(output_path) ){
        dir.create(output_path)
      }
      # create flow direction
      wbt_d8_pointer(dem = dem,
                     output = paste0(output_path, 'fdir.tif'),
                     wd = temp_dir)

      # # # Create D8 flow accumulation
      wbt_d8_flow_accumulation(input = dem,
                               output = paste0(output_path, 'facc.tif'),
                               wd = temp_dir)

      values$facc_path <- paste0(output_path, 'facc.tif')
    })

    return(values)
  })
}


