# main.R
print(getwd())
# runApp will start from project dir, so change the source dir, then source with (source("./R/load_library.R"))
# while shinyApp will start from the dir same with current code, e.g., main.R, then source with (source("load_library.R"))
source("./R/load_library.R")
# source("./R/upload_display.R")
# source("./R/draw_polygon.R")
source("./R/mo_downloadDEM.R")
source("./R/mo_fill_sinks.R")
source("./R/mo_fdir_facc.R")
source("./R/mo_create_streams.R")
source("./R/mo_pour_points.R")
# source("./R/mo_catchment.R")

options(shiny.maxRequestSize = 100 * 1024^2)
# 
ui <- navbarPage(
  "Shiny app for Automatic Delineation Catchment" ,
  # tabPanel("Draw a polygon", draw_polygon_ui("draw_polygon")),
  # tabPanel("Upload a Shapefile", upload_display_ui("upload_display")),
  tabPanel("Download DEM", downloadDEM_ui("downloadDEM")),
  tabPanel("Fill sinks",fill_sinks_ui("Fill_sinks")),
  tabPanel("Flow direction and Flow accumulation",fdir_facc_ui("fdc")),
  tabPanel("Create streams",create_streams_ui("streams")),
  tabPanel("Setting Pour points",pour_points_ui("ppts"))
  
  # tabPanel("Delineat catchment",catchment_ui("catchment"))
)


server <- function(input, output, session) {
  drawnPolygon <- reactiveValues(drawnPolygon = NULL)
  uploadedShapefile <- reactiveValues(value = NULL)
  
  # draw_polygon_server("draw_polygon", drawnPolygon)
  # upload_display_server("upload_display", uploadedShapefile)
  downloadDEM_server("downloadDEM",uploadedShapefile,drawnPolygon)
  fill_sinks_server("Fill_sinks")
  facc_values <- fdir_facc_server("fdc")
  # Use the reactive value in create_streams_server
  observe({
    # Wait until facc_values$facc_path is available
    req(facc_values$facc_path)
    
    # Use the reactive value in create_streams_server
    create_streams_server("streams", facc_values$facc_path)
  })
  pour_points_server("ppts")
  # catchment_server("catchment")
}

# runApp will start from project dir
runApp(list(ui = ui, server = server), launch.browser = TRUE)
