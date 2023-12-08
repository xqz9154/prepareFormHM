library(shiny)
library(sf)
library(shinyjs)
library(purrr)

ui <- fluidPage(
  useShinyjs(),  
  br(),
  fluidRow(column(6, offset = 2,
                  fileInput("shp", label = "Input Shapfiles (.shp,.dbf,.sbn,.sbx,.shx,.prj)", 
                            width = "100%", accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj"),
                            multiple=TRUE)),
           
           column(2, id = "clear", 
                  actionButton('reset', 'Clear Data', width = "100%", 
                               style = "margin-top: 25px;"))),
  
  br(),
  fluidRow(column(8, offset = 2,
                  p("input$shp$datapath" , style = "font-weight: bold"),                              
                  verbatimTextOutput("shp_location", placeholder = T))),
  
  br(),
  fluidRow(column(8, offset = 2,
                  p("input$shp$name" , style = "font-weight: bold"),                              
                  verbatimTextOutput("shp_name", placeholder = T))),  
  
  br(),
  fluidRow(column(8, offset = 2,
                  p("simple feature read-in" , style = "font-weight: bold"),                              
                  verbatimTextOutput("sf", placeholder = T))))

server <- function(input, output, session) {
  
  # Read-in shapefile function
  Read_Shapefile <- function(shp_path) {
    read_shp <- reactive({
      req(shp_path)
      infiles <- shp_path()$datapath # get the location of files
      print(infiles)
      dir <- unique(dirname(infiles)) # get the directory
      # print(dir)
      outfiles <- file.path(dir, shp_path()$name) # create new path name
      # print(outfiles)
      name <- strsplit(shp_path()$name[1], "\\.")[[1]][1] # strip name 
     
      # print(name)
      purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
      x <- read_sf(file.path(dir, paste0(name, ".shp")))# try to read-in shapefile
      # print(x)  # return Null or SF object
    })
    return(read_shp)
  }    
  
  # Read-in shapefile
  shp_path <- reactive({input$shp})
  user_shp <- Read_Shapefile(shp_path)
  
  # Print shapefile if it exists 
  observeEvent(input$shp, {
    if(!is.null(user_shp())) {
      output$sf <- renderPrint({user_shp()})
    }else{
      output$sf <- renderPrint({"NULL"})
    }
    
    # Print original file path location and file name to UI
    output$shp_location <- renderPrint({
      full_path <- strsplit(input$shp$datapath," ")
      purrr::walk(full_path, ~cat(.x, "\n")) 
    })
    
    output$shp_name <- renderPrint({
      name_split <- strsplit(input$shp$name," ")
      purrr::walk(name_split, ~cat(.x, "\n")) 
    })
  })
  
  # Clear UI
  observeEvent(input$reset,{
    reset("shp")
    output$sf <- renderPrint({ })
    output$shp_location <- renderPrint({ })
    output$shp_name <- renderPrint({ })
  })
}

shinyApp(ui, server)