




# # Fill single cell sinks then breach larger sinks
# wbt_breach_depressions_least_cost(
#   dem = input$dem$datapath,
#   output = here('./Output/dem_clip_fill_breached.tif'),
#   dist = 5,
#   fill = TRUE
# )
# wbt_fill_depressions_wang_and_liu(
#   dem = here('./output/dem_clip_fill_breached.tif'),
#   output = here('./output/dem_clip_fill_breached_wang_liu.tif')
# )
#
# # Create D8 flow accumulation and D8 pointer grids
# wbt_d8_flow_accumulation(
#   input = here('./Output/dem_clip_fill_breached_wang_liu.tif'),
#   output = here('./Output/dem_clip_D8FA.tif')
# )
# wbt_d8_pointer(
#   dem = here('./Output/dem_clip_fill_breached_wang_liu.tif'),
#   output = here('./Output/dem_clip_D8pt.tif')
# )
#
# # Extract streams
# wbt_extract_streams(
#   flow_accum = here('./Output/dem_clip_D8FA.tif'),
#   output = here('./Output/raster_streams.tif'),
#   threshold = 5000
# )
# wbt_raster_streams_to_vector(
#   streams = here('./Output/raster_streams.tif'),
#   d8_pntr = here('./Output/dem_clip_D8pt.tif'),
#   output = here('./Output/streams_6000.shp')
# )
#
# # Run watershed function
# wbt_watershed(
#   pour_points = input$pour_points$datapath,
#   d8_pntr = here('./Output/dem_clip_D8pt.tif'),
#   output = here('./Output/watershed.shp')
# )

# Display the result
# output$map <- renderLeaflet({
#   leaflet() %>%
#     (st_geometry(read_sf(here(
#       './Output/watershed.shp'
#     ))))
# })