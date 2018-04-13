

# APP ---------------------------------------------------------------------

function(input, output, session) {
 
 
 # SET THEME --------------------------------------------------------------
 
 theme_nord <- function(text_size = 16,
                        bg_color = nord_palettes$polarnight[1L],
                        title_color   = "gray80", text_color = "gray90",
                        grid_color    = nord_palettes$polarnight[3L],
                        caption_color = nord_palettes$polarnight[4L]) {
  
  theme(text = element_text("Open Sans", size = text_size, color = text_color),
        panel.grid.major   = element_line(color = grid_color),
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.text  = element_text(color = text_color),
        axis.title = element_blank(),
        plot.title = element_text(color = title_color),
        plot.subtitle = element_text(color = title_color),
        plot.caption  = element_text(colour = caption_color, size = 10),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color))
  
 }
 
 
 # SET UP AGGREGATING FUNCTIONS -------------------------------------------
 
 compute_weekly_mean <- function(df, grouping_vars) {
  df %>%
   group_by(!!!grouping_vars) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   summarize(RigCount = mean(RigCount,  na.rm = TRUE)) %>%
   ungroup() 
 }
 
 get_start_end <- function(df, grouping_vars) {
  df %>%
   group_by(!!!grouping_vars) %>%
   summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
   arrange(desc(PublishDate)) %>%
   mutate(PublishDate = if_else(PublishDate == min(PublishDate), "Start", "End")) %>%
   spread("PublishDate", "RigCount") %>%
   mutate_at(vars(Start, End), function(x) if_else(is.na(x), 0L, x)) %>%
   mutate(RigCountChange = End - Start) %>%
   ungroup()
 }
 

 # STATIC LINE CHART ------------------------------------------------------
 staticCharLevel <- rc_master %>%
  group_by(PublishDate) %>%
  summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(PublishDate, RigCount)) +
  labs(x = NULL, y = NULL, title = "Rig Count") +
  theme(panel.grid.major.y = element_blank())
 

 # ADD DYNAMIC RANGE TO STATIC CHART --------------------------------------
 output$chartLevel <- renderPlot({
  staticCharLevel +
   geom_rect(aes(xmin = as.Date(input$startWeek), xmax = as.Date(input$endWeek),
                 ymin = -Inf, ymax = Inf),
             inherit.aes = FALSE, fill = "steelblue", alpha = 0.01)
 })
 
 
 # ASSEMBLE DATA ----------------------------------------------------------
 mapLevelData <- reactive({
  
  
  if (input$boundary == "County") {
   # COUNTY
   
   rc_master %>%
    filter(between(PublishDate, as.Date(input$startWeek), as.Date(input$endWeek))) %>%
    compute_weekly_mean(quos(County, `State/Province`, PublishDate))
   
  } else {
   # BASIN
   
   c(isBasin, isNotBasin) %<-% {
    rc_master %>%
     filter(between(PublishDate, as.Date(input$startWeek), as.Date(input$endWeek))) %>%
     split(.$Basin == "Other")
   }
   
   isBasin <- isBasin %>%
    compute_weekly_mean(quos(Basin, PublishDate)) %>%
    right_join(sf_plays, c("Basin" = "Shale_play"))
   
   isNotBasin <- isNotBasin %>%
    compute_weekly_mean(quos(`State/Province`, PublishDate)) %>%
    mutate(`State/Province` = str_to_title(`State/Province`)) %>%
    right_join(states_map_adjusted_simp, by = c("State/Province" = "NAME"))
   
   isNotBasin %>%
    rename(Basin = `State/Province`) %>%
    select(Basin, RigCount, geometry) %>%
    rbind({
     isBasin %>%
      select(Basin, RigCount, geometry)
    }) %>%
    st_sf()
   
  }
 })
 
 mapChangeData <- reactive({
  
  if (input$boundary == "County") {
   # COUNTY
   
   if (input$startWeek == input$endWeek) {
    
    rc_master %>%
     filter(PublishDate == input$startWeek) %>%
     distinct(County, `State/Province`) %>%
     mutate(RigCountChange = factor(0))
    
   } else {
    
    rc_master %>%
     filter(PublishDate == input$startWeek | PublishDate == input$endWeek) %>%
     get_start_end(quos(County, `State/Province`, PublishDate)) 
    
   }
   
  } else {
   # BASIN
   
   if (input$startWeek == input$endWeek) {
    
    c(isBasin, isNotBasin) %<-% {
     rc_master %>%
      filter(PublishDate == input$startWeek) %>%
      split(.$Basin == "Other")
    }
    
    isBasin <- isBasin %>%
     distinct(Basin) %>%
     mutate(RigCountChange = factor(0)) %>%
     right_join(sf_plays, c("Basin" = "Shale_play"))
    
    isNotBasin <- isNotBasin %>%
     distinct(`State/Province`) %>%
     mutate(RigCountChange = factor(0)) %>%
     mutate(`State/Province` = str_to_title(`State/Province`)) %>%
     right_join(states_map_adjusted_simp, by = c("State/Province" = "NAME"))
    
    isNotBasin %>%
     rename(Basin = `State/Province`) %>%
     select(Basin, RigCountChange, geometry) %>%
     rbind({
      isBasin %>%
       select(Basin, RigCountChange, geometry)
     }) %>%
     st_sf()
    
   } else {
    
    c(isBasin, isNotBasin) %<-% {
     rc_master %>%
      filter(PublishDate == as.Date(input$startWeek) | PublishDate == as.Date(input$endWeek)) %>%
      split(.$Basin == "Other")
    }
    
    isBasin <- isBasin %>%
     get_start_end(quos(Basin, PublishDate)) %>%
     right_join(sf_plays, c("Basin" = "Shale_play"))
    
    isNotBasin <- isNotBasin %>%
     get_start_end(quos(`State/Province`, PublishDate)) %>%
     mutate(`State/Province` = str_to_title(`State/Province`)) %>%
     right_join(states_map_adjusted_simp, by = c("State/Province" = "NAME"))
    
    isNotBasin %>%
     rename(Basin = `State/Province`) %>%
     select(Basin, RigCountChange, geometry) %>%
     rbind({
      isBasin %>%
       select(Basin, RigCountChange, geometry)
     }) %>%
     st_sf()
    
   }
  }
 })
 
 
 # CREATE MAP -------------------------------------------------------------
 output$mapLevel <- renderPlot({
  
  if (input$boundary == "County") {
   # COUNTY
   
   mapLevelData() %>%
    right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>%
    ggplot() +
    geom_sf(aes(fill = RigCount), color = "gray55") +
    coord_sf(datum = NA) +
    scale_fill_viridis_c() +
    theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
   
  } else {
   # BASIN
   
   mapLevelData() %>%
    ggplot() +
    geom_sf(aes(fill = RigCount), color = "gray55") +
    coord_sf(datum = NA) +
    scale_fill_viridis_c() +
    theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
   
  }
 })
 
 output$mapChange <- renderPlot({
  
  if (input$boundary == "County") {
   # COUNTY
   
   if (input$startWeek == input$endWeek) {
    # startWeek == endWeek
    
    c(notNAMapData, naMapData) %<-% {
     mapChangeData() %>%
      right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>%
      split(is.na(.$RigCountChange))
    } 
    
    ggplot(naMapData) +
     geom_sf(fill = "gray50") +
     geom_sf(fill = "orange", data = notNAMapData) +
     coord_sf(datum = NA) +
     theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
    
   } else {
    # startWeek != endWeek
    
    mapChangeData() %>%
     right_join(counties_2014_adjusted_simp, c("County" = "NAME", "State/Province" = "state_name")) %>% # previously an inner_join()
     ggplot() +
     geom_sf(aes(fill = RigCountChange),
             color = "gray55") +
     coord_sf(datum = NA) +
     scale_fill_gradient2("Change in Rig Count", na.value = "gray50") +
     theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
    
   } 
  } else {
   # BASIN
   
   if (input$startWeek == input$endWeek) {
    # startWeek == endWeek
    
    c(notNAMapData, naMapData) %<-% {
     mapChangeData() %>%
      split(is.na(.$RigCountChange))
    } 
    
    ggplot(naMapData) +
     geom_sf(fill = "gray50") +
     geom_sf(fill = "orange", data = notNAMapData) +
     coord_sf(datum = NA) +
     theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
    
   } else {
    # startWeek != endWeek
    
    mapChangeData() %>%
     ggplot() +
     geom_sf(aes(fill = RigCountChange),
             color = "gray55") +
     coord_sf(datum = NA) +
     scale_fill_gradient2("Change in Rig Count", na.value = "gray50") +
     theme(legend.direction = "horizontal", legend.position = c(0.5, 0.95))
    
   }
  }
 })
 
 
 # Create stateMapLayout
 # stateMapLayout <- ggplot() +
 #  geom_sf(data = states_map_adjusted_simp, fill = "#00000000",color = nord_palettes$polarnight[4L]) +
 #  scale_color_gradient(low = nord_palettes$polarnight[1L],
 #                       high = nord_palettes$polarnight[4L]) +
 #  scale_x_continuous(expand = expand_scale()) +
 #  scale_y_continuous(expand = expand_scale()) +
 #  theme_nord(16) +
 #  theme(axis.title = element_blank()) +
 #  theme(legend.position = c(0.85, 0.08),
 #        legend.justification = c("left", "bottom"),
 #        legend.box.just = "right",
 #        legend.text = element_text(color = "gray80"),
 #        plot.background = element_rect(fill = nord_palettes$polarnight[1L],
 #                                       color = nord_palettes$polarnight[1L]))
 
}

