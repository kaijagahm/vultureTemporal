
#' Make repeatability plots
#' @param df
#' @param var
#' @param situation
#' @param ylab
plotRepeat <- function(df, var, situation = "flight", ylab){
  legend_title <- paste0("Mean\n", var, "\nvalue")
  plt <- df %>%
    filter(type == situation) %>%
    group_by(Nili_id) %>%
    mutate(avg = mean(.data[[var]], na.rm = T)) %>%
    ggplot(aes(x = seasonUnique, y = get(var), group = Nili_id, color = avg))+
    geom_point()+
    geom_line(alpha = 0.7)+
    theme_classic()+
    scale_color_viridis_c(name = legend_title)+
    xlab("Season")+
    ylab(ylab)+
    theme(axis.title = element_text(size = 16))
  return(plt)
}

cols_to_remove <- c("tag_id", "sensor_type_id", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "battery_charge_percent", "battery_charging_current", "external_temperature", "gps_hdop", "gps_satellite_count", "gps_time_to_fix", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol", "tag_voltage", "update_ts", "visible", "deployment_id", "event_id", "sensor_type", "tag_local_identifier", "location_long.1", "location_lat.1", "optional", "sensor", "earliest_date_born", "exact_date_of_birth", "group_id", "individual_id", "latest_date_born", "local_identifier", "marker_id", "mates", "mortality_date", "mortality_latitude", "mortality_type", "nick_name", "offspring", "parents", "ring_id", "siblings", "taxon_canonical_name", "taxon_detail", "number_of_events", "number_of_deployments")

# Function to get degree and strength
getmetrics <- function(graph, interval, type, days){
  if(length(graph) > 0){
    metrics <- data.frame(degree = igraph::degree(graph),
                          strength = igraph::strength(graph),
                          Nili_id = names(igraph::degree(graph)),
                          int = interval,
                          n = length(V(graph)),
                          type = type,
                          ndays = days) %>%
      mutate(normDegree = degree/(n-1),
             normStrength = strength/sum(strength))
  }else{
    metrics <- data.frame(degree = NA, strength = NA, 
                          Nili_id = NA, int = interval, 
                          n = 0, type = type, ndays = days)
  }
  
  return(metrics)
}

graph_fn <- function(graph, layout, title, bbox){
    g <- ggraph(graph, layout) +
      geom_edge_link(alpha = 0.3)+
      geom_node_point(aes(col = normDegree, size = normDegree))+
      geom_node_text(aes(label = name), 
                     col = "white", 
                     size = 2)+
      scale_color_viridis_c(limits = c(0, 1))+
      scale_size_continuous(limits = c(0, 1))+
      theme_void()+
      xlim(bbox$xmin[1], bbox$xmax[1])+
      ylim(bbox$ymin[1], bbox$ymax[1])+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "gray80", color = "gray80"))+
      ggtitle(title)
    return(g)
}
