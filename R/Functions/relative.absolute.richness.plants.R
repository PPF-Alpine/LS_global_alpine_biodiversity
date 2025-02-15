#---------------------------------------------------------------------------------------------#
# Function to calculate species richness for the different thresholds of the lower limit ---
#----------------------------------------------------------------------------------------------#

# filter data based on alpine category
filter.data.by.condition <- function(data, condition) {
  if (condition == "specialists") {
    return(data |> filter(max_elev >= treeline_GIFT, min_elev >= treeline_GIFT))
  } else if (condition != "generalists") {
    return(data |> filter(max_elev >= treeline_GIFT, min_elev >= !!sym(condition)))
  } else {
    return(data |> filter(max_elev >= treeline_GIFT))
  }
}

# calculate richness per geo_entity
calculate.richness <- function(data, all.geo.entities) {
  data |> 
    group_by(geo_entity) |> 
    summarize(richness = n_distinct(work_species), .groups = 'drop') |> 
    complete(geo_entity = all.geo.entities, fill = list(richness = 0))
}

# merge with alpine data 
merge.with.alpine.data <- function(data) {
  data |> 
    left_join(alpine_area_treeline_GIFT |> 
                select(Mountain_range, geo_entity, area_size_gift, alpine_area, perc_alpine_area, log_perc_alpine_area), 
              by = "geo_entity") |> 
    mutate(
      richness_log = log1p(richness),
      Mountain_range = ifelse(geo_entity == "New_Zealand", "New Zealand", Mountain_range)
    ) |> 
    distinct()
}

# build and apply the linear model
apply.linear.model <- function(data) {
  lm.mod <- lm(log1p(richness) ~ log1p(alpine_area), data = data, na.action = na.exclude)
  data$predicted_richness <- exp(predict(lm.mod, newdata = data))
  data$residuals <- insight::get_residuals(lm.mod, type = "response")
  data$z_value <- round(coef(lm.mod)[2], 3)  # Add z value (slope of the SAR)
  return(data)
}

# Main function 
relative.absolute.richness.plants <- function(data, condition) {
  all.geo.entities <- unique(data$geo_entity)  # Get all unique geo entities
  
  filtered.data <- filter.data.by.condition(data, condition)
  richness.data <- calculate.richness(filtered.data, all.geo.entities)
  enriched.data <- merge.with.alpine.data(richness.data)
  final.data <- apply.linear.model(enriched.data)
  
  return(final.data)
}


