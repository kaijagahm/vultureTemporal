library(tidyverse)
library(sf)

# Simplify data files
tar_load(data_seasons)

data_seasons_simplified <- map(data_seasons, ~.x %>%
                                 select(-c("heading", "height_above_msl", "trackId", "dataset", "keep", "remove", "daylight", "day_diff", "night_outlier", "birth_year", "sex","month", "season", "age", "age_group", "daysTracked", "timestampIsrael", "year", "day")))

# Shift coordinates
# Transform to utm
data_seasons_simplified <- map(data_seasons_simplified, ~{
  sf::st_as_sf(.x) %>% sf::st_transform(32636)
})

# extract the columns for x and y and drop the geometry
data_seasons_simplified <- map(data_seasons_simplified, ~{
  bind_cols(.x, as.data.frame(sf::st_coordinates(.x))) %>% 
    sf::st_drop_geometry()
})

# Now choose the shifts, randomly
set.seed(4)
# pick x shift (in km) and convert to m
x_shift_km <- runif(1, -100, 100)
x_shift_m <- x_shift_km*1000

# pick y shift (in km) and convert to m
y_shift_km <- runif(1, -100, 100)
y_shift_m <- y_shift_km*1000

# Apply the shifts
data_seasons_simplified_shifted <- map(data_seasons_simplified, ~{
  out <- .x %>% mutate(X = X + x_shift_m,
                       Y = Y + y_shift_m) %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = 32636) %>%
    sf::st_transform("WGS84")
  return(out)
})

for(i in 1:length(data_seasons_simplified_shifted)){
  write_csv(data_seasons_simplified_shifted[[i]], file = paste0(here("data/created/data_seasons_simplified_shifted_"), i, ".csv"))
}

# Now we have to shift the roost polygons by the same amount
tar_load(roostPolygons)
roostPolygons_shifted <- st_transform(roostPolygons, 32636)
shifted_geom <- st_geometry(roostPolygons_shifted) + c(x_shift_m, y_shift_m)
st_geometry(roostPolygons_shifted) <- shifted_geom
roostPolygons_shifted <- st_set_crs(roostPolygons_shifted, 32636)

st_write(roostPolygons_shifted, here("data/created/roostPolygons_shifted.kml"), driver = "kml", append = FALSE)
