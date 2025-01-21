#Install packages
install.packages("tidyverse")
install.packages("terra")
install.packages("giscoR")
install.packages("tidyterra")
install.packages("sf")
install.packages("rnaturalearth")

library(tidyverse)
library(terra)
library(giscoR)
library(tidyterra)
library(sf)
library(rnaturalearth)

#Putting in GHS Pop data
urls <- c(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E1990_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.zip",
  
"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
)
options(timeout = 10000)

for (url in urls){
  download.file(
    url = url,
    path = getwd(),
    destfile = basename(url)
  )
}

lapply(
  basename(urls),
  unzip
)

#loading GHS data
file_names <- list.files(
  path = getwd(),
  pattern = "tif$",
  full.names = T
)

pop_rasters <- lapply( #laaply because this is a list
  file_names,
  terra::rast
)

#country borders

get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "IN",
    resolution = "1"
  )
  return(country)
}

country <- get_country_borders()

#crop/clip global raster to the extent of India

country_pop_rasters <- lapply(
  pop_rasters,
  function(x){
    terra::crop(
      x,
      terra::vect(country),
      snap = "in",
      mask = T
    )
  }
)

#calculate population difference

pop_change <- (
  country_pop_rasters[[2]] - country_pop_rasters[[1]]
)

#categories

get_categories <- function(x){
  terra::ifel(
    pop_change == 0, 0,
    terra::ifel(
      pop_change > 0, 1,
      terra::ifel(
        pop_change <0, -1, pop_change
      )
    )
  )
}

pop_changes_cats <- get_categories(pop_change) |>
  as.factor()


str(pop_changes_cats)
head(pop_changes_cats)

#map

cols <- c(
  "#b73229", #decline
  "#2e5f77", #uninhabited
  "#bed992" #growth
)

p <- ggplot() +
  tidyterra::geom_spatraster(
    data = pop_changes_cats
  ) + 
  geom_sf(
    data = country,
    fill = "transparent",
    color = "grey40",
    size = 0.5
  ) +
  scale_fill_manual(
    name = "Growth or decline?",
    values = cols,
    labels = c(
      "Decline",
      "Uninhabited",
      "Growth"
    ),
    na.translate = FALSE
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, "mm"),
      keywidth = unit(40, "mm"),
      label.position = "bottom",
      label.hjust = 0.5,
      nrow = 1,
      byrow = T,
      drop = T
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.5, 0.95),
    legend.title = element_text(
      size = 20, color = "grey20",
    ),
      legend.text = element_text(
        size = 10, color = "grey20",
    ),
    plot.caption = element_text(
      size = 14, color = "grey40",
      hjust = 0.5, vjust = 5
    ),
    plot.margin = unit(
      c(
        t=0.5, b=-3,
        l=0, r=-3
      ), "lines")
  ) + 
  labs(
    caption = "Population Change in India (1990-2020), Global Human Settlement Layer at 30 arcsecs"
  )

w <- ncol(pop_changes_cats)
h <- nrow(pop_changes_cats)

p

ggsave(
  "india-population-change.png",
  p,
  width = w*9,
  height = h*9,
  units = "px",
  bg = "white"
)

p <- p + 
  geom_sf(
    data = india_states,
    fill = "transparent",
    color = "black", # Adjust the color to distinguish state boundaries
    size = 0.03       # Adjust the size for better visibility
  )


india_states <- gisco_get_nuts(
  country = "IN",
  nuts_level = 1, # State-level boundaries
  resolution = "1"
)

india_states <- st_transform(india_states, crs = st_crs(country))

india_states <- st_crop(india_states, terra::ext(pop_changes_cats))


print(india_states)


crs(india_states)
crs(country) # This should match `india_states`
crs(pop_changes_cats)

st_bbox(india_states)
terra::ext(pop_changes_cats)



india_states <- ne_states(country = "India", returnclass = "sf")

p <- p +
  coord_sf(
    xlim = c(68, 98),  # Adjust longitude range for India
    ylim = c(6, 38),   # Adjust latitude range for India
    expand = FALSE
  )


p

ggsave(
  "india-population-change-expanded.png",
  p,
  width = 12,   # Increase width for better aspect ratio
  height = 10,  # Increase height for better aspect ratio
  units = "in",
  dpi = 300,
  bg = "white"
)


p <- p +
  coord_sf(
    xlim = c(68, 98),   # Adjust longitude range to include all of India
    ylim = c(6, 38),    # Adjust latitude range to include all of India
    expand = FALSE      # Prevent additional space around the map
  )

# Display the updated map
print(p)


p <- p +
  coord_sf(
    xlim = c(68, 98),   # Adjust longitude range to include all of India
    ylim = c(6, 38),    # Adjust latitude range to include all of India
    expand = FALSE      # Prevent additional space around the map
  ) +
  theme(
    plot.caption = element_text(
      size = 14,       # Adjust the font size of the caption
      color = "grey40",
      hjust = 0.5      # Center the caption
    ),
    plot.margin = unit(
      c(1, 1, 2, 1),   # Increase the bottom margin for the caption
      "lines"
    )
  )

# Display the updated map
print(p)


# Get the values of the raster as a vector
pop_change_values <- values(pop_change)

# Sort the values to find the top 1 growth and top 1 decline areas
top_growth_indices <- order(pop_change_values, decreasing = TRUE)[1]  # Top 1 growth


# Convert the indices back to coordinates
top_growth_coords <- xyFromCell(pop_change, top_growth_indices)


# Convert to data frames for ggplot
top_growth_df <- data.frame(x = top_growth_coords[1], y = top_growth_coords[2], type = "Growth")


# Combine both growth and decline data
highlight_df <- rbind(top_growth_df)

# Add the points for the top 1 growth and top 1 decline areas to the map
p <- p +
  geom_point(
    data = highlight_df,
    aes(x = x, y = y, color = type),  # Color by growth or decline
    size = 5, alpha = 0.9  # Increase size and visibility for emphasis
  ) +
  scale_color_manual(
    values = c("Growth" = "green")
  )

# Display the updated map
print(p)


max(pop_change_values, na.rm = TRUE)  # Maximum population change
min(pop_change_values, na.rm = TRUE)  # Minimum population change


top_growth_indices <- which.max(pop_change_values)
top_decline_indices <- which.min(pop_change_values)

top_growth_coords <- xyFromCell(pop_change, top_growth_indices)
top_decline_coords <- xyFromCell(pop_change, top_decline_indices)

top_growth_coords
top_decline_coords
