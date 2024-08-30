
# Install and load necessary packages
install.packages("sf")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("dplyr")
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)

# Read the Shapefile
shapefile_path <- "/Users/kabeer/Downloads/dataverse_files/Kerela landslide.shp"
kerala_landslides <- st_read(shapefile_path)

# Replace the short forms in the dataset with their full forms
kerala_landslides <- kerala_landslides %>%
  mutate(Impact_Agr = recode(Impact_Agr,
                             "BSL" = "Bare farmland",
                             "FCP" = "Forest plantation",
                             "FMP" = "Mixed forest plantation",
                             "TEA" = "Tea Plantation",
                             "RUB" = "Rubber plantation",
                             "SPL" = "Shrub plantation",
                             "GMC" = "Meadows (cultivated grassland)",
                             "FNO" = "Open Natural Forest"),
         Road_impac = recode(Road_impac,
                             "N" = "No Impact",
                             "C" = "Collapsed",
                             "D" = "Damaged"))

# Summarize Building_I separately as it's numeric
building_impact_data <- kerala_landslides %>%
  summarize(Total_Buildings_Impacted = sum(as.numeric(Building_I), na.rm = TRUE))

# Count occurrences of each category in Impact_Agr and Road_impac
impact_agr_data <- kerala_landslides %>%
  count(Impact_Agr)

road_impact_data <- kerala_landslides %>%
  count(Road_impac)

# Plot the impact categories for agriculture
ggplot(impact_agr_data, aes(x = Impact_Agr, y = n, fill = Impact_Agr)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Impact on Agriculture by Landslides",
       x = "Impact Type (Agriculture)",
       y = "Count",
       fill = "Impact Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter out the "No Impact" category from the data for roads
road_impact_filtered <- road_impact_data %>%
  filter(Road_impac != "No Impact")

# Plot the impact categories for roads, excluding "No Impact"
ggplot(road_impact_filtered, aes(x = Road_impac, y = n, fill = Road_impac)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Impact on Roads by Landslides",
       x = "Impact Type (Roads)",
       y = "Count",
       fill = "Impact Type")

# Convert the geometry to WGS84 (if not already in that CRS)
kerala_landslides <- st_transform(kerala_landslides, crs = 4326)

# Remove rows where Impact_Agr is "N"
kerala_landslides <- kerala_landslides %>%
  filter(Impact_Agr != "N")

# Add a categorical column for visualization, if needed
kerala_landslides <- kerala_landslides %>%
  mutate(Land_Use_Type = recode(Impact_Agr,
                                "BSL" = "Bare farmland",
                                "FCP" = "Forest plantation",
                                "FMP" = "Mixed forest plantation",
                                "TEA" = "Tea Plantation",
                                "RUB" = "Rubber plantation",
                                "SPL" = "Shrub plantation",
                                "GMC" = "Meadows (cultivated grassland)",
                                "FNO" = "Open Natural Forest"))

# Create a color palette for the land use types
land_use_pal <- colorFactor(palette = "Set3", domain = kerala_landslides$Land_Use_Type)

# Create the leaflet map for point data
map <- leaflet(kerala_landslides) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2],
                   color = ~land_use_pal(Land_Use_Type), weight = 1, opacity = 1,
                   fillOpacity = 0.7, popup = ~paste("Land Use:", Land_Use_Type,
                                                     "<br>District:", District,
                                                     "<br>Area:", Area)) %>%
  addLegend(pal = land_use_pal, values = ~Land_Use_Type, opacity = 1,
            title = "Land Use Type", position = "bottomright")

# Print the map
map
