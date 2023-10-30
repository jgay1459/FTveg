library(ggplot2)
library(sf)
library(terra)
library(readr)


dir <- '~/Dropbox/Fire Lab/'
setwd(dir)

##Clean up Global Environment
remove(list = ls()) 
cat("\014")

#Info about the 250 m modeled soil bulk density data, raw data is in kg / cubic meter, 0-5 cm mineral soil
###https://gitlab.com/openlandmap/global-layers/-/tree/master/soil/soil_properties

# Load your Montana shapefile
mt <- vect("~/Dropbox/Fire Lab/Data/GIS/MontanaStateBoundary_shp/StateofMontana.shp")

# Load your BD raster
BD_0_5cm2 <- rast('~/Dropbox/Fire Lab/Data/Bulk_density/BLDFIE_M_sl2_250m_ll.tif')

# Load the coordinates data frame
coords <- read_csv('~/Dropbox/Fire Lab/Data/FT_veg_coords.csv')

# Load your points as a SpatVector
ftv.points <- vect(coords, crs = crs('EPSG:4326'), geom = c("Long", "Lat"))

# Extract BD values for your coordinates
BD_in_Montana <- extract(BD_0_5cm2, ftv.points)

# Rename the columns
colnames(BD_in_Montana) <- c("ID", "BD_g_cm3")

# Convert BD units to g/cm³
coords$BD_g_cm3 <- BD_in_Montana$BD_g_cm3 / 1000

#### write new .csv with plot bulk densities
write.csv(coords, '~/Dropbox/Fire Lab/Data/bulk_density_plots.csv')


##########################Map of the sampling sites###################
####clean the global environment before running

##Clean up Global Environment
remove(list = ls()) 


# Load your Montana shapefile
mt <- st_read("~/Dropbox/Fire Lab/Data/GIS/MontanaStateBoundary_shp/StateofMontana.shp")

# Convert the mt object to a data frame
mt_df <- st_as_sf(mt)

# Create a base map of Montana
mt_base_map <- ggplot() +
  geom_sf(data = mt_df) +
  theme_void()  # This removes the axes and labels

# Load your coordinates data frame
coords <- read.csv('~/Dropbox/Fire Lab/Data/FT_veg_coords.csv')

# Convert the coordinates data frame to an sf object
coords_sf <- st_as_sf(coords, coords = c("Long", "Lat"), crs = 4326)

# Add your coordinates as points to the map
final_map <- mt_base_map +
  geom_sf(data = coords_sf, color = "black", size = 2) +
  labs(title = NULL, color = NULL)

# Plot the final map
print(final_map)

