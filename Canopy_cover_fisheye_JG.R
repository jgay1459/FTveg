library(tidyverse)
library(raster)
library(terra)
library(here)
library(ijtiff)
library(hemispheR)


################################################################################
### Bring in necessary packages
package.list = c("raster", "tidyverse", "hemispheR", "terra","here", "ijtiff")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org")

## And loading them
for(i in package.list){library(i, character.only = T)}

######################################################

c.im <- "~/Dropbox/Fire Lab/Data/2023/Canopy cover photos/canopycover/" 
photo_files <- list.files(path = c.im, pattern = ".JPG", full.names = TRUE)

# Create an empty data frame to store the results
analyses_df <- data.frame()


###Loop to analyze field plot images for LAI + Openness
for (file_path in photo_files) {
  cat("Processing:", file_path, "\n")
  
  img <- import_fisheye(circ.mask = camera_fisheye('Coolpix4500+FC-E8'),file = file_path) %>%
    binarize_fisheye() %>%
    gapfrac_fisheye(lens = 'FC-E8', nrings = 7, nseg = 8, endVZA = 70, display = TRUE) %>% ##not sure how we want to parameterize these i.e use Zenith rings similar to LAI-2000/2200: 
    canopy_fisheye()
  
  # Store the analysis results in the data frame
  analyses_df <- rbind(analyses_df, img)
}

# Save the data frame to a CSV file
write.csv(analyses_df, "analyses_results_oct.csv", row.names = FALSE)


########
###code to print a single plot photo
#######

im <-"~/Dropbox/Fire Lab/Data/2023/Canopy cover photos/DSCN0124.JPG" 

print.im <- im %>%
  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) %>%
  binarize_fisheye() %>%
  gapfrac_fisheye(lens='FC-E8',nrings=7,nseg=8,endVZA=70,display=TRUE) %>%
  canopy_fisheye()

##Zenith rings similar to LAI-2000/2200 method:
print.im.compare <- im %>%
  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) %>%
  binarize_fisheye() %>%
  gapfrac_fisheye(lens='FC-E8',nrings=5,nseg=8,endVZA=75,display=TRUE) %>%
  canopy_fisheye()

####The ring parameter adds more circles and the nseg adds more "pizza slices. enVZA increases the diameter of the outer most circle image analyzed




####Possible adjustments might be needed to be made based on time of photograph, ideally photos are taken close to sunrise or sunset


## other methods from the package manual 

# #Zenith rings similar to LAI-2000/2200:
# c.im |>
#   import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#   binarize_fisheye() |>
#   gapfrac_fisheye(lens='FC-E8',nrings=5,nseg=8,endVZA=75,display=TRUE) |>
#   canopy_fisheye()
# #The hinge angle method close to 1 radian (57 degree):
# c.im |>
#   import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#   binarize_fisheye() |>
#   gapfrac_fisheye(lens='FC-E8',nrings=1,nseg=8,startVZA=55,endVZA=60,display=TRUE) |>
#   canopy_fisheye()