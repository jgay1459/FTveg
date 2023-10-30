library(tidyverse)
library(dplyr)
library(reader)
library(stringr)
library(readr)
library(plotrix)
library(wesanderson)
library(RColorBrewer)
library(stringr)
library(vegan)
library(forcats)


##Clean up Global Environment
remove(list = ls())
cat("\014")

##Set working directory

dir <- '~/Dropbox/Fire Lab/'
setwd(dir)

#########
#FTVeg plot, fuels, LPI and canopy cover data
#########

fuels_site_id <- read_csv('~/Dropbox/Fire Lab/Data/2023/Fuels_2023/Plot_identifier.csv') ##from original Fuels_0,##ParentGlobal ID = Plot #
FTv_plot <- read_csv('~/Dropbox/Fire Lab/Data/FTv_plot_v2.csv') ###Ftv_plot_v2 is the most up to date
LAI <- read_csv('~/Dropbox/Fire Lab/Data/2023/Fisheye_LAI_Openness_full.csv')
x1000hr <- read_csv('~/Dropbox/Fire Lab/Data/2023/Fuels_2023/1000_hr_log.csv') #1000 hr lod .csv was originally labeled repeat_log_2
fuel_transect <- read_csv('~/Dropbox/Fire Lab/Data/2023/Fuels_2023/transect_fuels_2.csv') # transect photoloads
soils <- read_csv('~/Dropbox/Fire Lab/Data/2023/soils.csv') #soils web (SSURGO) plot level soils data
LPI_site_id <- read_csv('~/Dropbox/Fire Lab/Data/2023/LPI_2023/LPI_0.csv') ###needed to chanbe GlobalID to ParentGlobalID
LPI_transect <- read_csv('~/Dropbox/Fire Lab/LPI.csv') #also found repeat_transect_1.csv
belt_transect <- read_csv('~/Dropbox/Fire Lab/Data/2023/LPI_2023/species_repeat_2.csv') # changed ParentGlobalID to GlobalID to use joins,  OG GlobalID is now GlobaID2
corrections <- read.csv('~/Dropbox/Fire Lab/Data/2023/LPI_2023/species_corrections.csv')## species corrections list # Meloff and Melofff not working
belt_corrections <- read_csv('~/Dropbox/Fire Lab/Data/2023/LPI_2023/species_corrections_belt.csv')
functional_types <- read.csv('~/Dropbox/Fire Lab/Data/2023/LPI_2023/functional_type_link.csv')
species.compare <- read_csv('~/Dropbox/Fire Lab/sorted_species.csv')

# Compare two columns
unique_to_column1 <- setdiff(species.compare$sorted_species, species.compare$Species_code)
unique_to_column2 <- setdiff(species.compare$Species_code, species.compare$sorted_species)

cat("Values unique to Column1:\n")
print(unique_to_column1)

cat("\nValues unique to Column2:\n")
print(unique_to_column2)

##Replacing misspellings with the gsub method####

#LPI_transect <- LPI_transect %>%
  #mutate_if(is.character, function(x) str_trim(str_squish(x))) Code to remove spaces before and after characters


##remove first two rows since this was practice at Larch camp
LPI_transect <- LPI_transect[-c(1, 2), ]

# Join LPI_site_id_select to LPI_transect by GlobalID
#LPI_transect <- LPI_transect %>%
  #left_join(LPI_site_id, by = c("ParentGlobalID" = "ParentGlobalID"))

# First, convert all columns to character type
LPI_transect <- LPI_transect %>%
  mutate(across(everything(), as.character))

# Define a function to apply corrections to a single column using gsub with whole word matching
correct_species_column <- function(column) {
  # Iterate over each row in the corrections data frame
  for (i in 1:nrow(corrections)) {
    # Construct a regular expression pattern for whole word matching
    misspelled <- paste0("(?<!\\S)", corrections$Misspelled[i], "(?!\\S)")  # Use negative lookbehind and negative lookahead
    
    corrected_value <- corrections$Corrected[i]
    
    # Use gsub to replace misspelled values with corrected values
    column <- gsub(misspelled, corrected_value, column, perl = TRUE, ignore.case = TRUE)
  }
  
  return(column)
}

# Identify the columns with species observations
species_columns <- grep("^layer", names(LPI_transect), value = TRUE)

# Apply corrections to each species column
for (col_name in species_columns) {
  LPI_transect[[col_name]] <- correct_species_column(LPI_transect[[col_name]])
  
  
}

# Extract all species from the selected columns
all_species <- unique(unlist(LPI_transect[species_columns]))

# Sort the species alphabetically
sorted_species <- sort(all_species)

# Save the sorted species to a CSV file
write.csv(data.frame(Species = sorted_species), "sorted_species.csv", row.names = FALSE)
sorted_species <- as.data.frame(sorted_species)


##########Belt transect correction code

# Left join the LPI_transect data for Site and Plot_id
belt_transect <- belt_transect %>%
  left_join(dplyr::select(LPI_transect, GlobalID, Site, Plot_id), by = c("GlobalID" = "GlobalID"))

# Define a function to apply corrections to the 'Species' column using gsub with whole word matching
correct_species_column <- function(column) {
  for (i in 1:nrow(belt_corrections)) {
    misspelled <- paste0("(?<!\\S)", belt_corrections$Misspelled[i], "(?!\\S)")
    corrected_value <- belt_corrections$Corrected[i]
    column <- gsub(misspelled, corrected_value, column, perl = TRUE, ignore.case = TRUE)
  }
  return(column)
}

# Apply corrections to the 'Species' column in belt_transect
belt_transect$Species <- correct_species_column(belt_transect$Species)

# Get unique species from belt_transect
unique_species_list <- belt_transect %>%
  distinct(Species) %>%
  arrange(Species)


#####Making a unique species data frame from the LPI species df

unique_species_list <- belt_transect %>%
  distinct(Species) %>%
  arrange(Species)

unique_w_location_df <- belt_transect %>%
  dplyr::select(Site, Plot_id, Species) %>%
  distinct(Species, Site, Plot_id) %>%
  arrange(Species)

write.csv(unique_species_list, "belt_sorted_species.csv", row.names = FALSE)

####################LPI###########################

# Searching for rows with "0" in the Species column
rows_with_speciesX <- which(apply(unique_sorted_df, 1, function(row) "Holdis" %in% row["Species"]))

###searching for certain species codes in the LPI transect df
rows_with_speciesX <- which(apply(long_data, 1, function(row) "0" %in% row))

# Access rows with "laoc" by row indices
rows_containing_speciesX <- unique_sorted_df[rows_with_speciesX, ]

############Joining site and plot

# Select the columns you want from LPI_site_id
LPI_site_id <- LPI_site_id %>%
  select(ParentGlobalID, Site, Plot_id, TX)

# Join LPI_site_id_select to LPI_transect by GlobalID
LPI_transect <- LPI_transect %>%
  left_join(LPI_site_id, by = c("ParentGlobalID" = "ParentGlobalID"))

#move plot_id and site to the front of the df
LPI_transect <- LPI_transect %>%
  select(Site, Plot_id, TX, everything())

##remove WHG site from the dataset
LPI_transect <- LPI_transect %>% filter(Site != "WHG")


#####

write.csv(LPI_transect, "LPI.csv", row.names = FALSE)

####new code####




########CODE to evaluate unique species codes from the LPI_transect DF before I use the corrections function
# Aspecies columns start with "layer_" 
species_columns_raw <- grep("^layer_", names(LPI_transect), value = TRUE)

# Extract unique species from selected columns
unique_species <- unique(unlist(LPI_transect[species_columns_raw]))

# Sort the unique species alphabetically
unique_species <- sort(unique_species)

# Print or view the sorted unique species
unique_species
###################################### skip to here if not checking





####Linking plant functional type and invasive/fixer distinction to df


####Calculating percent cover by species per plot########################################

# Assuming your wide-format dataset is named LPI_transect
# Subset the columns you need (adjust the range as needed)
subset_LPI <- LPI_transect %>% select(1:206)  # Assuming you have 100 columns with species data

##remove first two rows since this was practice
subset_LPI <- LPI_transect[-c(1, 2), ]

# First, convert all columns to character type
subset_LPI <- subset_LPI %>%
  mutate(across(everything(), as.character))

# Reshape the data from wide to long format, combining "LPI point" with Transect
long_data <- subset_LPI %>%
  pivot_longer(cols = starts_with("layer_"), 
               names_to = "LPI point", 
               values_to = "Species")


###replacing Melofff with Meloff as the sorting for-loop above does not fix this

long_data$Species <- gsub("Melofff", "Meloff", long_data$Species)
long_data$Species <- gsub("Symalbalb", "Symalb", long_data$Species)
long_data$Species <- gsub("Vicamep", "Vicame", long_data$Species)

######
# Merge the data frames based on the "Species" column
long_data <- long_data %>%
  left_join(functional_types, by = c("Species" = "Species"))

# Count the number of times each species is present in each Plot_id
species_count <- long_data %>%
  group_by(Site, Plot_id, Species, TX, functional_type, invasive,nitrogen_fixer) %>%
  summarise(species_count = sum(!is.na(Species)))

# Calculate the percentage cover for each species within each Plot_id
species_count <- species_count %>%
  group_by(Site, Plot_id, TX, functional_type, invasive, nitrogen_fixer) %>%
  mutate(Percentage_Cover = species_count / 72 * 100)  # 72 total pin drops

# Group by 'Site', 'Species', and 'TX', and calculate the total percent cover for each combination
site_species_summary <- species_count %>%
  group_by(Site, Species, functional_type, invasive, nitrogen_fixer) %>%
  summarise(Total_Percent_Cover = sum(Percentage_Cover))

# Calculate the number of unique 'Plot_id' values for each 'Site' and 'TX'
site_plot_counts <- species_count %>%
  group_by(Site) %>%
  summarise(Num_Plots = n_distinct(Plot_id))

# Join the summary data with the number of plots per site and TX
site_species_summary <- site_species_summary %>%
  left_join(site_plot_counts, by = c("Site"))

# Calculate the average percent cover by dividing the total percent cover by the number of plots
site_species_summary <- site_species_summary %>%
  mutate(Average_Percent_Cover = Total_Percent_Cover / Num_Plots)

# rounding sig figs
site_species_summary$Average_Percent_Cover <- round(site_species_summary$Average_Percent_Cover, 1)

# Select the columns you want in the final summary data frame
site_pct_cover_summary <- site_species_summary %>%
  dplyr::select(Site, Species, Average_Percent_Cover, functional_type, invasive, nitrogen_fixer)

# View the summary
View(site_species_summary)


#######Similar code as above but to calculate by functional type and not species

# Remove the first two rows since this was practice
subset_LPI <- LPI_transect[-c(1, 2), ]

# First, convert all columns to character type
subset_LPI <- subset_LPI %>%
  mutate(across(everything(), as.character))

# Reshape the data from wide to long format, combining "LPI point" with Transect
long_data <- subset_LPI %>%
  pivot_longer(cols = starts_with("layer_"), 
               names_to = "LPI point", 
               values_to = "Species")

# Merge the data frames based on the "Species" column
long_data <- long_data %>%
  left_join(functional_types, by = c("Species" = "Species"))

# Count the number of times each species is present in each Plot_id
species_count <- long_data %>%
  group_by(Site, Plot_id, Species, functional_type, invasive, nitrogen_fixer) %>%
  summarise(species_count = sum(!is.na(Species)))

# Calculate the percentage cover for each species within each Plot_id
species_count <- species_count %>%
  group_by(Site, Plot_id, functional_type, invasive, nitrogen_fixer) %>%
  mutate(Percentage_Cover = species_count / 72 * 100)  # 72 pin drops

# Group by 'Site', 'TX', and 'functional_type', and calculate the average percent cover for each combination
functional_type_summary <- species_count %>%
  group_by(Site, functional_type) %>%
  summarise(Average_Percent_Cover = mean(Percentage_Cover, na.rm = TRUE))

# Rounding sig figs
functional_type_summary$Average_Percent_Cover <- round(functional_type_summary$Average_Percent_Cover, 1)

# Replace NA values in the functional_type column with "No herbaceous cover"
functional_type_summary$functional_type <- ifelse(is.na(functional_type_summary$functional_type), "No cover", functional_type_summary$functional_type)



##########Nitrogen fixer % cover analysis########

# Filter rows where 'nitrogen_fixer' is 'Y'
nitrogen_fixer_data <- site_species_summary %>%
  filter(nitrogen_fixer == "Y")

# Group by 'Site' and 'TX' and calculate total percent cover for nitrogen fixers
nitrogen_fixer_summary <- nitrogen_fixer_data %>%
  group_by(Site, TX) %>%
  summarise(Total_Percent_Cover = sum(Total_Percent_Cover))

# Calculate the average percent cover by dividing the total cover by the number of observations
nitrogen_fixer_summary <- nitrogen_fixer_summary %>%
  mutate(Average_Percent_Cover = Total_Percent_Cover / n())

# If you want to round the average cover to one decimal place
nitrogen_fixer_summary$Average_Percent_Cover <- round(nitrogen_fixer_summary$Average_Percent_Cover, 1)


####Looking at N-fixers across sites

long_data_n_fixers <- long_data %>% filter(nitrogen_fixer == "Y")










##### Species total abundance by site ####

long_data_noN <- long_data %>%
  filter(!(is.na(Species) | Species == "N"))

# Group by Site, Plot_id, and Species, and calculate species counts
site_plot_species_counts <- long_data_noN %>%
  group_by(Site, Plot_id, Species) %>%
  summarise(Species_Count = n())

# Group by Site and Plot_id, and calculate the total count for each Plot_id
Abundance <- site_plot_species_counts %>%
  group_by(Site, Plot_id) %>%
  summarise(Total_Count = sum(Species_Count))

# Merge the species counts with total counts
site_plot_species_counts <- site_plot_species_counts %>%
  left_join(Abundance, by = c("Site", "Plot_id"))

# Calculate relative abundance by dividing species count by total count
relative_abundance <- site_plot_species_counts %>%
  mutate(Relative_Abundance = Species_Count / Total_Count)


########abundance counts of unique species within each plot (Richness)

long_data_noN <- long_data %>%
  filter(!(is.na(Species) | Species == "N"))

# Group by Site, Plot_id, and Species, and calculate species counts
site_plot_species_counts <- long_data_noN %>%
  group_by(Site, Plot_id, Species) %>%
  summarise(Species_Count = n())

# Group by Site and Plot_id, and calculate the number of unique species within each Plot_id
Richness <- site_plot_species_counts %>%
  group_by(Site, Plot_id) %>%
  summarise(Unique_Species_Count = n_distinct(Species))


#####Calculating diversity######

plot_shannon_diversity <- site_plot_species_counts %>%
  group_by(Site, Plot_id) %>%
  summarise(Shannon_Diversity = -sum((Species_Count / sum(Species_Count)) * log(Species_Count / sum(Species_Count))))





#######Attaching soils data############################################################################
###this step not necessary if most up to date Ftv_plot is used as the soils data is already added (Ftv_plot_v2)
FTv_plot <- FTv_plot %>%
  left_join(select(soils, Soil_order, Parent_material, Sand_pct, Clay_pct, Plot_id), by = ("Plot_id"))

############# Basal area (BA) calculations #############################################################
###this step not necessary if most up to date Ftv_plot is used as the soils data is already added
BA_cols <- c("Dead_tally", "PIPO_tally", "PSME_tally", "LAOC_tally",
             "PICO_tally", "PIEN_tally", "ABLA_tally", "Other_tally")

# Calculating BA by each tree type and printing to a new column
for (col in BA_cols) {
  new_col_name <- paste0(col, "_BA")
  FTv_plot <- FTv_plot %>%
    mutate({{ new_col_name }} := .data[[col]] * 10)
}
# Calculate the Plot total BA estimate and convert from square ft/acre to meters squared per hectare

conversion_factor <- 1 / 4.356

FTv_plot <- FTv_plot %>%
  mutate_at(vars(ends_with("_BA")), ~ . * conversion_factor) %>%
  mutate(Total_BA = rowSums(select(., ends_with("_BA")), na.rm = TRUE))


########## Pulling and pairing photo ID and plot ###########################################################

###09/08/2023.  Not all photos are currently in the DF need to download the rest

##create new column from the photo id file with just the number
LAI <- LAI %>%
  mutate(plot_photo_id = str_extract(id, "\\d+")) %>%
  mutate(plot_photo_id = as.numeric(plot_photo_id))

###joining LAI and Canopy openess to FTv_plot by plot_photo_id

FTv_plot <- FTv_plot %>%
  left_join(dplyr::select(LAI, plot_photo_id, L, DIFN), by = "plot_photo_id")


######### Transect Fuels ###############################################################################
#note that I had to change the GlobalID in the fuels transect raw .csv to ParentGlobalID to match.

#the suffix argument is used as a many-to-many because there are multiple matches for site and plot id (because of 3 transects in each)
## the NA's produced are from the WHG site we put in (can filter out)
fuel_transect<- fuel_transect %>%
  left_join(fuels_site_id, by = c("ParentGlobalID" = "ParentGlobalID"), suffix = c("_transect", "_site_id"))

###remove WHG plot transects
fuel_transect <- fuel_transect %>%
  filter(ObjectID_transect != 10 & ObjectID_transect != 11 & ObjectID_transect != 12)

# Pivoting columns to rows
fuel_transect_long <- fuel_transect %>%
  pivot_longer(
    cols = c(
      Photoload_1hrb, Photoload_10hrb, Photoload_100hrb, 
      litter_depth_b, duff_depth_b, litter_duff_depthb,
      Photoload_1hra, Photoload_10hra, Photoload_100hra,
      litter_depth_a, duff_depth_a, litter_duff_depth_a
    ),
    names_to = "Rep",
    values_to = "kg_m2"
  )


# Reclassifying a and b transect fuel observations to a common fuel hr (and duff/litter depth)
fuel_transect_long <- fuel_transect_long %>%
  mutate(Fuel_load = case_when(
    Rep %in% c("Photoload_1hra", "Photoload_1hrb") ~ "Photoload_1hr",
    Rep %in% c("Photoload_10hra", "Photoload_10hrb") ~ "Photoload_10hr",
    Rep %in% c("Photoload_100hra", "Photoload_100hrb") ~ "Photoload_100hr",
    Rep %in% c("litter_depth_a", "litter_depth_b") ~ "litter_depth",
    Rep %in% c("duff_depth_a", "duff_depth_b") ~ "duff_depth",
    Rep %in% c("litter_duff_depth_a", "litter_duff_depthb") ~ "litter_duff_depth",
    TRUE ~ Rep  # Keep the original Rep value if none of the conditions match
  ))


###select only the columns needed for summary statistics
fuel_transect_long <- fuel_transect_long %>% dplyr::select(Transect, Site, Plot_id, Rep, kg_m2, Fuel_load)

# Calculate means and standard deviations by Plot_id
summary_stats <- fuel_transect_long %>%
  group_by(Fuel_load, Plot_id) %>%
  summarise(
    Mean = mean(kg_m2, na.rm = TRUE),
    StdDev = sd(kg_m2, na.rm = TRUE)
  )

View(summary_stats)

# Pivot the summarized dataframe back to a wide format
wide_summary_stats <- summary_stats %>%
  pivot_wider(names_from = Fuel_load, values_from = c(Mean, StdDev))

# rename the columns 
colnames(wide_summary_stats) <- c("Plot_id", "Photoload_1hr_Mean", "Photoload_1hr_StdDev",
                                  "Photoload_10hr_Mean", "Photoload_10hr_StdDev",
                                  "Photoload_100hr_Mean", "Photoload_100hr_StdDev",
                                  "litter_depth_Mean", "litter_depth_StdDev",
                                  "duff_depth_Mean", "duff_depth_StdDev",
                                  "litter_duff_depth_Mean", "litter_duff_depth_StdDev")

# Reduce to 3 significant figures
wide_summary_stats[, -1] <- lapply(wide_summary_stats[, -1], function(x) round(x, 2))

####adding transect fuels data to master plot data
FTv_plot <- FTv_plot %>%
  left_join(wide_summary_stats, by = "Plot_id")


#########1000 hr fuels#######################################################

x1000hr_fuel.LKR <- x1000hr_fuel %>% filter(Site == "LKR")

####MAR 306 and BWN354 had large end diameter adjustments made from 150-15cm and 130 to 13cm,  assuming this was a data entry error

###The equation used to calculate 1000hr fuel values was taken from Fraver 2007 et al. and wood densities 
## for solid and rotten wood were taken from Harmon et al 2008. The 0.3 and 0.4 numbers were in g/cm3 and needed
##to be adjusted to kg/m3 (300 and 400)
###Fuel loading is defined as the mass of a fuel component per square area expressed as kg m2
### conic paraboloid equation V = L/12(5AB + 5AU + 2√(AB*AU)).   V= volume in m3, L is Length in m, AB is the
### cross sectional area at the base while AU is the cross sectional area at upper end

x1000hr_fuel <- x1000hr %>%
  left_join(dplyr::select(fuels_site_id, ParentGlobalID, Site, Plot_id), by = "ParentGlobalID")

# Remove rows with NA values (WHG site that was removed)
x1000hr_fuel <- na.omit(x1000hr_fuel)

# Initialize an empty data frame to store results
x1000hr_results <- data.frame(Plot_id = integer(), Mass_kg = numeric(), x1000hr_kg_m2 = numeric(), Site = character())

# Loop through unique plot IDs
for (plot_id in unique(x1000hr_fuel$Plot_id)) {
  # Subset data for the current plot
  plot_data <- x1000hr_fuel %>% filter(Plot_id == plot_id)
  
  # Calculate the volume for each log based on length and diameters (2 ends) - unit m^3
  log_volumes_m3 <- with(plot_data, (Length_m / 12) * (5 * pi * (Large_end_diameter_m / 2)^2 +
                                                               5 * pi * (Small_end_diameter_m / 2)^2 +
                                                               2 * sqrt((pi * (Large_end_diameter_m / 2)^2) * (pi * (Small_end_diameter_m / 2)^2))))
  
  # Apply specific gravity based on Decay_class (volume (m3)* density kg m3) to get a mass
  log_mass <- ifelse(plot_data$Decay_class == "S", log_volumes_m3 * 400, log_volumes_m3 * 300)
  
  # Calculate the total adjusted volume (in kg) for the current plot
  total_mass_kg <- sum(log_mass)
  
  # Calculate the area based on the plot's diameter
  plot_area_m2 <- if (plot_id == 302) {
    pi * (6.31^2)  # Special diameter for plot 302 (12.62 m diameter)
  } else {
    pi * (5^2)     # Standard diameter for other plots (10 m diameter)
  }
  
  # Scale the results to kg per m^2
  fuel_loadkgm2 <- total_mass_kg / plot_area_m2
  
  # Get the unique Site value for the current plot
  site_value <- unique(plot_data$Site)
  
  # Append the result to the x1000hr_results data frame
  x1000hr_results <- rbind(x1000hr_results, data.frame(Plot_id = plot_id, Mass_kg = total_mass_kg, x1000hr_kg_m2 = fuel_loadkgm2, Site = site_value))
}

summary_stats_1000 <- x1000hr_results %>%
  group_by(Site) %>%
  summarise(
    Mean_1000hr_kg_m2 = mean(`x1000hr_kg_m2`, na.rm = TRUE),
    SD_1000hr_kg_m2 = sd(`x1000hr_kg_m2`, na.rm = TRUE))

FTv_plot <- FTv_plot %>%
  left_join(x1000hr_results, by = "Plot_id")

####add 1000 hr results to master plot df
FTv_plot <- FTv_plot %>%
  left_join(dplyr::select(x1000hr_results, Plot_id, x1000hr_kg_m2), by = "Plot_id")


#############Litter and duff fuel loads ####################################

###default FFI Litter bulk density 44 kg/m3 (mass per unit volume)
##L (kg m2) = (V m3)*BD(kg m3)/A(m2).  Where L is loading for litter and duff, V is the volume of the litter+duff layer, BD is bulk density
##and A is the area of the microplot sampling frame (1m2). Volume is calculated following V=dA where d is the average depth (m) of the litter+duff
##layer wihtin the 10m2 plot and A is the area of the microplot (1m2)

#To account for correlated uncertainties, you would need to consider the covariance between depth measurements within the same plot. This involves 
#calculating the covariance matrix of the depth measurements and using it to estimate the uncertainty in the average depth (V) for each plot.
#This code uses var to calculate the variance of depth measurements within each plot and then computes the standard deviation (SD_V) based on the variance.

# Litter bulk density in kg/m³
BD <- 44

# Initialize empty vectors and dataframes to store fuel load values and SD
litter_fuel_load <- numeric()
litter_fuel_load_sd <- numeric()
litter_fuel_load_df <- data.frame(Plot_id = integer(), Site = character(), Litter_Fuel_Load_kg_m2 = numeric(), SD_Litter_Fuel_Load_kg_m2 = numeric())

# Get unique Plot_ids
unique_plot_ids <- unique(FTv_plot$Plot_id)

# Loop through each unique Plot_id
for (plot_id in unique_plot_ids) {
  # Subset the dataframe for the current Plot_id
  subset_df <- FTv_plot[FTv_plot$Plot_id == plot_id, ]
  
  # Get the depth measurements for the current Plot_id (converted from cm to m)
  litter_duff_depth_cm <- subset_df$litter_duff_depth_Mean
  litter_duff_depth_sd_cm <- subset_df$litter_duff_depth_StdDev
  
  # Convert depth data to meters
  litter_duff_depth_m <- litter_duff_depth_cm / 100
  
  # Calculate the volume (V) and its uncertainty (SD(V)) accounting for correlated uncertainties
  V <- mean(litter_duff_depth_m, na.rm = TRUE)
  SD_V <- sqrt(mean(litter_duff_depth_sd_cm^2)) / 100  # Convert SD from cm to m
  
  # Calculate the fuel load (L) and its uncertainty (SD(L))
  L <- V * BD
  SD_L <- SD_V * BD
  
  # Append the calculated fuel load and its uncertainty to the vectors
  litter_fuel_load <- c(litter_fuel_load, L)
  litter_fuel_load_sd <- c(litter_fuel_load_sd, SD_L)
  
  # Get the Site information for the current Plot_id
  site <- unique(subset_df$Site)
  
  # Create a dataframe with Plot_id, Site, Litter_Fuel_Load_kg_m2, and SD_Litter_Fuel_Load_kg_m2
  litter_fuel_load_row <- data.frame(Plot_id = plot_id, Site = site, Litter_Fuel_Load_kg_m2 = L, SD_Litter_Fuel_Load_kg_m2 = SD_L)
  
  # Append the row to the results dataframe
  litter_fuel_load_df <- rbind(litter_fuel_load_df, litter_fuel_load_row)
}

# Left join the dataframes by Plot_id
FTv_plot <- left_join(FTv_plot, select(litter_fuel_load_df, Plot_id, Litter_Fuel_Load_kg_m2, SD_Litter_Fuel_Load_kg_m2), by = "Plot_id")

summary_stats_litter_fuel <- litter_fuel_load_df %>%
  group_by(Site) %>%
  summarise(
    Mean_Litter_Fuel_Load_kg_m2 = mean(Litter_Fuel_Load_kg_m2, na.rm = TRUE),
    SD_Litter_Fuel_Load_kg_m2 = sd(Litter_Fuel_Load_kg_m2, na.rm = TRUE)
  )

print(summary_stats_litter_fuel)



###########Total fuel load############

FTv_plot <- FTv_plot %>%
  rowwise() %>%
  mutate(Total_fuel_load = sum(Litter_Fuel_Load_kg_m2, Photoload_100hr_Mean, Photoload_10hr_Mean, Photoload_1hr_Mean,x1000hr_kg_m2))


########NMDS plots#########

####Example code from Shealyn####

# t2 is complete df with treatment, plot, biomass, etc.
# t2VOC is df with JUST biomasses of functional groups

t2VOC <- t2[,colnms4]
dist.t2 <- vegdist(t2VOC, method='bray') #create dissimilarity matrix
perm.t2 <- adonis(dist.t2 ~ t2$treatment * t2$time, permutations = 10000) # PERMANOVA to look at effects of treatment
# on composition of functional groups.
perm.t2


#obtain stress value here, check stress plot (0.07)
# As a rule of thumb, NMDS ordination with a stress value above 0.2 is deemed
# suspect. Values approaching 0.3 indicate the ordination is arbitrary. Values
# equal or below 0.1 are considered fair, values at or below 0.05 indicate a
# good fit.
ord <- metaMDS(dist.t2, autotransform = FALSE, engine=c("monoMDS"))
ord
stressplot(ord)


# Make NMDS-------------------------------------------------------
ord2 <- metaMDS(dist.t2, autotransform = FALSE, engine = c('monoMDS')) #put in dissimilarity matrix

nmds_data2 <- data.frame(t2$plot, t2$treatment, ord2$points[,1], ord2$points[,2]) # plot = individual points on NDMS
# treatment = how points are grouped (color)
# ord points 1 are x axis values
# ord poitns 2 are y axis values
names(nmds_data2) <- c('plot', 'treatment', 'NMDS1', 'NMDS2')

#calculating the centroid value for the center of each NMDS group
nmds.mean2 <- aggregate(nmds_data2[c("NMDS1", "NMDS2")], 
                        list(group = nmds_data2$treatment), mean)

par(mar = c(4.1, 5.1, 1.1, 2))
#this command plots the single points on your NMDS
plot(nmds_data2$NMDS1, nmds_data2$NMDS2, pch = 21,
     bg = c('#56B4E9', '#CCCCCC','#E69F00','#D55E00')[as.numeric(nmds_data2$treatment)],
     cex = 4,
     cex.axis = 2.5,
     cex.lab=2.5,
     ylab = 'NMDS1',
     xlab = '',
     main = '',
     las = 1)

#this command plots the centroid
points(nmds.mean2$NMDS1, nmds.mean2$NMDS2, cex = 5,
       pch = 23, bg = c('#56B4E9', '#CCCCCC','#E69F00','#D55E00')
       [as.numeric((nmds.mean2$group))])

#this command puts on the ellipses
ordiellipse(ord2, paste(nmds_data2$treatment), kind = 'sd',
            draw = 'polygon',
            lwd = 1.5, )




############################Analysis and visualizaton########################

unique_count <- FTv_plot %>%
  dplyr::select(Habitat_type) %>%
  n_distinct()

####14 Habitat Types


### mean plot level basal area across units and comparing planned burn and control 

ggplot(FTv_plot, aes(x = Site, y = Total_BA,)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  labs(x = "Site", y = expression("Total Basal Area (kg"~m^2*")"), fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


ggplot(FTv_plot, aes(x = Site, y = mean_litter_duff_depth, fill = TX)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = "Litter + Duff Depth (cm)", fill = "Plot Type") +
  scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


ggplot(FTv_plot, aes(x = Site, y = mean_litter_duff_depth)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = "Litter + Duff Depth (cm)", fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


######Photoload#######

ggplot(FTv_plot, aes(x = Site, y = mean_1hr_PL)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = expression(" 1 hr photoload (kg"~m^2*")"), fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


ggplot(FTv_plot, aes(x = Site, y = mean_10hr_PL, fill = TX)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = expression(" 10 hr photoload (kg"~m^2*")"), fill = "Plot Type") +
  scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


ggplot(FTv_plot, aes(x = Site, y = mean_100hr_PL)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = expression(" 100 hr photoload (kg"~m^2*")"), fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()




BWC <- FTv_plot %>% filter(Site == "BWC")

mean(BWN$mean_litter_depth)


### basal area and fuel characteristics

ggplot(FTv_plot, aes(x = Total_BA, y = L)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(y = "LAI", x = "Total BA" , fill = "") +
  theme(axis.text=element_text(size=18, color = "black"),
        axis.title=element_text(size=18),
        axis.text.x=element_text(angle = 25, hjust = .8, vjust = 0.8),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "none",
        title = element_text(size=14))


##########Canopy cover

ggplot(FTv_plot, aes(x = Site, y = L)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = "LAI", fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()



#####Total Fuel load across sites

ggplot(FTv_plot, aes(x = Site.x, y = Total_fuel_load,)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  labs(x = "Site", y = expression("Total Fuel Load (kg"~m^2*")"), fill = "Plot Type") +
  #scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


ggplot(FTv_plot, aes(x = Site.x, y = Total_fuel_load, fill = TX)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +  # Remove outliers
  labs(x = "Site", y = expression(" Total Fuel Load (kg"~m^2*")"), fill = "Plot Type") +
  scale_fill_manual(values = c("Burn" = "red3", "Control" = "steelblue")) +
  theme_minimal()


##Filtering on the sites that are in the burn treatment

filtered_data <- FTv_plot %>%
  filter(TX == "Burn")

ggplot(filtered_data, aes(x = Site.x, y = Total_fuel_load)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  labs(x = "Site", y = expression("Total Fuel Load (kg" ~ m^2 * ")"), fill = "Plot Type") +
  theme_minimal()



#######Habitat Type######
ggplot(FTv_plot, aes(x = Habitat_type, y = Total_fuel_load)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  labs(x = "Habitat Type", y = expression("Total Fuel Load (kg" ~ m^2 * ")"), fill = "Plot Type") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) 




###Segmented bar chart for fuel loads


data_long <- pivot_longer(FTv_plot, cols = c(x1000hr_kg_m2, Litter_Fuel_Load_kg_m2, Photoload_100hr_Mean, Photoload_10hr_Mean, Photoload_1hr_Mean), 
                          names_to = "Fuel_load_component", values_to = "Total_Fuel_Load_kgm2")

data_long_burn <- data_long %>% filter(TX == "Burn")


ggplot(data_long, aes(x = Site.x, y = Total_Fuel_Load_kgm2, fill = Fuel_load_component)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = expression("Fuel Load (kg" ~ m^2 * ")")) +
  theme_minimal()


mean_data <- data_long_burn %>%
  group_by(Site.x, Fuel_load_component) %>%
  summarize(Mean_Value = mean(Total_Fuel_Load_kgm2))

mean_data$Fuel_load_component <- gsub("x1000hr_kg_m2", "1000 hr", mean_data$Fuel_load_component)
mean_data$Fuel_load_component <- gsub("Litter_Fuel_Load_kg_m2", "Litter", mean_data$Fuel_load_component)
mean_data$Fuel_load_component <- gsub("Photoload_1hr_Mean", "1 hr", mean_data$Fuel_load_component)
mean_data$Fuel_load_component <- gsub("Photoload_10hr_Mean", "10 hr", mean_data$Fuel_load_component)
mean_data$Fuel_load_component <- gsub("Photoload_100hr_Mean", "100 hr", mean_data$Fuel_load_component)


my_palette <- wes_palette("Darjeeling2")
my_palette <- brewer.pal(n = 10, name = "RdGy")[-1]

ggplot(mean_data, aes(x = Site.x, y = Mean_Value, fill = Fuel_load_component)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Site", y = expression("Fuel Load (kg" ~ m^2 * ")"), fill = "Fuel Load Component") +
  scale_fill_manual(values = my_palette) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) 



#######Water deficit#########

ggplot(FTv_plot, aes(x = deficit_, y = Total_fuel_load)) +
  geom_point(shape = 21, fill = "white", color = "black", size =2.5) +                    
  geom_smooth(method = "loess", se = TRUE, color = "red3", linetype = "solid") +
  labs(x = "Annual climatic water deficit (mm)", y = expression("Total plot fuel load (kg" ~ m^2 * ")")) + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20))


ggplot(data_long, aes(x = deficit_30yr_mean, y = Total_Fuel_Load_kgm2)) +
  geom_point(shape = 21, fill = "white", color = "black", size =2.5) +                    
  geom_smooth(method = "loess", se = TRUE, color = "red3", linetype = "solid") +
  labs(x = "Annual climatic water deficit (mm)", y = expression("Total plot fuel load (kg" ~ m^2 * ")")) + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) +
  facet_wrap(~Fuel_load_component)

data_long$Fuel_load_component <- gsub("x1000hr_kg_m2", "1000 hr", data_long$Fuel_load_component)
data_long$Fuel_load_component <- gsub("Litter_Fuel_Load_kg_m2", "Litter", data_long$Fuel_load_component)
data_long$Fuel_load_component <- gsub("Photoload_1hr_Mean", "1 hr", data_long$Fuel_load_component)
data_long$Fuel_load_component <- gsub("Photoload_10hr_Mean", "10 hr", data_long$Fuel_load_component)
data_long$Fuel_load_component <- gsub("Photoload_100hr_Mean", "100 hr", data_long$Fuel_load_component)


######Grouped bar chart for average percent cover by site by functional type########

# Define a color palette
my_palette <- brewer.pal(7, "Dark2")  # Change 5 to the number of functional types you have

# Create a grouped bar chart
ggplot(functional_type_summary, aes(x = Site, y = Average_Percent_Cover, fill = functional_type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = my_palette) +  # Use the defined color palette
  labs(x = "Site", y = "Average Plot Cover (%)", fill = "Plant Functional Type") +
  theme_minimal()




######Nitrogen fixer data, stacked bar chart  #########


my_palette <- wes_palette("Moonrise2")

ggplot(nitrogen_fixer_data, aes(x = Site, y = Average_Percent_Cover, fill = functional_type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Site", y = "Average plot cover N-fixers (%)", fill = "Plant Functional Type") +
  scale_fill_manual(values = my_palette) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) 





##########Total species abundance by Site##########

site_mean_se <- site_plot_total_counts %>%
  group_by(Site) %>%
  summarise(Mean_Total_Count = mean(Total_Count),
            SE_Total_Count = sd(Total_Count) / sqrt(n()))


# Create the bar graph with error bars
ggplot(site_mean_se, aes(x = Site, y = Mean_Total_Count, fill = Site)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", width = 0.5) +  # Add color and adjust width
  geom_errorbar(aes(ymax = Mean_Total_Count + SE_Total_Count,
                    ymin = Mean_Total_Count),
                width = 0.4, position = position_dodge(0.9)) +
  labs(x = "Site", y = "Species Abundance", fill = "Site") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.position = "none") 


#####Species abundnace by unique number of species

site_plot_mean_se <- site_plot_unique_species %>%
  group_by(Site) %>%
  summarise(Mean_Plot_Count = mean(Unique_Species_Count),
            SE_Plot_Count = sd(Unique_Species_Count) / sqrt(n()))


ggplot(site_plot_mean_se, aes(x = Site, y = Mean_Plot_Count, fill = Site)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", width = 0.5) +  # Add color and adjust width
  geom_errorbar(aes(ymax = Mean_Plot_Count + SE_Plot_Count,
                    ymin = Mean_Plot_Count),
                width = 0.4, position = position_dodge(0.9)) +
  scale_fill_manual(values = my_palette) +
  labs(x = "Site", y = "Species Richness", fill = "Site") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.position = "none") 

####Comparing abundance and richess across all of the plots 

my_palette <- brewer.pal(n = 9, name = "BrBG")

ggplot(abun_richness, aes(y = Unique_Species_Count, x = Total_Count, fill = Site.y)) +
  geom_point(shape = 21, color = "black", size = 3) +
  scale_fill_brewer(palette = "BrBG") +
  labs(x = "Understory species abundance", y = "Understory species richness", fill = "Site") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) 



#######Diversity plot by site

# Calculate the mean Shannon Diversity and SE by Site
site_shannon_summary <- plot_shannon_diversity %>%
  group_by(Site) %>%
  summarise(Mean_Shannon_Diversity = mean(Shannon_Diversity),
            SE_Shannon_Diversity = sd(Shannon_Diversity) / sqrt(n()))

# Create a bar plot with error bars
library(ggplot2)

ggplot(site_shannon_summary, aes(x = Site, y = Mean_Shannon_Diversity, fill = Site)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", width = 0.5) +
  geom_errorbar(aes(ymax = Mean_Shannon_Diversity + SE_Shannon_Diversity,
                    ymin = Mean_Shannon_Diversity),
                width = 0.4, position = position_dodge(0.9)) +
  labs(x = "Site", y = "Shannon Diversity Index ", fill = "Site") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        axis.text.x=element_text(color = "black"),
        axis.text.y=element_text(color = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size=20)) 





#### write new .csv with plot calculations
  write.csv(FTv_plot, '~/Dropbox/Fire Lab/Data/FTv_plot_v2.csv')




######Extra Code########
  
  long_data <- LPI_transect %>%
    pivot_longer(cols = starts_with("layer_"), 
                 names_to = "LPI point", 
                 values_to = "Species")
  
  long_data$Species <- trimws(long_data$Species)
  
  
  long_data$Species <- gsub("0", "NA", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Agrasca|Agrtra", "Agrsca", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ace gla", "Acegla", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Achaml|Achmel|Achmil|ACHMIL|Achmio|Ack mil|Ackmil", "Achmil", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Grass01|GR1|Grass", "Agrsca", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Agr spi|Agrispi|Agrospi|Agrspa|argspi", "Agrspi", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Allsim", "Allcer", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Alialn|Ame aln|Ame alm|ame alnn|Amealm|AMEALN|Amialn|Amealnn|Amyaln", "Amealn", long_data$Species,  ignore.case = TRUE)
  long_data$Species <- gsub("Ant rac|ANTARC|Antennaria sp|ANTRAC|ANTARC|AntRac", "Antrac", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Apo and", "Apoand", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Arc uva|ARCUVA|ArcUva|Arcuvq|Racuva", "Arcuva", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Armcor|Arn cor|ARNCOR|ArnCor", "Arncor", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb24|Forb19|Forb 19|Forb 24|Forb 13", "Artcam", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Artemesia spp 1|Artspp1|Forb27", "Artspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Astragalis spp|Astralagis spp|AstragALIS SPP|Astragalis SPP|Astralagus|Astralagus sp|Astralagus spp", "Astmis", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Bal sag|BALSAT", "Balsag", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ber rep|BERREP|Berrub", "Berrep", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Boachera pendulocarpa|Boe pen|Boapen|Boaspp|Forb 12", "Boepen", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grotec", "Brotec", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Bula spp", "Bulspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cal api|CalApi|Calapo|Colapi", "Calapi", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cal can", "Calcan", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cal rub|Calru|CALRUB|CalRub|Caltub|Carrub|carrub|CalrubB|Calrubb|Talrub", "Calrub", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Calochodus", "Calspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cam rot|CamRot", "Camrot", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Car con|Carcan|CARCON", "Carcon", long_data$Species)
  long_data$Species <- gsub("Calgey|Car gey|Carget|Cargry|CARGEY|CarGEy|CArgey|CARGEYA|CargeyA|cargey|Catgey", "Cargey", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cas min", "Casmin", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cea vel|Ceavil|Ceavul", "Ceavel", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Censto|CENSTO|Sen sto|S3nsto|Sensto", "Censto", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cha ang|Epi and|Epiang|Epilobium spp|Cha arg", "Chaang", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Chaduey", "Chadou", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Chelab", "Chealb", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Chi umb", "Chiumb", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb26", "Cleocc", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Col lin", "Collin", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Col par|COLPAR|Colpar", "Colpar", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("CREATR|Cr|Forb 18", "Creatr", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Cyn off", "Cynoff", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Dacgoo|DACGLO", "Dacglo", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Delphinium sp|Delvic", "Delbic", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ely gla", "Elygla", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Agrtra", "Elytra", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 15|Forb15", "Episax", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Eremogone congesta|Arecon", "Erecon", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Erigeron sp", "Ergspe", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grass02", "Erihym", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Erygea|ERYGRA|EryGra", "Erygra", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Eur con|Eurcon|Forb25|Forb17", "Eurcon", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Fes ida|Fesdida|FESIDA|Fesidas|Idafes", "Fesida", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grass 8|Fes occ", "Fesocc", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb28", "Filarv", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 16", "Forb16", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 3", "Forb3", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forbe29", "Forb29", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forbe 53", "Forb5", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Forbe 4", "Forb4", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Fra vir|Fra ver|Fraver|FRAVIR|FraVir|Farves|Fra ves|FRAVES|Fravest", "Fravir", long_data$Species, ignore.case = TRUE )
  long_data$Species <- gsub("G", "NA", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Calbor", "Galbor", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Gayophytum humile", "Gayhum", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Geranium sp", "Gervis", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Goo obl|Gooobl|Groobl", "Goobl", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grass", "grass6", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grass 7|Unknown Grass_7|Unknown Grass7|Unknown grass 7", "grass7", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Grass 8|Unknown Grass 8", "grass8", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Hed occ", "Hedocc", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Hetvik|Hetvill", "Hetvil", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("HeuCyl", "Heucyl", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Hie alb|Hie ale|Hie ale|Hie aln|Hie sp|HIERACIUM|Hiealb sp. Native|Hieracium sp", "Hiealb", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Hie sco|Hie sp1|Hieracium sp native|Hiearacium sp. (native)|Hieracium sp. Native|Piesco", "Hiesco", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Jnsco", "Junsco", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("GR3|Grass03", "Koemac", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("L", "NA", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Latcer", "Lacer", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lin bor", "Linbor", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lit rud", "Litrud", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Litter", "NA", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 11", "Litrud", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lomcus", "Lomcou", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lom spp|Lomspp|Lummol", "Lommul", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lomatium triternatum|Lom tri|LOMTRI", "Lomtri", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("LONCIL|Lonsil", "Loncil", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Lupcer|Lupser|Leparg|Lup arg|luparg|LUPARG", "Luparg", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Luzcam|Luz cam", "Luzmul", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Mai rac|Smiste|Smirac|Smilacina spp|Smi rac|SMIRAC|Smilacina sp", "Mairac", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Medlap|Medlep", "Medlup", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Melofff|Mellof|Melloff", "Meloff", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("MICGRA", "Micgra", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 14|Forb14", "Mendis", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("MonLin", "Monlin", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Nn", "N", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Osm ber|OSMBER|Osmorhiza sp|Osmorhiza spp", "Osmber", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ozosta|Forb53", "Mitsta", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Patcan", "Paccan", long_data$Species)
  long_data$Species <- gsub("Ped sp|Pen sp|Pensemmon sp|Penspp|Penstemen spp|Penstemon sp|Penstemon spp|Penstemon sp.|Pensppp|Penstemonspp", "Penspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Penstemmon sp 1", "Pensp1", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 22", "Pensp2", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Penstemmon sp 2", "Pensp3", long_data$Species,ignore.case = TRUE)
  long_data$Species <- gsub("Penstemon spp 3", "Penlya", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Pen wil|Pen will|Penwile|Penstemmon sp", "Penwil", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb20", "Phahas", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Forb 21|Forb21", "Phlspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("PHLPRA", "Phlpra", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Phypre|Phlpre|Phg mal|Phy mal|Phy malN|PhymalN|Phyaml|PHYMAL|Phymao|Pphymal", "Phymal", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Pipuna1|Plauna", "Pipuna", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("PoaCom", "Poacom", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Poa sec|Poa secunda|Grass 1|Poasecunda|Grass4", "Poasec", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Pruver", "Pruvir", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("PSME|Pisme|Psmi", "Psme", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ribes spp|Rubaur", "Ribaur", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ros woo|ROSWOO" , "Roswoo", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Sal sco|Salasco", "Salsco", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Senecio so|Senecio spp", "Senspp", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("She can", "Shecan", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("SILMEN|SiMen|SilNen", "Silmen", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Smi ste", "Smiste", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Smyfol|Symfol maybe lavae", "Symfol", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Sorbis spp", "Sorsco", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Spibec|Soi bet|Spi bet|SpiBet|Spubet|SPIBET|Spirub", "Spibet", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Symalb alb|Symalbalb|Symalbalb|Cymalb|SIMALB|Sym|Sym alb|SYMALB|SymAlb|Symalblab|Symalbfol|SymalbALB|SymalbAlb|SYMalb|Symaln|Symlab|Symalb alb|SymLab", "Symalb", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Sym lae", "Symlae", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("TAROFF|Teroff", "Taroff", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Tha occ|THAOC|THAOCC|ThaoccC|Thaoccc|Thocc|Theocc", "Thaocc", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Tri can|TRICAN|TRISET", "Trican", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Vac glo|VACGLO|Vavglo|Vacmen|Vacmem", "Vacglo", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Ver tha|Verthw", "Vertha", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Vicamep|Vicaspp|Vicea spp|Vicia sp|Vicia spp|Viciaspp", "Vicame", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("VIOADU", "Vioadu", long_data$Species, ignore.case = TRUE)
  long_data$Species <- gsub("Viola sp|Viola sp.|Viosppp", "Viospp", long_data$Species, ignore.case = TRUE)


