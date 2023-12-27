library(terra)
library(dplyr)
library(purrr)

# Define  paths
base_path <- "path"
output_path <- "path"

# Define the file names and their respective variable names
file_names <- c("NDVI", "PSRI", "RDVI", "GNDVI", "WDRVI", "MSAVI2", "VDVI", "RVI", "EVI", "STVI")
file_paths <- paste0(base_path, file_names, ".tif")

# Load raster and polygon layers
polygons <- "path/file.shp"
v <- vect(polygons)

# Function to load rasters
load_rasters <- function(paths) {
  map(paths, ~rast(.x))
}

# Function to extract and calculate weighted mean
calculate_mean <- function(rasters, v) {
  map(rasters, ~{
    extracted <- terra::extract(.x, v, weight = TRUE, touches = TRUE)
    results <- extracted %>%
      group_by(ID) %>%
      summarize(weighted_mean = sum(get(names(.x)) * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE), .groups = 'drop')
    colnames(results) <- c("ID", paste0("weighted_mean_", tolower(names(.x))))
    results
  })
}

# Function to calculate median and rename columns
calculate_median <- function(rasters, v) {
  map(rasters, ~{
    median_result <- terra::extract(.x, v, fun = median, na.rm = TRUE)
    colnames(median_result) <- c("ID", paste0("median_", tolower(names(.x))))
    median_result
  })
}

# Function to calculate stand deviation and rename columns
calculate_sd <- function(rasters, v) {
  map(rasters, ~{
    sd_result <- terra::extract(.x, v, fun = sd, na.rm = TRUE)
    colnames(sd_result) <- c("ID", paste0("sd_", tolower(names(.x))))
    sd_result
  })
}

# Load rasters
rasters <- load_rasters(file_paths)

# Calculate results
results_weighted_mean <- calculate_mean(rasters, v)
results_median <- calculate_median(rasters, v)
results_sd <-calculate_sd(rasters, v)


# Combine the results

# Create a new list that groups corresponding data frames together
list_of_dfs <- list(results_median, results_weighted_mean, results_sd)

combined_results <- pmap(list_of_dfs, function(df1, df2, df3) {
  joined_df <- full_join(df1, df2, by = "ID") %>%
    full_join(df3, by = "ID")
  
})

# Write to CSV
write.csv(combined_results, file = paste0(output_path, "weighted_mean_medain_sd_results.csv"), row.names = FALSE)
