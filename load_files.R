# ### aggregated aoi combined ###
# library(tidyverse)
# library(readr)
# files <- list.files(path = "C:/Users/Kuba/Desktop/dane do analizy magisterka/mgr/", 
#                     pattern = "AggregatedAOImetrics.csv", 
#                     full.names = TRUE, 
#                     recursive = TRUE)
# 
# files <- sort(files)
# files
# 
# combined_data <- read_csv(files[1])
# 
# col_names <- names(combined_data)
# 
# col_types <- spec(combined_data)$cols
# 
# for (file in files[-1]) {
#   temp_data <- read_csv(
#     file,
#     skip = 9,
#     col_names = col_names,
#     col_types = col_types,
#     show_col_types = FALSE
#   )
#   combined_data <- bind_rows(combined_data, temp_data)
#   # if (nrow(temp_data) > 0) {
#   #   combined_data <- bind_rows(combined_data, temp_data)
#   # } else {
#   #   message("Skipped empty or malformed file: ", file)
#   # }
# }
# 
# 
# ### fixation table combined ###
# files <- list.files(path = "C:/Users/Kuba/Desktop/dane do analizy magisterka/mgr/", 
#                     pattern = "FixationTable.csv", 
#                     full.names = TRUE, 
#                     recursive = TRUE)
# 
# files <- sort(files)
# files
# 
# combined_data <- read_csv(files[1])
# 
# col_names <- names(combined_data)
# 
# col_types <- spec(combined_data)$cols
# 
# for (file in files[-1]) {
#   temp_data <- read_csv(
#     file,
#     skip = 7,
#     col_names = col_names,
#     col_types = col_types,
#     show_col_types = FALSE
#   )
#   combined_data <- bind_rows(combined_data, temp_data)
# }

############### organized, make sure all files are being able to be combined this way


# ---- Load libraries ----
library(tidyverse)
library(readr)

# ---- Define helper function ----
combine_csv_files <- function(files, skip, col_names = NULL, col_types = NULL) {
  files <- sort(files)
  first_file <- read_csv(files[1], show_col_types = FALSE)
  
  if (is.null(col_names)) col_names <- names(first_file)
  if (is.null(col_types)) col_types <- spec(first_file)$cols
  
  combined <- first_file
  
  for (file in files[-1]) {
    temp <- read_csv(file, skip = skip, col_names = col_names, col_types = col_types, show_col_types = FALSE)
    if (nrow(temp) > 0) {
      combined <- bind_rows(combined, temp)
    } else {
      message("Skipped empty or malformed file: ", file)
    }
  }
  return(combined)
}

# ---- Set file path ----
base_path <- "./data"

# ---- Aggregated AOI Combined ----
aoi_files <- list.files(path = base_path, pattern = "AggregatedAOImetrics.csv", full.names = TRUE, recursive = TRUE)
combined_aoi <- combine_csv_files(aoi_files, skip = 9)

# ---- Fixation Table Combined ----
fix_files <- list.files(path = base_path, pattern = "FixationTable.csv", full.names = TRUE, recursive = TRUE)
combined_fix <- combine_csv_files(fix_files, skip = 7)


# ---- Individual  AOI Combined ----
indiv_aoi_files <- list.files(path = base_path, pattern = "IndividualAOImetrics.csv", full.names = TRUE, recursive = TRUE)
combined_indiv_aoi <- combine_csv_files(indiv_aoi_files, skip = 8)

# ---- Saccade Table Combined ----
saccade_files <- list.files(path = base_path, pattern = "SaccadeTable.csv", full.names = TRUE, recursive = TRUE)
combined_saccade <- combine_csv_files(saccade_files, skip = 7)

# check the first file if necessary
first_file <- read_csv(saccade_files[1], show_col_types = FALSE)

if (!dir.exists("data-clean")) dir.create("data-clean")

write_csv(combined_aoi, "data-clean/combined_aoi.csv")
write_csv(combined_fix, "data-clean/combined_fix.csv")
write_csv(combined_indiv_aoi, "data-clean/combined_indiv_aoi.csv")
write_csv(combined_saccade, "data-clean/combined_saccade.csv")
