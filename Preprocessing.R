# ===============================================================
#        Theta Frequency Modulation During Visual Working Memory
#
#                     PRE-PROCESSING PIPELINE
#
#                     K. Hendriks (Mar 2025)
# ===============================================================


# ------------------------------------------------------------
#               STEP 1 — INITIAL SETUP
# ------------------------------------------------------------

rm(list = ls())             # Clear all objects from the environment
gc()                        # Free up unused memory


# Load required libraries
library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(afex)
library(car)
library(ggsignif)
library(patchwork)
library(emmeans)
library(tidyverse)
library(reshape2)
library(ez)

# Check versions of loaded packages
packageVersion("purrr")
packageVersion("stringr")
packageVersion("dplyr")
packageVersion("tidyr")
packageVersion("readxl")
packageVersion("ggplot2")
packageVersion("rstatix")
packageVersion("ggpubr")
packageVersion("afex")
packageVersion("car")
packageVersion("ggsignif")
packageVersion("patchwork")
packageVersion("emmeans")
packageVersion("tidyverse")
packageVersion("reshape2")
packageVersion("ez")


# ------------------------------------------------------------
#               STEP 2 — DATA PROCESSING NOTES
# ------------------------------------------------------------

# - Loop through each subject file and reshape the data
# - Replace commas with dots in numeric values
# - Convert µV² to dBµV² using 10*log10(value)
# - Average Cz, Fz, and Pz for each condition/layer
# - Define frequency mapping for each layer
# - Isolate peak theta/alpha frequency per phase


# ------------------------------------------------------------
#               STEP 3 — IMPORT AND RESTRUCTURE DATA
# ------------------------------------------------------------

main_directory <- # * set your data directory here
  
  subject_dirs <- list.dirs(main_directory, recursive = FALSE)  # Get subject folders


# Function: Process all wavelet files in a subject folder
process_subject <- function(subject_dir) {
  subject_id <- basename(subject_dir)  # Extract subject ID
  
  file_list <- list.files(
    path = subject_dir,
    pattern = "WaveletDataExport_\\d+_layer\\d+_\\w+\\.csv$",
    full.names = TRUE
  )
  
  if (length(file_list) == 0) {
    warning(paste("No CSV files found for subject:", subject_id))
    return(NULL)
  }
  
  processed_data <- map(file_list, process_wavelet_data) %>% compact()
  
  if (length(processed_data) == 0) {
    warning(paste("No valid data found for subject:", subject_id))
    return(NULL)
  }
  
  subject_data <- reduce(processed_data, full_join, by = "File")
  
  long_data <- subject_data %>%
    pivot_longer(
      cols = -File,
      names_to = c("Electrode", "Layer", "Condition"),
      names_pattern = "(Cz|Pz|Fz)-layer(\\d{1,2})_(encoding|er|lr|retr)",
      values_to = "Value"
    )
  
  wide_data <- long_data %>%
    pivot_wider(
      names_from = c("Electrode", "Layer", "Condition"),
      names_sep = "-",
      values_from = Value
    )
  
  integrated_data <- wide_data %>%
    summarise(across(everything(), ~ first(na.omit(.))))
  
  integrated_data <- integrated_data %>%
    mutate(File = subject_id) %>%
    select(File, everything())
  
  return(integrated_data)
}


# Function: Process individual wavelet file
process_wavelet_data <- function(file_path) {
  file_name <- basename(file_path) %>% str_remove(".csv$")
  match <- str_match(file_name, "WaveletDataExport_\\d+_(layer\\d+)_(\\w+)")
  
  if (is.na(match[,2]) | is.na(match[,3])) {
    warning(paste("Skipping file due to incorrect naming pattern:", file_name))
    return(NULL)
  }
  
  layer <- match[,2]
  phase <- match[,3]
  
  data <- tryCatch({
    read.csv(file_path, skip = 1, header = TRUE, check.names = FALSE)
  }, error = function(e) {
    warning(paste("Error reading file:", file_path))
    return(NULL)
  })
  
  if (is.null(data) || ncol(data) < 4) {
    warning(paste("Skipping file due to unexpected format:", file_name))
    return(NULL)
  }
  
  colnames(data) <- c("Segment",
                      paste0("Cz-", layer, "_", phase),
                      paste0("Fz-", layer, "_", phase),
                      paste0("Pz-", layer, "_", phase))
  
  data <- data %>% select(-Segment) %>%
    mutate(File = file_name) %>%
    select(File, everything())
  
  return(data)
}


# Merge all subject data
final_dataset <- map_dfr(subject_dirs, process_subject)

# Save raw merged dataset
save_path <- file.path(main_directory, "final_dataset.csv")
write.csv(final_dataset, save_path, row.names = FALSE)


# ------------------------------------------------------------
#               STEP 4 — DATA CLEANING & TRANSFORM
# ------------------------------------------------------------

# Replace commas, convert to numeric
final_dataset_cleaned <- final_dataset %>%
  mutate(across(-File, ~ as.numeric(str_replace(., ",", "."))))

# Save cleaned dataset
save_path_cleaned <- file.path(main_directory, "final_dataset_cleaned.csv")
write.csv(final_dataset_cleaned, save_path_cleaned, row.names = FALSE)


# Convert µV² to dBµV²
final_dataset_log <- final_dataset_cleaned %>%
  mutate(across(-File, ~ ifelse(. > 0, 10 * log10(.), NA)))

# Save dB-transformed data
save_path_log <- file.path(main_directory, "final_dataset_log.csv")
write.csv(final_dataset_log, save_path_log, row.names = FALSE)


# ------------------------------------------------------------
#               STEP 5 — EXTRACT MAX POWER VALUES
# ------------------------------------------------------------

# Reload log-transformed dataset if needed
main_directory <- # * set your directory
  final_dataset_log <- read.csv(file.path(main_directory, "final_dataset_log.csv"))

layers_group_1 <- 1:13
layers_group_2 <- 14:18
electrodes <- c("Fz", "Cz", "Pz")
phases <- c("encoding", "er", "lr", "retr")

get_max_for_group <- function(layer_group) {
  result <- list()
  for (electrode in electrodes) {
    for (phase in phases) {
      pattern <- paste0("^", electrode, "\\.(", paste(layer_group, collapse = "|"), ")\\.", phase, "$")
      matching_cols <- grep(pattern, colnames(final_dataset_log), value = TRUE)
      if (length(matching_cols) > 0) {
        max_val <- max(final_dataset_log[, matching_cols], na.rm = TRUE)
        result[[paste(electrode, phase, sep = "_")]] <- max_val
      }
    }
  }
  return(result)
}

max_values_group_1 <- get_max_for_group(layers_group_1)
max_values_group_2 <- get_max_for_group(layers_group_2)

max_df <- rbind(
  Layer_1_13 = unlist(max_values_group_1),
  Layer_14_18 = unlist(max_values_group_2)
)

print(max_df)


# ------------------------------------------------------------
#               STEP 6 — AVERAGE PER ELECTRODE GROUP
# ------------------------------------------------------------

extract_layer_group_long <- function(final_dataset_log, layer_group, group_label) {
  electrodes <- c("Fz", "Cz", "Pz")
  phases <- c("encoding", "er", "lr", "retr")
  final_dataset_log$Subject <- factor(1:nrow(final_dataset_log))  # Add subject index
  
  long_data <- data.frame()
  
  for (electrode in electrodes) {
    for (phase in phases) {
      pattern <- paste0("^", electrode, "\\.(", paste(layer_group, collapse = "|"), ")\\.", phase, "$")
      cols <- grep(pattern, colnames(final_dataset_log), value = TRUE)
      if (length(cols) > 0) {
        avg_values <- rowMeans(final_dataset_log[, cols], na.rm = TRUE)
        temp <- data.frame(
          Subject = final_dataset_log$Subject,
          Electrode = electrode,
          Phase = phase,
          LayerGroup = group_label,
          Value = avg_values
        )
        long_data <- rbind(long_data, temp)
      }
    }
  }
  return(long_data)
}

df_layer_1_13 <- extract_layer_group_long(final_dataset_log, 1:13, "Layer_1_13")
df_layer_14_18 <- extract_layer_group_long(final_dataset_log, 14:18, "Layer_14_18")

long_df <- rbind(df_layer_1_13, df_layer_14_18)

long_df$Subject <- factor(long_df$Subject)
long_df$Electrode <- factor(long_df$Electrode)
long_df$Phase <- factor(long_df$Phase)
long_df$LayerGroup <- factor(long_df$LayerGroup)


# ------------------------------------------------------------
#               STEP 7 — AVERAGE ACROSS ELECTRODES
# ------------------------------------------------------------

final_dataset_avg <- final_dataset_log %>%
  pivot_longer(
    cols = -File,
    names_to = c("Electrode", "Layer", "Condition"),
    names_pattern = "(Cz|Fz|Pz)-(\\d+)-(encoding|er|lr|retr)",
    values_to = "Value"
  ) %>%
  group_by(File, Layer, Condition) %>%
  summarise(avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c("Layer", "Condition"),
    names_sep = "-",
    values_from = avg
  )

save_path_avg <- file.path(main_directory, "final_dataset_avg.csv")
write.csv(final_dataset_avg, save_path_avg, row.names = FALSE)


# ------------------------------------------------------------
#               STEP 8 — ADD FREQUENCY MAPPING
# ------------------------------------------------------------

layer_frequencies <- tibble(
  Layer = 1:18,
  Frequency = c(3.000, 3.270, 3.565, 3.886, 4.236, 4.618, 5.034, 5.487, 5.981, 6.520, 
                7.108, 7.748, 8.446, 9.207, 10.036, 10.940, 11.926, 13.000)
)

final_dataset_avg_with_frequencies <- final_dataset_avg %>%
  pivot_longer(
    cols = -File,
    names_to = c("Layer", "Condition"),
    names_sep = "-",
    values_to = "Power"
  ) %>%
  mutate(Layer = as.numeric(Layer)) %>%
  left_join(layer_frequencies, by = "Layer") %>%
  arrange(File, Layer) %>%
  pivot_wider(
    names_from = Condition,
    values_from = Power,
    names_glue = "Power_{Condition}"
  ) %>%
  relocate(Frequency, .after = Layer)

save_path_avg_with_frequencies <- file.path(main_directory, "final_dataset_avg_with_frequencies.csv")
write.csv(final_dataset_avg_with_frequencies, save_path_avg_with_frequencies, row.names = FALSE)


# ------------------------------------------------------------
#               STEP 9 — THETA ANALYSIS
# ------------------------------------------------------------

final_dataset_theta <- final_dataset_avg_with_frequencies %>%
  filter(Layer >= 1 & Layer <= 13)

save_path_theta <- file.path(main_directory, "final_dataset_theta.csv")
write.csv(final_dataset_theta, save_path_theta, row.names = FALSE)

View(final_dataset_theta)

long_theta <- final_dataset_theta %>%
  pivot_longer(
    cols = starts_with("Power_"),
    names_to = "Condition",
    names_prefix = "Power_",
    values_to = "Power"
  )

final_dataset_max_theta <- long_theta %>%
  group_by(File, Condition) %>%
  slice_max(order_by = Power, with_ties = FALSE) %>%
  ungroup() %>%
  select(File, Condition, Power, Frequency)

save_path_max_theta <- file.path(main_directory, "final_dataset_max_theta.csv")
write.csv(final_dataset_max_theta, save_path_max_theta, row.names = FALSE)


# ------------------------------------------------------------
#               STEP 10 — ALPHA ANALYSIS
# ------------------------------------------------------------

main_directory <- # * set your directory
  
  final_dataset_avg_with_frequencies <- read.csv(file.path(main_directory, "final_dataset_avg_with_frequencies.csv"))

final_dataset_alpha <- final_dataset_avg_with_frequencies %>%
  filter(Layer >= 14 & Layer <= 18)

save_path_alpha <- file.path(main_directory, "final_dataset_alpha.csv")
write.csv(final_dataset_alpha, save_path_alpha, row.names = FALSE)

View(final_dataset_alpha)

long_alpha <- final_dataset_alpha %>%
  pivot_longer(
    cols = starts_with("Power_"),
    names_to = "Condition",
    names_prefix = "Power_",
    values_to = "Power"
  )

final_dataset_max_alpha <- long_alpha %>%
  group_by(File, Condition) %>%
  slice_max(order_by = Power, with_ties = FALSE) %>%
  ungroup() %>%
  select(File, Condition, Power, Frequency)

save_path_max_alpha <- file.path(main_directory, "final_dataset_max_alpha.csv")
write.csv(final_dataset_max_alpha, save_path_max_alpha, row.names = FALSE)
