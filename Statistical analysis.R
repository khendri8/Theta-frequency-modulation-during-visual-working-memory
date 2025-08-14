# ==============================================================================
#        Theta Frequency Modulation During Visual Working Memory
#
#                        ANALYSIS
#
#                     K. Hendriks (Mar 2025)
# ==============================================================================
# Script Author: K. Hendriks
# Date: March 2025
# ------------------------------------------------------------------------------


# ==============================================================================
#                                SCRIPT OVERVIEW
# ==============================================================================

# Extra information: 
# * Change directory/file accordingly
# Use the following datasets created using the Preprocessing.R script:
# --- EEG_readouts_theta                      <- this is the dataset of all participants with maximum theta power and ITF peaks
# --- EEG_readouts_alpha                      <- this is the dataset of all participants with maximum alpha power and IAF peaks

# Use the following dataset loaded later in the script
# --- data_ccdt                               <- this is the dprime readouts file

# Created later in the script
# --- final_dataset_no_8446_anywhere          <- this is the filtered dataset without the participants with undetectable ITF peaks
# --- merged_data_filtered                    <- this is the dataset without outliers
# --- merged_data_n20                         <- this is the dataset with participants with detectable ITF peaks during encoding and response


# ==============================================================================
#                                SCRIPT OVERVIEW
# ==============================================================================

# 1. Descriptives of theta and alpha
# 2. Plotting RQ1 (theta and alpha)
# 3. Statistical analysis - RQ1 (theta and alpha)
# 4. Statistical analysis - RQ2 (theta and alpha related to d-prime)
# 5. Plotting RQ2 - Theta related to d-prime

# ==============================================================================
#                                 LOAD PACKAGES
# ==============================================================================

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

# Version packages
packages <- c(
  "purrr", "stringr", "dplyr", "tidyr", "readxl", "ggplot2",
  "rstatix", "ggpubr", "afex", "car", "ggsignif", "patchwork",
  "emmeans", "tidyverse", "reshape2", "ez"
)

sapply(packages, packageVersion)


# ==============================================================================
#                                 DESCRIPTIVES
# ==============================================================================


# Define the main directory
main_directory <- # * Change directory/file accordingly

# Define the dataset file path
EEG_readouts_theta <- file.path(main_directory, "EEG_readouts_theta.csv")

# Load the dataset
EEG_readouts_theta<- read.csv(EEG_readouts_theta)

# Full dataset - theta
descriptives_all <- EEG_readouts_theta %>%
  group_by(Condition) %>%
  summarise(
    mean_frequency = mean(Frequency, na.rm = TRUE),
    median_frequency = median(Frequency, na.rm = TRUE),
    sd_frequency = sd(Frequency, na.rm = TRUE),
    mean_power = mean(Power, na.rm = TRUE),
    median_power = median(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    n = n()
  )

print(descriptives_all)

# Subset dataset (loaded later in script) - theta

descriptives_all <- final_dataset_no_8446_rows %>%
  group_by(Condition) %>%
  summarise(
    mean_frequency = mean(Frequency, na.rm = TRUE),
    median_frequency = median(Frequency, na.rm = TRUE),
    sd_frequency = sd(Frequency, na.rm = TRUE),
    mean_power = mean(Power, na.rm = TRUE),
    median_power = median(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    n = n()
  )

print(descriptives_all)

# Full dataset - alpha

# Define the dataset file path
EEG_readouts_alpha <- file.path(main_directory, "EEG_readouts_alpha.csv")

# Load the dataset
EEG_readouts_alpha <- read.csv(EEG_readouts_alpha)

descriptives_all <- EEG_readouts_alpha %>%
  group_by(Condition) %>%
  summarise(
    mean_frequency = mean(Frequency, na.rm = TRUE),
    median_frequency = median(Frequency, na.rm = TRUE),
    sd_frequency = sd(Frequency, na.rm = TRUE),
    mean_power = mean(Power, na.rm = TRUE),
    median_power = median(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    n = n()
  )

print(descriptives_all)

# ==============================================================================
#                               PLOTTING - RQ1
# ==============================================================================


# Define the main directory
main_directory <- # * set your directory

# Define the dataset file path
EEG_readouts_theta <- file.path(main_directory, "EEG_readouts_theta.csv")

# Load the dataset
EEG_readouts_theta <- read.csv(EEG_readouts_theta)

# Theta frequency - violin plot

ggplot(EEG_readouts_theta, aes(x = Condition, y = Frequency)) +
  # Violin plot with soft fill and transparency
  geom_violin(fill = "#cce5ff", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2)+
  
  # Boxplot within violin
  geom_boxplot(width = 0.15, outlier.shape = NA, 
               fill = "#99ccff", color = "#3399ff", linewidth = 0.5, alpha = 0.85) +
  
  # Individual subject points (jittered)
  geom_jitter(position = position_jitter(width = 0.15), size = 2, alpha = 0.7, color = "dodgerblue") +
  
  # Axis formatting
  scale_y_continuous(
    breaks = seq(2.0, 8.5, 1.0),
    limits = c(2.5, 8.6),
    expand = c(0, 0)
  ) +
  scale_x_discrete(labels = c(
    "encoding" = "encoding",
    "er" = "early retention",
    "lr" = "late retention",
    "response" = "response"
  )) +
  
  # Theme customization
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


# Theta frequency - violin plot (N=13)

# Define the dataset file path
final_dataset_exclude_8446_participants <- file.path(main_directory, "final_dataset_exclude_8446_participants.csv")

# Load the dataset
final_dataset_exclude_8446_participants <- read.csv(final_dataset_exclude_8446_participants)

ggplot(final_dataset_exclude_8446_participants, aes(x = Condition, y = Frequency)) +
  # Violin plot with soft fill and transparency
  geom_violin(fill = "#cce5ff", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  
  # Boxplot within violin
  geom_boxplot(width = 0.15, outlier.shape = NA, 
               fill = "#99ccff", color = "#3399ff", linewidth = 0.5, alpha = 0.85) +
  
  # Individual subject points (jittered)
  geom_jitter(position = position_jitter(width = 0.15), size = 2, alpha = 0.7, color = "dodgerblue") +
  
  # Significance annotations
  geom_signif(comparisons = list(c("encoding","er"),c("encoding", "response"),c("er","lr"),c("er","response")),
              annotations = c("*"," "," "," "),
              y_position = c(8.05,7.9,7.75,7.6),  # adjust based on your y-axis
              tip_length = 0.01,
              size = 1,
              textsize = 8) +
  
  # Axis formatting
  scale_y_continuous(
    breaks = seq(2.0, 8.5, 1.0),
    limits = c(2.5, 8.5),
    expand = c(0, 0)
  ) +
  scale_x_discrete(labels = c(
    "encoding" = "encoding",
    "er" = "early retention",
    "lr" = "late retention",
    "response" = "response"
  )) +
  
  # Theme customization
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )



######################################


# Theta power - violin plot

ggplot(EEG_readouts_theta, aes(x = Condition, y = Power)) +
  # Violin plot with warm orange tone
  geom_violin(
    fill = "#ffdab3",  # Light peach
    color = NA,
    alpha = 0.6,
    trim = FALSE,
    width = 0.9,
    adjust = 1.2
  ) +
  
  # Boxplot within violin
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    fill = "#ffb84d",      # Soft orange
    color = "#e67300",     # Stronger outline orange
    linewidth = 0.5,
    alpha = 0.85
  ) +
  
  # Jittered individual data points
  geom_jitter(
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.7,
    color = "tan1"
  ) +
  
  # Significance annotations
  geom_signif(comparisons = list(c("encoding","er"),c("encoding","lr"),c("lr","response"),c("er","response")),
              annotations = c("***"," "," ","**"),
              y_position = c(32.0,31.8,31.8,31.4),  # adjust based on your y-axis
              tip_length = 0.01,
              size = 1,
              textsize = 8) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(26.0, 32.0, 1),
    limits = c(26.0, 32.5),
    expand = c(0, 0)
  ) +
  
  # X-axis labels
  scale_x_discrete(labels = c(
    "encoding" = "encoding",
    "er" = "early retention",
    "lr" = "late retention",
    "response" = "response"
  )) +
  
  # Theme
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


###############

# PLOT TOGETHER FOR MANUSCRIPT: THETA RQ 1


# ---- Plot A: Theta Power ----
plot_a <- ggplot(EEG_readouts_theta, aes(x = Condition, y = Power)) +
  geom_violin(fill = "#ffdab3", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#ffb84d", color = "#e67300", linewidth = 0.5, alpha = 0.85) +
  geom_jitter(position = position_jitter(width = 0.15), size = 1.5, alpha = 0.7, color = "tan1") +
  geom_signif(
    comparisons = list(c("encoding","er"), c("encoding","lr"), c("lr","response"), c("er","response")),
    annotations = c("***", " ", " ", "**"),
    y_position = c(32.0, 31.8, 31.8, 31.1),
    tip_length = 0.015,
    size = 0.8,       # Increased bracket line width
    textsize = 7      # Larger asterisk text
  ) +
  scale_y_continuous(breaks = seq(26.0, 32.0, 1), limits = c(26.0, 32.8), expand = c(0, 0)) +
  scale_x_discrete(labels = c("encoding" = "encoding", "er" = "early retention", "lr" = "late retention", "response" = "response")) +
  labs(y = "Theta power (dBµV²)", tag = "A") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),   # Smaller x-axis text
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# ---- Plot B: ITF (Full Sample) ----
plot_b <- ggplot(EEG_readouts_theta, aes(x = Condition, y = Frequency)) +
  geom_violin(fill = "#cce5ff", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#99ccff", color = "#3399ff", linewidth = 0.5, alpha = 0.85) +
  geom_jitter(position = position_jitter(width = 0.15), size = 1.5, alpha = 0.7, color = "dodgerblue") +
  scale_y_continuous(breaks = seq(2.0, 8.5, 1.0), limits = c(2.5, 8.8), expand = c(0, 0)) +
  scale_x_discrete(labels = c("encoding" = "encoding", "er" = "early retention", "lr" = "late retention", "response" = "response")) +
  labs(y = "ITF (Hz)", tag = "B") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# ---- Load N=13 dataset ----
final_dataset_exclude_8446_participants <- read.csv(file.path(main_directory, "final_dataset_exclude_8446_participants.csv"))

# ---- Plot C: ITF (N = 13) ----
plot_c <- ggplot(final_dataset_exclude_8446_participants, aes(x = Condition, y = Frequency)) +
  geom_violin(fill = "#cce5ff", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#99ccff", color = "#3399ff", linewidth = 0.5, alpha = 0.85) +
  geom_jitter(position = position_jitter(width = 0.15), size = 1.5, alpha = 0.7, color = "dodgerblue") +
  geom_signif(
    comparisons = list(c("encoding","er"), c("encoding", "response"), c("er","lr"), c("er","response")),
    annotations = c("*", " ", " ", " "),
    y_position = c(8.1, 7.9, 7.7, 7.5),
    tip_length = 0.015,
    size = 0.8,
    textsize = 7
  ) +
  scale_y_continuous(breaks = seq(2.0, 8.5, 1.0), limits = c(2.5, 8.8), expand = c(0, 0)) +
  scale_x_discrete(labels = c("encoding" = "encoding", "er" = "early retention", "lr" = "late retention", "response" = "response")) +
  labs(y = "ITF (Hz)", tag = "C") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# ---- Final Layout Assembly ----
layout <- "
AB
_C
"

# Swapped A and B
final_plot <- (plot_b + plot_a + plot_spacer() + plot_c) +
  plot_layout(design = layout)

# ---- Display or save ----
print(final_plot)

# Optionally save:
ggsave("FIG2.pdf", plot = final_plot, dpi = 600)

# ==============================================================================
#                           SUPPLEMENTARY PLOTTING - ALPHA
# ==============================================================================

# Alpha frequency - violin plot

# Define the dataset file path
EEG_readouts_alpha <- file.path(main_directory, "EEG_readouts_alpha.csv")

# Load the dataset
EEG_readouts_alpha <- read.csv(EEG_readouts_alpha)

ggplot(EEG_readouts_alpha, aes(x = Condition, y = Frequency)) +
  # Violin plot with soft blue fill
  geom_violin(
    fill = "#cce5ff", 
    color = NA,
    alpha = 0.6,
    trim = FALSE,
    width = 0.9,
    adjust = 1.2
  ) +
  
  # Boxplot within the violin
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    fill = "#99ccff", 
    color = "#3399ff",
    linewidth = 0.5,
    alpha = 0.85
  ) +
  
  # Jittered individual data points
  geom_jitter(
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.7,
    color = "dodgerblue"
  ) +
  
  # Significance annotations
  geom_signif(comparisons = list(c("encoding","er"),c("er","response"),c("er","lr"),c("lr", "response")),
              annotations = c("***"," ","**","*"),
              y_position = c(13.4,13.4,13.1,13.0),  # adjust based on your y-axis
              tip_length = 0.01,
              size = 1,
              textsize = 8) +
  
  # Y-axis for Alpha frequency
  scale_y_continuous(
    breaks = seq(8.0, 13.5, 1.0),
    limits = c(8.5, 13.8),
    expand = c(0, 0)
  ) +
  
  # X-axis labels
  scale_x_discrete(labels = c(
    "encoding" = "encoding",
    "er" = "early retention",
    "lr" = "late retention",
    "response" = "response"
  )) +
  
  # Minimal theme styling
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


# Alpha power - violin plot

ggplot(EEG_readouts_alpha, aes(x = Condition, y = Power)) +
  # Violin plot with light orange tone
  geom_violin(
    fill = "#ffdab3",  # Light peach
    color = NA,
    alpha = 0.6,
    trim = FALSE,
    width = 0.9,
    adjust = 1.2
  ) +
  
  # Boxplot inside violin
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    fill = "#ffb84d",      # Soft orange
    color = "#e67300",     # Stronger outline orange
    linewidth = 0.5,
    alpha = 0.85
  ) +
  
  # Jittered data points
  geom_jitter(
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.7,
    color = "tan1"
  ) +
  
  # Y-axis (adjust if needed for alpha power range)
  scale_y_continuous(
    breaks = seq(26.0, 32.0, 1),
    limits = c(26.0, 32.0),
    expand = c(0, 0)
  ) +
  
  # Condition labels
  scale_x_discrete(labels = c(
    "encoding" = "encoding",
    "er" = "early retention",
    "lr" = "late retention",
    "response" = "response"
  )) +
  
  # Clean minimal theme
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


# PLOT TOGETHER FOR MANUSCRIPT: ALPHA 


# ---- Load Alpha Dataset ----
EEG_readouts_alpha <- read.csv(file.path(main_directory, "EEG_readouts_alpha.csv"))

# ---- Plot A: Alpha Power ----
plot_alpha_power <- ggplot(EEG_readouts_alpha, aes(x = Condition, y = Power)) +
  geom_violin(fill = "#ffdab3", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#ffb84d", color = "#e67300", linewidth = 0.5, alpha = 0.85) +
  geom_jitter(position = position_jitter(width = 0.15), size = 1.5, alpha = 0.7, color = "tan1") +
  scale_y_continuous(breaks = seq(26.0, 32.0, 1), limits = c(26.0, 32.2), expand = c(0, 0)) +
  scale_x_discrete(labels = c("encoding" = "encoding", "er" = "early retention", 
                              "lr" = "late retention", "response" = "response")) +
  labs(y = "Alpha power (dBµV²)", tag = "A") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# ---- Plot B: Alpha Frequency ----
plot_alpha_freq <- ggplot(EEG_readouts_alpha, aes(x = Condition, y = Frequency)) +
  geom_violin(fill = "#cce5ff", color = NA, alpha = 0.6, trim = FALSE, width = 0.9, adjust = 1.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "#99ccff", color = "#3399ff", linewidth = 0.5, alpha = 0.85) +
  geom_jitter(position = position_jitter(width = 0.15), size = 1.5, alpha = 0.7, color = "dodgerblue") +
  geom_signif(comparisons = list(c("encoding", "er"), c("er", "response"), c("er", "lr"), c("lr", "response")),
              annotations = c("***", " ", "**", "*"),
              y_position = c(13.5, 13.5, 13.15, 13.0),
              tip_length = 0.015,
              size = 0.8,
              textsize = 7) +
  scale_y_continuous(breaks = seq(8.0, 13.5, 1.0), limits = c(8.5, 14.0), expand = c(0, 0)) +
  scale_x_discrete(labels = c("encoding" = "encoding", "er" = "early retention", 
                              "lr" = "late retention", "response" = "response")) +
  labs(y = "IAF (Hz)", tag = "B") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# ---- Combine: Side-by-Side ----
final_alpha_plot <- plot_alpha_power + plot_alpha_freq + plot_layout(nrow = 1)

# ---- Display or save ----
print(final_alpha_plot)

# Optional save
ggsave("SUPFIG1.jpg", plot = final_alpha_plot, dpi = 600)


# ==============================================================================
#                            STATISTICAL ANALYSIS - RQ1
# ==============================================================================

# Define the main directory
main_directory <- # * set your directory

# Construct the full file path
file_path <- file.path(main_directory, "EEG_readouts_theta.csv")

# Load the dataset using the full file path
EEG_readouts_theta <- read.csv(file_path)

# ---- Friedman

# Assumption for RM-ANOVA
shapiro.test(EEG_readouts_theta$Power) # p<0.05, perform non-parametric Friedman
shapiro.test(EEG_readouts_theta$Frequency) # p<0.05, perform non-parametric Friedman

# Test differences across phases
friedman_power<-friedman.test(Power ~ Condition | File, data = EEG_readouts_theta)
print(friedman_power)

friedman_frequency<-friedman.test(Frequency ~ Condition | File, data = EEG_readouts_theta)
print(friedman_frequency)

# Identify participants who have ANY 8.446Hz in any condition 
# Explanation: 8.446Hz is not really a peak frequency, simply the upper boundary of the theta range
participants_with_8446_anywhere <- EEG_readouts_theta %>%
  filter(Frequency == 8.446) %>%
  distinct(File)

# Filter dataset to keep only participants who NEVER have 8.446Hz in any condition
final_dataset_no_8446_anywhere <- EEG_readouts_theta %>%
  filter(!File %in% participants_with_8446_anywhere$File)

output_path <- file.path(main_directory, "final_dataset_no_8446_anywhere.csv")

# Save the data
write.csv(final_dataset_no_8446_anywhere, output_path, row.names = FALSE)

# Run Friedman test on filtered dataset
friedman_frequency <- friedman.test(Frequency ~ Condition | File, data = final_dataset_no_8446_anywhere)
print(friedman_frequency)

# Post-hoc testing
pairwise_frequency <-pairwise.wilcox.test(EEG_readouts_theta$Frequency,
                                          EEG_readouts_theta$Condition,
                                          paired = TRUE,
                                          p.adjust.method = "fdr") # use FDR for multiple testing
print(pairwise_frequency)

pairwise_frequency <-pairwise.wilcox.test(final_dataset_no_8446_anywhere$Frequency,
                                          final_dataset_no_8446_anywhere$Condition,
                                          paired = TRUE,
                                          p.adjust.method = "fdr") # use FDR for multiple testing
print(pairwise_frequency)

pairwise_power<-pairwise.wilcox.test(EEG_readouts_theta$Power,
                                     EEG_readouts_theta$Condition,
                                     paired = TRUE,
                                     p.adjust.method = "fdr") # use FDR for multiple testing
print(pairwise_power)

# Extra Z-statistics and effect sizes

Zstat<-qnorm(pairwise_power$p.value/2)
print(Zstat)
wilcox_effsize(EEG_readouts_theta,Power~Condition,paired=TRUE)

Zstat<-qnorm(pairwise_frequency$p.value/2)
print(Zstat)
wilcox_effsize(final_dataset_no_8446_anywhere,Frequency~Condition,paired=TRUE)

# Extra t-statistics

# ---- Function to calculate T, Z, and p for all condition pairs
get_wilcoxon_T_stats <- function(data, variable, id_col, cond_col, main_directory, filename_prefix) {
  library(dplyr)
  library(tidyr)
  
  conds <- unique(data[[cond_col]])
  pairs <- combn(conds, 2, simplify = FALSE)
  
  results_list <- lapply(pairs, function(pair) {
    data_pair <- data %>%
      filter(!!sym(cond_col) %in% pair) %>%
      arrange(!!sym(id_col), !!sym(cond_col))
    
    # Pivot to wide format by subject
    data_wide <- data_pair %>%
      pivot_wider(id_cols = !!sym(id_col),
                  names_from = !!sym(cond_col),
                  values_from = !!sym(variable))
    
    # Paired vectors
    x <- data_wide[[pair[1]]]
    y <- data_wide[[pair[2]]]
    
    # Handle missing data
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
    
    if (length(x) < 3) return(NULL) # Too few samples to test
    
    # Wilcoxon test
    test <- wilcox.test(x, y, paired = TRUE, exact = FALSE, correct = FALSE)
    
    # T-stat
    d <- x - y
    d_nonzero <- d[d != 0]
    r <- rank(abs(d_nonzero))
    sr <- r * sign(d_nonzero)
    T_stat <- min(sum(sr[sr > 0]), abs(sum(sr[sr < 0])))
    
    # Z and p
    n <- length(d_nonzero)
    meanT <- n * (n + 1) / 4
    sdT <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
    z_stat <- (T_stat - meanT) / sdT
    p_val <- 2 * pnorm(-abs(z_stat))
    
    data.frame(
      Condition1 = pair[1],
      Condition2 = pair[2],
      n = n,
      T = T_stat,
      Z = round(z_stat, 3),
      p = signif(p_val, 3)
    )
  })
  
  results_df <- do.call(rbind, results_list)
  
  # Save to CSV
  output_file <- file.path(main_directory, paste0(filename_prefix, "_", variable, "_wilcoxon_T_stats.csv"))
  write.csv(results_df, output_file, row.names = FALSE)
  
  cat("Saved:", output_file, "\n")
  return(results_df)
}



# ---- Set main directory
main_directory <- # * set your directory
  

# ---- Run the tests

# Power – full dataset
get_wilcoxon_T_stats(
  data = EEG_readouts_theta,
  variable = "Power",
  id_col = "File",
  cond_col = "Condition",
  main_directory = main_directory,
  filename_prefix = "allsubjects"
)

# Frequency – filtered dataset
get_wilcoxon_T_stats(
  data = final_dataset_no_8446_anywhere,
  variable = "Frequency",
  id_col = "File",
  cond_col = "Condition",
  main_directory = main_directory,
  filename_prefix = "filtered"
)



### SUPPLEMENTARY: ALPHA

# Define the dataset file path
EEG_readouts_alpha <- file.path(main_directory, "EEG_readouts_alpha.csv")

# Load the dataset
EEG_readouts_alpha <- read.csv(EEG_readouts_alpha)

# Test differences across phases
friedman_power<-friedman.test(Power ~ Condition | File, data = EEG_readouts_alpha) # alpha
print(friedman_power)

friedman_frequency<-friedman.test(Frequency ~ Condition | File, data = EEG_readouts_alpha) # alpha
print(friedman_frequency)


pairwise_frequency <-pairwise.wilcox.test(EEG_readouts_alpha$Frequency,
                                          EEG_readouts_alpha$Condition,
                                          paired = TRUE,
                                          p.adjust.method = "fdr") # use FDR for multiple testing
print(pairwise_frequency)

Zstat<-qnorm(pairwise_frequency$p.value/2)
print(Zstat)
wilcox_effsize(EEG_readouts_alpha,Frequency~Condition,paired=TRUE)

# Extra t-statistics

get_wilcoxon_T_stats(
  data = EEG_readouts_alpha,
  variable = "Frequency",
  id_col = "File",
  cond_col = "Condition",
  main_directory = main_directory,
  filename_prefix = "alpha"
)



# ==============================================================================
#                     STATISTICAL ANALYSIS - RQ2 (Theta ~ d-prime)
# ==============================================================================



# Define the main directory
main_directory <- # * set your directory
  
# Define the dataset file path
EEG_readouts_theta <- file.path(main_directory, "EEG_readouts_theta.csv")

# Load the dataset
EEG_readouts_theta <- read.csv(EEG_readouts_theta)

# ==============================================================================
#                     RQ2 - THETA FREQUENCY ~ d-prime
# ==============================================================================

### Theta frequency and d-prime

# Convert dataset to wide format
theta_wide <- EEG_readouts_theta %>%
  select(File, Condition, Frequency) %>%
  pivot_wider(names_from = Condition, values_from = Frequency, 
              names_prefix = "Theta_Frequency_")


# Load CCDT dataset (File, hitrate, falserate, dprime)
data_ccdt <- read_excel(
  file.path(main_directory, "data_ccdt.xlsx"),
  col_names = TRUE
)

# Ensure File is numeric for both datasets
data_ccdt <- data_ccdt %>% mutate(File = as.integer(File))
theta_wide <- theta_wide %>% mutate(File = as.integer(File))

# Merge datasets
merged_data <- data_ccdt %>% inner_join(theta_wide, by = "File")

# Ensure dprime is numeric
merged_data$dprime <- as.numeric(as.character(merged_data$dprime))

# Define the save path in the main directory
save_path_merged_data <- file.path(main_directory, "merged_data.csv")

# Save the dataset to the main directory
write.csv(merged_data, save_path_merged_data, row.names = FALSE)

# Set directory
main_directory <- # * set your data directory here

# Define the dataset file path
merged_data <- file.path(main_directory, "merged_data.csv")

# Load the dataset
merged_data <- read.csv(merged_data)



# ==============================================================================
#                       RQ2 - THETA POWER ~ d-prime
# ==============================================================================

### Theta power and d-prime


# Convert dataset to wide format using Power instead of Frequency
theta_power_wide <- EEG_readouts_theta %>%
  select(File, Condition, Power) %>%
  pivot_wider(names_from = Condition, values_from = Power, 
              names_prefix = "Theta_Power_")

# Load CCDT dataset (File, hitrate, falserate, dprime)
data_ccdt <- read_excel(
  file.path(main_directory, "data_ccdt.xlsx"),
  col_names = TRUE
)

# Ensure File is numeric for both datasets
data_ccdt <- data_ccdt %>% mutate(File = as.integer(File))
theta_power_wide <- theta_power_wide %>% mutate(File = as.integer(File))

# Merge datasets
merged_data_power <- data_ccdt %>% inner_join(theta_power_wide, by = "File")

# Ensure dprime is numeric
merged_data_power$dprime <- as.numeric(as.character(merged_data_power$dprime))

# Define the path to save the merged dataset
save_path_power <- file.path(main_directory, "merged_data_power.csv")

# Save to CSV
write.csv(merged_data_power, save_path_power, row.names = FALSE)





###############################

### Frequency ~ d-prime

###############################

# Load the dataset
merged_data <- read.csv("merged_data.csv")  # Update if needed


### General, full dataset: 

# Compute correlations of frequency with d-prime
freq_cor_tests <- list(
  Freq_Enc_cor  = cor.test(merged_data$dprime, merged_data$Theta_Frequency_encoding, method = "spearman"),
  Freq_ER_cor  = cor.test(merged_data$dprime, merged_data$Theta_Frequency_er, method = "spearman"),
  Freq_LR_cor = cor.test(merged_data$dprime, merged_data$Theta_Frequency_lr, method = "spearman"),
  Freq_Ret_cor   = cor.test(merged_data$dprime, merged_data$Theta_Frequency_response, method = "spearman")
)
# Extract results
correlations <- sapply(freq_cor_tests, function(x) x$estimate)
p_values <- sapply(freq_cor_tests, function(x) x$p.value)

# Apply FDR correction
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Combine into results data frame
freq_results_df <- data.frame(
  Comparison = names(freq_cor_tests),
  Correlation = as.numeric(correlations),
  P_Value = p_values,
  P_Value_FDR = p_values_fdr
)

# View results
print("Correlations between d-prime and theta frequency:")
print(freq_results_df)

#### Shifts, full dataset:

# Compute frequency shifts between phases
merged_data <- merged_data %>%
  mutate(
    Freq_Enc_ER  = Theta_Frequency_encoding - Theta_Frequency_er,
    Freq_Enc_LR  = Theta_Frequency_encoding - Theta_Frequency_lr,
    Freq_Enc_Ret = Theta_Frequency_encoding - Theta_Frequency_response,
    Freq_ER_LR   = Theta_Frequency_er - Theta_Frequency_lr,
    Freq_ER_Ret  = Theta_Frequency_er - Theta_Frequency_response,
    Freq_LR_Ret  = Theta_Frequency_lr - Theta_Frequency_response
  )

# Compute correlations of frequency differences with d-prime
freq_diff_cor_tests <- list(
  Freq_Enc_ER_cor  = cor.test(merged_data$dprime, merged_data$Freq_Enc_ER, method = "spearman"),
  Freq_Enc_LR_cor  = cor.test(merged_data$dprime, merged_data$Freq_Enc_LR, method = "spearman"),
  Freq_Enc_Ret_cor = cor.test(merged_data$dprime, merged_data$Freq_Enc_Ret, method = "spearman"),
  Freq_ER_LR_cor   = cor.test(merged_data$dprime, merged_data$Freq_ER_LR, method = "spearman"),
  Freq_ER_Ret_cor  = cor.test(merged_data$dprime, merged_data$Freq_ER_Ret, method = "spearman"),
  Freq_LR_Ret_cor  = cor.test(merged_data$dprime, merged_data$Freq_LR_Ret, method = "spearman")
)


# Extract results
correlations <- sapply(freq_diff_cor_tests, function(x) x$estimate)
p_values <- sapply(freq_diff_cor_tests, function(x) x$p.value)

# Apply FDR correction
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Combine into results data frame
freq_diff_results_df <- data.frame(
  Comparison = names(freq_diff_cor_tests),
  Correlation = as.numeric(correlations),
  P_Value = p_values,
  P_Value_FDR = p_values_fdr
)

# View results
print("Correlations between d-prime and theta frequency differences:")
print(freq_diff_results_df)



### Plot theta peak difference encoding-responseieval ~ d-prime (for visualization now)

# Define the dataset file path
merged_data <- file.path(main_directory, "merged_data.csv")

# Load the dataset
merged_data <- read.csv(merged_data)

# Calculate frequency difference
merged_data$Freq_Enc_Ret <- merged_data$Theta_Frequency_encoding - merged_data$Theta_Frequency_response

# Your values (replace with exact if different)
rho_val <- -0.435
p_fdr <- 0.048

# Plot with manual annotation
ggplot(merged_data, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 3, color = "#3366CC", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#3366CC", fill = "#A6C8FF") +
  annotate("text", x = min(merged_data$Freq_Enc_Ret), 
           y = max(merged_data$dprime), 
           label = paste(rho_label, p_label, sep = "\n"),
           hjust = 0, vjust = 1, size = 5) +
  labs(
    title = "Correlation between ITF shift and d-prime",
    subtitle = "encoding – responseieval ITF shift vs. d-prime",
    x = "ITF shift (encoding – responseieval, in Hz)",
    y = "d-prime"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Suitable for Spearman
ggplot(merged_data, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 3, color = "black", alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  annotate("text", 
           x = min(merged_data$Freq_Enc_Ret), 
           y = max(merged_data$dprime), 
           label = paste(rho_label, p_label, sep = "\n"),
           hjust = 0, vjust = 1, size = 5) +
  labs(
    title = "Correlation between ITF Shift and d-prime",
    subtitle = "Spearman correlation: encoding – responseieval ITF shift vs. d-prime",
    x = "ITF Shift (Encoding – responseieval, Hz)",
    y = "d-prime"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Check outliers in the predictor (Freq_Enc_Ret)
merged_data %>% 
  identify_outliers(Freq_Enc_Ret)

merged_data_filtered <- merged_data %>% filter(Freq_Enc_Ret < 4.5)  # example threshold
cor.test(merged_data_filtered$Freq_Enc_Ret, merged_data_filtered$dprime, method = "spearman")

# Define the path to save the merged dataset
save_path_merged_data_filtered <- file.path(main_directory, "merged_data_filtered.csv")

# Save to CSV
write.csv(merged_data_filtered, save_path_merged_data_filtered, row.names = FALSE)


# Plot without outlier
ggplot(merged_data_filtered, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 4, color = "#3366CC", alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  annotate("text", 
           x = min(merged_data_filtered$Freq_Enc_Ret), 
           y = max(merged_data_filtered$dprime), 
           label = paste(rho_label, p_label, sep = "\n"),
           hjust = 0, vjust = 1, size = 10) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size=25),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
##############################

### Power ~ d-prime

##############################


# Define the main directory
main_directory <- # * set your directory
  
# Define the dataset file path
merged_data_power <- file.path(main_directory, "merged_data_power.csv")

# Load the dataset
merged_data_power <- read.csv(merged_data_power)


### General, full dataset: 

# Compute correlations of power with d-prime
power_cor_tests <- list(
  Power_Enc_cor  = cor.test(merged_data_power$dprime, merged_data_power$Theta_Power_encoding, method = "spearman"),
  Power_ER_cor  = cor.test(merged_data_power$dprime, merged_data_power$Theta_Power_er, method = "spearman"),
  Power_LR_cor = cor.test(merged_data_power$dprime, merged_data_power$Theta_Power_lr, method = "spearman"),
  Power_Ret_cor   = cor.test(merged_data_power$dprime, merged_data_power$Theta_Power_response, method = "spearman")
)
# Extract results
correlations <- sapply(power_cor_tests, function(x) x$estimate)
p_values <- sapply(power_cor_tests, function(x) x$p.value)

# Apply FDR correction
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Combine into results data frame
power_results_df <- data.frame(
  Comparison = names(power_cor_tests),
  Correlation = as.numeric(correlations),
  P_Value = p_values,
  P_Value_FDR = p_values_fdr
)

# View results
print("Correlations between d-prime and theta Power:")
print(power_results_df)

#### Differences, full dataset


# Compute power differences between phases
merged_data_power <- merged_data_power %>%
  mutate(
    Power_Enc_ER  = Theta_Power_encoding - Theta_Power_er,
    Power_Enc_LR  = Theta_Power_encoding - Theta_Power_lr,
    Power_Enc_Ret = Theta_Power_encoding - Theta_Power_response,
    Power_ER_LR   = Theta_Power_er - Theta_Power_lr,
    Power_ER_Ret  = Theta_Power_er - Theta_Power_response,
    Power_LR_Ret  = Theta_Power_lr - Theta_Power_response
  )

# Perform Spearman correlations with d-prime
power_diff_cor_tests <- list(
  Power_Enc_ER_cor  = cor.test(merged_data_power$dprime, merged_data_power$Power_Enc_ER, method = "spearman"),
  Power_Enc_LR_cor  = cor.test(merged_data_power$dprime, merged_data_power$Power_Enc_LR, method = "spearman"),
  Power_Enc_Ret_cor = cor.test(merged_data_power$dprime, merged_data_power$Power_Enc_Ret, method = "spearman"),
  Power_ER_LR_cor   = cor.test(merged_data_power$dprime, merged_data_power$Power_ER_LR, method = "spearman"),
  Power_ER_Ret_cor  = cor.test(merged_data_power$dprime, merged_data_power$Power_ER_Ret, method = "spearman"),
  Power_LR_Ret_cor  = cor.test(merged_data_power$dprime, merged_data_power$Power_LR_Ret, method = "spearman")
)

# Extract correlation coefficients and p-values
power_correlations <- sapply(power_diff_cor_tests, function(x) x$estimate)
power_p_values <- sapply(power_diff_cor_tests, function(x) x$p.value)

# Apply FDR correction
power_p_values_fdr <- p.adjust(power_p_values, method = "fdr")

# Combine into results data frame
power_diff_results_df <- data.frame(
  Comparison = names(power_diff_cor_tests),
  Correlation = as.numeric(power_correlations),
  P_Value = power_p_values,
  P_Value_FDR = power_p_values_fdr
)

# View results
print("Correlations between d-prime and theta power differences:")
print(power_diff_results_df)


# ==============================================================================
#                         RQ2 - ALPHA FREQUENCY ~ d-prime
# ==============================================================================


# Define the main directory
main_directory <- # * set your directory

# Load Alpha dataset
EEG_readouts_alpha <- read.csv(file.path(main_directory, "EEG_readouts_alpha.csv"))


#----------------------------
### Alpha Frequency ~ d-prime
#----------------------------

# Convert to wide format
alpha_freq_wide <- EEG_readouts_alpha %>%
  select(File, Condition, Frequency) %>%
  pivot_wider(names_from = Condition, values_from = Frequency,
              names_prefix = "Alpha_Frequency_")

# Load CCDT dataset (File, hitrate, falserate, dprime)
data_ccdt <- read_excel(
  file.path(main_directory, "data_ccdt.xlsx"),
  col_names = TRUE
)

# Ensure File is numeric for both datasets
data_ccdt <- data_ccdt %>% mutate(File = as.integer(File))
alpha_freq_wide <- alpha_freq_wide %>% mutate(File = as.integer(File))

# Merge with frequency data
merged_alpha_freq <- merge(data_ccdt, alpha_freq_wide, by = "File")
merged_alpha_freq$dprime <- as.numeric(as.character(merged_alpha_freq$dprime))

# Save merged frequency data
write.csv(merged_alpha_freq, file.path(main_directory, "merged_data_alpha_freq.csv"), row.names = FALSE)

# Correlations: Alpha Frequency ~ d-prime
alpha_freq_cor_tests <- list(
  Freq_Enc_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Alpha_Frequency_encoding, method = "spearman"),
  Freq_ER_cor   = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Alpha_Frequency_er, method = "spearman"),
  Freq_LR_cor   = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Alpha_Frequency_lr, method = "spearman"),
  Freq_Ret_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Alpha_Frequency_response, method = "spearman")
)

# Extract correlation results
alpha_freq_cor_df <- data.frame(
  Comparison = names(alpha_freq_cor_tests),
  Correlation = sapply(alpha_freq_cor_tests, function(x) x$estimate),
  P_Value = sapply(alpha_freq_cor_tests, function(x) x$p.value)
)
alpha_freq_cor_df$P_Value_FDR <- p.adjust(alpha_freq_cor_df$P_Value, method = "fdr")

# Print results
print("Correlations between d-prime and alpha frequency:")
print(alpha_freq_cor_df)

# Frequency shifts
merged_alpha_freq <- merged_alpha_freq %>%
  mutate(
    Freq_Enc_ER  = Alpha_Frequency_encoding - Alpha_Frequency_er,
    Freq_Enc_LR  = Alpha_Frequency_encoding - Alpha_Frequency_lr,
    Freq_Enc_Ret = Alpha_Frequency_encoding - Alpha_Frequency_response,
    Freq_ER_LR   = Alpha_Frequency_er - Alpha_Frequency_lr,
    Freq_ER_Ret  = Alpha_Frequency_er - Alpha_Frequency_response,
    Freq_LR_Ret  = Alpha_Frequency_lr - Alpha_Frequency_response
  )

# Correlation of frequency differences
alpha_freq_diff_tests <- list(
  Freq_Enc_ER_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_Enc_ER, method = "spearman"),
  Freq_Enc_LR_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_Enc_LR, method = "spearman"),
  Freq_Enc_Ret_cor = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_Enc_Ret, method = "spearman"),
  Freq_ER_LR_cor   = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_ER_LR, method = "spearman"),
  Freq_ER_Ret_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_ER_Ret, method = "spearman"),
  Freq_LR_Ret_cor  = cor.test(merged_alpha_freq$dprime, merged_alpha_freq$Freq_LR_Ret, method = "spearman")
)

# Extract results
alpha_freq_diff_df <- data.frame(
  Comparison = names(alpha_freq_diff_tests),
  Correlation = sapply(alpha_freq_diff_tests, function(x) x$estimate),
  P_Value = sapply(alpha_freq_diff_tests, function(x) x$p.value)
)
alpha_freq_diff_df$P_Value_FDR <- p.adjust(alpha_freq_diff_df$P_Value, method = "fdr")

print("Correlations between d-prime and alpha frequency differences:")
print(alpha_freq_diff_df)

# ==============================================================================
#                          RQ2 - ALPHA POWER ~ d-prime
# ==============================================================================

# Convert to wide format
alpha_power_wide <- EEG_readouts_alpha %>%
  select(File, Condition, Power) %>%
  pivot_wider(names_from = Condition, values_from = Power,
              names_prefix = "Alpha_Power_")

# Merge with CCDT data
merged_alpha_power <- merge(data_ccdt, alpha_power_wide, by = "File")
merged_alpha_power$dprime <- as.numeric(as.character(merged_alpha_power$dprime))

# Save merged power data
write.csv(merged_alpha_power, file.path(main_directory, "merged_data_alpha_power.csv"), row.names = FALSE)

# Correlations: Alpha Power ~ d-prime
alpha_power_cor_tests <- list(
  Power_Enc_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Alpha_Power_encoding, method = "spearman"),
  Power_ER_cor   = cor.test(merged_alpha_power$dprime, merged_alpha_power$Alpha_Power_er, method = "spearman"),
  Power_LR_cor   = cor.test(merged_alpha_power$dprime, merged_alpha_power$Alpha_Power_lr, method = "spearman"),
  Power_Ret_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Alpha_Power_response, method = "spearman")
)

# Extract results
alpha_power_cor_df <- data.frame(
  Comparison = names(alpha_power_cor_tests),
  Correlation = sapply(alpha_power_cor_tests, function(x) x$estimate),
  P_Value = sapply(alpha_power_cor_tests, function(x) x$p.value)
)
alpha_power_cor_df$P_Value_FDR <- p.adjust(alpha_power_cor_df$P_Value, method = "fdr")

print("Correlations between d-prime and alpha power:")
print(alpha_power_cor_df)

# Power differences
merged_alpha_power <- merged_alpha_power %>%
  mutate(
    Power_Enc_ER  = Alpha_Power_encoding - Alpha_Power_er,
    Power_Enc_LR  = Alpha_Power_encoding - Alpha_Power_lr,
    Power_Enc_Ret = Alpha_Power_encoding - Alpha_Power_response,
    Power_ER_LR   = Alpha_Power_er - Alpha_Power_lr,
    Power_ER_Ret  = Alpha_Power_er - Alpha_Power_response,
    Power_LR_Ret  = Alpha_Power_lr - Alpha_Power_response
  )

# Correlate power differences
alpha_power_diff_tests <- list(
  Power_Enc_ER_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_Enc_ER, method = "spearman"),
  Power_Enc_LR_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_Enc_LR, method = "spearman"),
  Power_Enc_Ret_cor = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_Enc_Ret, method = "spearman"),
  Power_ER_LR_cor   = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_ER_LR, method = "spearman"),
  Power_ER_Ret_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_ER_Ret, method = "spearman"),
  Power_LR_Ret_cor  = cor.test(merged_alpha_power$dprime, merged_alpha_power$Power_LR_Ret, method = "spearman")
)

# Extract power diff results
alpha_power_diff_df <- data.frame(
  Comparison = names(alpha_power_diff_tests),
  Correlation = sapply(alpha_power_diff_tests, function(x) x$estimate),
  P_Value = sapply(alpha_power_diff_tests, function(x) x$p.value)
)
alpha_power_diff_df$P_Value_FDR <- p.adjust(alpha_power_diff_df$P_Value, method = "fdr")

print("Correlations between d-prime and alpha power differences:")
print(alpha_power_diff_df)


# ==============================================================================
#                       RQ2 PLOTTING FOR MANUSCRIPT
# ==============================================================================


### Include ppns with ITF not being 8.446Hz during encoding and responseieval

# Identify participants with 8.446Hz in encoding or responseieval
participants_with_8446_in_enc_or_ret <- EEG_readouts_theta %>%
  filter(Frequency == 8.446, Condition %in% c("encoding", "response")) %>%
  distinct(File)

# Filter dataset to exclude these participants entirely
final_dataset_no_8446_in_enc_or_ret <- EEG_readouts_theta %>%
  filter(!File %in% participants_with_8446_in_enc_or_ret$File)

# Convert dataset to wide format
theta_wide <- final_dataset_no_8446_in_enc_or_ret %>%
  select(File, Condition, Frequency) %>%
  pivot_wider(names_from = Condition, values_from = Frequency, 
              names_prefix = "Theta_Frequency_")

# Load CCDT dataset (File, hitrate, falserate, dprime)
data_ccdt <- read_excel(
  file.path(main_directory, "data_ccdt.xlsx"),
  col_names = TRUE
)

# Ensure File is numeric for both datasets
data_ccdt <- data_ccdt %>% mutate(File = as.integer(File))
theta_wide <- theta_wide %>% mutate(File = as.integer(File))


# Merge datasets
merged_data_n20 <- merge(data_ccdt, theta_wide, by = "File")

# Ensure dprime is numeric
merged_data_n20$dprime <- as.numeric(as.character(merged_data_n20$dprime))

# Correlations: frequency with d-prime per condition
freq_cor_tests <- list(
  Freq_Enc_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Theta_Frequency_encoding, method = "spearman"),
  Freq_ER_cor   = cor.test(merged_data_n20$dprime, merged_data_n20$Theta_Frequency_er, method = "spearman"),
  Freq_LR_cor   = cor.test(merged_data_n20$dprime, merged_data_n20$Theta_Frequency_lr, method = "spearman"),
  Freq_Ret_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Theta_Frequency_response, method = "spearman")
)

# Extract correlation results
correlations <- sapply(freq_cor_tests, function(x) x$estimate)
p_values <- sapply(freq_cor_tests, function(x) x$p.value)
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Results data frame
freq_results_df <- data.frame(
  Comparison = names(freq_cor_tests),
  Correlation = as.numeric(correlations),
  P_Value = p_values,
  P_Value_FDR = p_values_fdr
)

print("Correlations between d-prime and theta frequency (filtered dataset):")
print(freq_results_df)


# Frequency differences between phases
merged_data_n20 <- merged_data_n20 %>%
  mutate(
    Freq_Enc_ER  = Theta_Frequency_encoding - Theta_Frequency_er,
    Freq_Enc_LR  = Theta_Frequency_encoding - Theta_Frequency_lr,
    Freq_Enc_Ret = Theta_Frequency_encoding - Theta_Frequency_response,
    Freq_ER_LR   = Theta_Frequency_er - Theta_Frequency_lr,
    Freq_ER_Ret  = Theta_Frequency_er - Theta_Frequency_response,
    Freq_LR_Ret  = Theta_Frequency_lr - Theta_Frequency_response
  )

# Optional: Save the new  dataset
write.csv(merged_data_n20, "merged_data_n20.csv", row.names = FALSE)

# Correlations of frequency differences with d-prime
freq_diff_cor_tests <- list(
  Freq_Enc_ER_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_Enc_ER, method = "spearman"),
  Freq_Enc_LR_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_Enc_LR, method = "spearman"),
  Freq_Enc_Ret_cor = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_Enc_Ret, method = "spearman"),
  Freq_ER_LR_cor   = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_ER_LR, method = "spearman"),
  Freq_ER_Ret_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_ER_Ret, method = "spearman"),
  Freq_LR_Ret_cor  = cor.test(merged_data_n20$dprime, merged_data_n20$Freq_LR_Ret, method = "spearman")
)

# Extract correlation results
correlations <- sapply(freq_diff_cor_tests, function(x) x$estimate)
p_values <- sapply(freq_diff_cor_tests, function(x) x$p.value)
p_values_fdr <- p.adjust(p_values, method = "fdr")

# Results data frame
freq_diff_results_df <- data.frame(
  Comparison = names(freq_diff_cor_tests),
  Correlation = as.numeric(correlations),
  P_Value = p_values,
  P_Value_FDR = p_values_fdr
)

print("Correlations between d-prime and theta frequency differences (filtered dataset):")
print(freq_diff_results_df)

# Extract values for the plot
spearman_test <- cor.test(merged_data_n20$dprime, merged_data_n20$Freq_Enc_Ret, method = "spearman")
rho_val <- as.numeric(spearman_test$estimate)
p_val <- spearman_test$p.value
p_fdr <- p.adjust(p_val, method = "fdr")  # Adjust p-value (though unnecessary for single test)

# Create annotation labels
rho_label <- paste0("Spearman's ρ = ", round(rho_val, 3))
#p_label <- paste0("FDR-adjusted p = ", format.pval(p_fdr, digits = 3))
p_label <- paste0("FDR-adjusted p = 0.433")


# Plot Spearman
ggplot(merged_data_n20, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 4, color = "#3366CC", alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  annotate("text", 
           x = min(merged_data_n20$Freq_Enc_Ret), 
           y = max(merged_data_n20$dprime), 
           label = paste(rho_label, p_label, sep = "\n"),
           hjust = 0, vjust = 1, size = 10) +
  scale_y_continuous(breaks = seq(1, 3, by = 1), limits = c(1, 4)) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size=25),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )


### Plot together the n=35 and n=20 ITF shift enc-response ~ dprime


# Define the dataset file path
merged_data_filtered <- file.path(main_directory, "merged_data_filtered.csv")

# Load the dataset
merged_data_filtered <- read.csv(merged_data_filtered)

# Define the dataset file path
merged_data_n20 <- file.path(main_directory, "merged_data_n20.csv")

# Load the dataset
merged_data_n20 <- read.csv(merged_data_n20)

# predefine labels
rho_label_n35 <- paste0("Spearman's ρ = -0.44")
p_label_n35 <- paste0("FDR-adjusted p = 0.048")
rho_label_n20 <- paste0("Spearman's ρ = -0.29")
p_label_n20 <- paste0("FDR-adjusted p = 0.433")

rho_label_n35 <- paste0("")
p_label_n35 <- paste0("")
rho_label_n20 <- paste0("")
p_label_n20 <- paste0("")

merged_data$dprime <- as.numeric(as.character(merged_data$dprime))
merged_data_n20$dprime <- as.numeric(as.character(merged_data_n20$dprime))
merged_data$dprime <- as.numeric(as.character(merged_data$dprime))

# Define common axis limits (adjust as needed)
x_limits <- c(min(c(merged_data_filtered$Freq_Enc_Ret, merged_data_n20$Freq_Enc_Ret), na.rm = TRUE),
              max(c(merged_data_filtered$Freq_Enc_Ret, merged_data_n20$Freq_Enc_Ret), na.rm = TRUE))
y_limits <- c(0, 4)  # Uniform y-axis for both

# Plot for n = 35 (left)
plot_n35 <- ggplot(merged_data_filtered, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 4, color = "#3366CC", alpha = 0.8) +
  annotate("text", 
           x = min(x_limits),
           y = min(y_limits),
           label = paste(rho_label_n35, p_label_n35, sep = "\n"),
           hjust = 0, vjust = 0, size = 6)+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = y_limits) +
  labs(
    title = "A        Complete dataset",
    x = "",
    y = "d-prime"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Plot for n = 20 (right)
plot_n20 <- ggplot(merged_data_n20, aes(x = Freq_Enc_Ret, y = dprime)) +
  geom_point(size = 4, color = "#3366CC", alpha = 0.8) +
  annotate("text", 
           x = min(x_limits),
           y = min(y_limits),
           label = paste(rho_label_n20, p_label_n20, sep = "\n"),
           hjust = 0, vjust = 0, size = 6)+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = y_limits) +
  labs(
    title = "B        Subset with ITF detected peaks",
    x = "",
    y = NULL  # omit y label to avoid duplication
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Combine the plots (with no x-axis titles)
combined_plot <- (plot_n35 + plot_n20) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(axis.title.x = element_blank())

# Create a separate grob for the shared x-axis title
shared_x_label <- grid::textGrob(
  "ITF shift (encoding - response stage, Hz)",
  gp = grid::gpar(fontsize = 16, fontface = "bold")
)

# Assemble final layout: plots + x-axis label
final_plot <- patchwork::wrap_elements(full = combined_plot) /
  patchwork::wrap_elements(full = shared_x_label) +
  plot_layout(heights = c(1, 0.05))  # Adjust spacing below plots

# Display it
print(final_plot)

# Optionally save:
ggsave("FIG3.pdf", plot = final_plot, dpi = 600)

# ==============================================================================
#                               END OF SCRIPT
# ==============================================================================

