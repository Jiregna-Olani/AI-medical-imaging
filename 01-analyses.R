# Load Required Libraries
library(gt)
library(ggrepel)
library(here)
library(janitor)
library(psych)
library(readxl)
library(RColorBrewer)
library(scales)
library(stringr)
library(tidyr)
library(tidyverse)


# Load and Clean Your Dataset
data <- readxl::read_xlsx(here("Data_Raw", "review_561049_20250506232627.xlsx")) %>%
  clean_names()  # Makes column names lower_case_with_underscores
# Convert Percent Variables to Numeric
# data <- data %>%
#   mutate(
#     dice_subgroup1 = as.numeric("dice_score_percent_subgroup_1"),
#     dice_subgroup2 = as.numeric("dice_score_percent_subgroup_2"),
#     sensitivity_subgroup1 = as.numeric("sensitivity_percent_subgroup_1") / 100,
#     specificity_subgroup1 = as.numeric("specificity_percent_subgroup_1") / 100,
#     sensitivity_subgroup2 = as.numeric("sensitivity_percent_subgroup_2") / 100,
#     specificity_subgroup2 = as.numeric("specificity_percent_subgroup_2") / 100
#   )


# Summary table 1
# Get names of all columns except 'study_id'
var_names <- setdiff(names(data), "study_id")

# Create a tibble with subgroup mapping
variable_subgroups <- tibble(
  variable = var_names,
  subgroup = case_when(
    str_ends(variable, "1") ~ 1,
    str_ends(variable, "2") ~ 2,
    TRUE ~ NA_real_
  )
)


# Summary 1: Study Design Frequency
data %>%
  count(study_design) %>%
  arrange(desc(n)) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  gt() %>%
  tab_header(title = "Study Designs in the Umbrella Review")


# Summary table 2
patterns <- c("MRI", "CT scan", "PET-CT Scans", "PET", "Ultrasonography", "X- Ray", "Other")
names(patterns) <- c("MRI", "CT scan", "PET-CT Scans", "PET", "Ultrasonography", "X-Ray", "Others")

counts <- sapply(patterns, function(p) sum(grepl(p, data$diagnostic_technique, ignore.case = TRUE)))
category_summary <- data.frame(
  diagnostic_technique = names(counts),
  n = as.integer(counts),
  percent = round(100 * counts / nrow(data), 1)
)

category_summary %>%
  gt() %>%
  tab_header(title = "Diagnostic Technique Frequency and Percentages")


category_summary <- data.frame(
  diagnostic_technique = c('MRI', 'CT scan', 'PET-CT Scans', 'PET', 'Ultrasonography', 'X-Ray', 'Others'),
  n = c(55, 41, 9, 14, 6, 6, 8)
)

category_summary <- category_summary %>%
  mutate(percent = n / sum(n) * 100,
         label = paste0(diagnostic_technique, "\n", n, " (", round(percent, 1), "%)")) %>%
  arrange(desc(diagnostic_technique)) %>%
  mutate(
    ymax = cumsum(n),
    ymin = c(0, head(ymax, -1)),
    ypos = (ymax + ymin) / 2
  )

ggplot(category_summary, aes(x = "", y = n, fill = diagnostic_technique)) +
  geom_col(width = 1, color = "white", show.legend = TRUE) +
  coord_polar(theta = "y") +
  geom_text_repel(
    aes(y = ypos, label = label),
    nudge_x = 0.3,
    direction = "y",
    size = 4,
    show.legend = FALSE,
    segment.size = 0,
    box.padding = 0.7,
    point.padding = 0.8,
    force = 2
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribution of Diagnostic Techniques") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


# Clean and standardize the type_of_ml_used_subgroup_1 and type_of_ml_used_subgroup_2 columns
data <- data %>%
  mutate(across(
    c(type_of_ml_used_subgroup_1, type_of_ml_used_subgroup_2),
    ~ .x %>%
      str_trim() %>%
      str_replace_all("_|-", " ") %>%
      str_squish() %>%
      recode(
        # Core categories
        "ML" = "ML",
        "DL" = "DL",
        "ML, DL" = "ML & DL",
        "ML/DL" = "ML & DL",
        "DL AND TML" = "ML & DL",

        # Deep learning variants
        "2D DL Models" = "DL",
        "3D DL Models" = "DL",
        "AI, DL" = "DL",
        "CNN" = "DL",
        "CNN, LASSO" = "DL",
        "DNN" = "DL",
        "Deep learning neural networks" = "DL",

        # ML variants
        "Random forest, SVM" = "ML",
        "Machine learning classifiers" = "ML",
        "Decision Trees" = "ML",

        # CAD
        "CAD, CADx, CADt" = "CAD",

        # Radiologist
        "Radiologists" = "Radiologist",
        "Radiologist" = "Radiologist",

        # Non-AI (all possible variants)
        "Non-AI" = "Non-AI",
        "Non AI" = "Non-AI",
        "non AI" = "Non-AI",
        "non-AI" = "Non-AI",
        "NON AI" = "Non-AI",
        "Non deep learning" = "Non-AI",

        .default = .x
      )
  ))

# Combine the two columns
ml_type_combined <- data %>%
  select(type_of_ml_used_subgroup_1, type_of_ml_used_subgroup_2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Subgroup", values_to = "ML_Type"
  ) %>%
  filter(!is.na(ML_Type) & ML_Type != "")

# Final normalization for edge cases (e.g., "Non AI", "Non-AI", "non AI", etc.)
ml_type_combined <- ml_type_combined %>%
  mutate(
    ML_Type = case_when(
      ML_Type %in% c("Non-AI", "Non AI", "non AI", "non-AI", "NON AI") ~ "Non-AI",
      TRUE ~ ML_Type
    )
  )

# Count frequencies and percentages
ml_type_counts <- ml_type_combined %>%
  count(ML_Type, sort = TRUE) %>%
  filter(!is.na(ML_Type) & ML_Type != "") %>%
  mutate(
    Percentage = n / sum(n),
    Percentage_Label = paste0(n, " (", percent(Percentage, accuracy = 0.1), ")")
  )

# Plot
ggplot(ml_type_counts, aes(x = reorder(ML_Type, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  geom_text(aes(label = Percentage_Label), hjust = -0.1, size = 4.5) +
  labs(
    title = "Frequency Distribution of AI Techniques Used in Studies",
    x = "Type of AI Used",
    y = "Frequency Distribution of AI Techniques"
  ) +
  ylim(0, max(ml_type_counts$n) * 1.15) +
  theme_minimal()



summary_table <- data %>%
  filter(
    !is.na(dice_score_percent_subgroup_1) | !is.na(dice_score_percent_subgroup_2)
  ) %>%
  group_by(study_id, field_of_application, diagnostic_technique) %>%
  summarise(
    total_numbers_of_studies = sum(total_number_of_studies, na.rm = TRUE),
    n_of_patients_in_meta = sum(n_of_patients_in_meta, na.rm = TRUE),
    I2_percent = coalesce(first(na.omit(i2_percent_subgroup_1)), first(na.omit(i2_percent_subgroup_2))),
    dice_score = coalesce(first(na.omit(dice_score_percent_subgroup_1)), first(na.omit(dice_score_percent_subgroup_2))),
    q_test = coalesce(first(na.omit(q_test_subgroup_1)), first(na.omit(q_test_subgroup_2))),
    .groups = "drop"
  )

print(summary_table)


# Define diagnostic technique patterns and names
patterns <- c("MRI", "CT scan", "PET-CT Scans", "PET", "Ultrasonography", "X- Ray", "Other")
names(patterns) <- c("MRI", "CT scan", "PET-CT Scans", "PET", "Ultrasonography", "X-Ray", "Others")

# Extract publication year from study_id
data$publication_year <- str_extract(data$study_id, "\\b(19|20)\\d{2}\\b") %>% as.numeric()

# Classify by diagnostic technique pattern
data$diagnostic_technique_category <- "Others"
for (p in names(patterns)) {
  matches <- grepl(patterns[p], data$diagnostic_technique, ignore.case = TRUE)
  data$diagnostic_technique_category[matches] <- names(patterns)[which(names(patterns) == p)]
}
data$diagnostic_technique_category[data$diagnostic_technique_category == "X- Ray"] <- "X-Ray"

# Summarize counts by year and diagnostic technique, keep all years/techniques
dt_year_summary <- data %>%
  filter(!is.na(publication_year)) %>%
  count(publication_year, diagnostic_technique_category) %>%
  complete(publication_year, diagnostic_technique_category, fill = list(n = 0))

# Order techniques by total overall frequency
tech_order <- dt_year_summary %>%
  group_by(diagnostic_technique_category) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(diagnostic_technique_category)
dt_year_summary$diagnostic_technique_category <- factor(dt_year_summary$diagnostic_technique_category, levels = tech_order)

# Best line graph
ggplot(dt_year_summary, aes(x = publication_year, y = n, color = diagnostic_technique_category)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Trends in Diagnostic Techniques Used by Publication Year",
    x = "Publication Year",
    y = "Number of Reviews",
    color = "Diagnostic Technique"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = sort(unique(dt_year_summary$publication_year)))



