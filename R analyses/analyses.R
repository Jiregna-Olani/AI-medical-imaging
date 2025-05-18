# ===============================
# 🧰 Step 1: Install & Load Packages
# ===============================
# Install once
install.packages(c(
  "readxl", "dplyr", "meta", "metafor",
  "ggplot2", "psych", "janitor"
))

# Load necessary libraries
library(readxl)
library(dplyr)
library(meta)
library(metafor)
library(ggplot2)
library(psych)
library(janitor)

# Load the dataset
# file_path <- "C:/Users/JiregnaOlaniKedida/Documents/github/FB-injuries-Africa/DataRaw/db_review_africa.xlsx"
#
# # Read data and clean column names
# data <- read_excel(file_path, sheet = "Sheet 1") %>%
#   janitor::clean_names()


```
# ===============================
# 📂 Step 2: Import and Inspect Data
# ===============================
# Load your data
data <- readxl::read_xlsx(here::here("Data_Raw",
                                     "review_561049_20250506232627.xlsx"))
# data <- read_excel("/mnt/data/review_561049_20250506232627.xlsx")

# Clean column names (optional, recommended)
data <- clean_names(data)

# Inspect data structure
str(data)
summary(data)
head(data)

# ===============================
# 🧹 Step 3: Prepare Data
# ===============================
# Convert character columns to numeric
data$dice_subgroup1 <- as.numeric(data$dice_score_percent_subgroup_1)
data$dice_subgroup2 <- as.numeric(data$dice_score_percent_subgroup_2)

# Replace with actual SEs if available
data$se_sub1 <- rep(5, nrow(data))  # Placeholder
data$se_sub2 <- rep(5, nrow(data))  # Placeholder

# ===============================
# 📊 Step 4: Descriptive Statistics
# ===============================
# Frequency tables
table(data$study_design)
table(data$subgroup)

# Descriptive stats
describe(data$dice_subgroup1)
describe(data$dice_subgroup2)

# ===============================
# 📈 Step 5: Meta-Analysis
# ===============================
meta_dice1 <- metagen(
  TE = data$dice_subgroup1,
  seTE = data$se_sub1,
  studlab = data$study_id,
  data = data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  hakn = TRUE
)

meta_dice2 <- metagen(
  TE = data$dice_subgroup2,
  seTE = data$se_sub2,
  studlab = data$study_id,
  data = data,
  sm = "MD",
  comb.fixed = FALSE,
  comb.random = TRUE,
  hakn = TRUE
)

# ===============================
# 🌲 Step 6: Forest and Funnel Plots
# ===============================
forest(meta_dice1, main = "Dice Score - Subgroup 1")
forest(meta_dice2, main = "Dice Score - Subgroup 2")

funnel(meta_dice1)
funnel(meta_dice2)

# ===============================
# 🧪 Step 7: Heterogeneity Stats
# ===============================
meta_dice1$I2
meta_dice1$Q
meta_dice1$pval.Q

meta_dice2$I2
meta_dice2$Q
meta_dice2$pval.Q

# ===============================
# 🔍 Step 8: Subgroup Analysis
# ===============================
meta_subgroup <- update.meta(meta_dice1, subgroup = data$subgroup)
forest(meta_subgroup, main = "Subgroup Analysis")

# ===============================
# 📉 Step 9: Publication Bias (Egger's test)
# ===============================
metabias(meta_dice1, method.bias = "linreg")
metabias(meta_dice2, method.bias = "linreg")

# ===============================
# 🔬 Step 10: Influence Diagnostics
# ===============================
influence1 <- influence(meta_dice1)
plot(influence1)

influence2 <- influence(meta_dice2)
plot(influence2)

# ===============================
# 🔁 Step 11: Meta-Regression (if moderator available)
# ===============================
# Example: if you have `sample_size` column
# NOTE: You must have numeric moderator variable
rma_reg <- rma(
  yi = data$dice_subgroup1,
  sei = data$se_sub1,
  mods = ~ sample_size,   # change to an actual moderator
  data = data
)
summary(rma_reg)


# ===================================
# 📊 Sensitivity and Specificity Meta-Analysis for Subgroups
# ===================================

# Convert percent values to proportions
data <- data %>%
  mutate(
    sensitivity_subgroup1 = as.numeric(sensitivity_percent_subgroup_1) / 100,
    sensitivity_subgroup2 = as.numeric(sensitivity_percent_subgroup_2) / 100,
    specificity_subgroup1 = as.numeric(specificity_percent_subgroup_1) / 100,
    specificity_subgroup2 = as.numeric(specificity_percent_subgroup_2) / 100
  )

# Sensitivity - Subgroup 1
meta_sens1 <- metaprop(
  event = sensitivity_subgroup1 * 100,
  n = rep(100, nrow(data)),  # Replace with actual sample size if available
  studlab = covidence_number,
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  data = data
)

# Specificity - Subgroup 1
meta_spec1 <- metaprop(
  event = specificity_subgroup1 * 100,
  n = rep(100, nrow(data)),
  studlab = covidence_number,
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  data = data
)

# Sensitivity - Subgroup 2
meta_sens2 <- metaprop(
  event = sensitivity_subgroup2 * 100,
  n = rep(100, nrow(data)),
  studlab = covidence_number,
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  data = data
)

# Specificity - Subgroup 2
meta_spec2 <- metaprop(
  event = specificity_subgroup2 * 100,
  n = rep(100, nrow(data)),
  studlab = covidence_number,
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  data = data
)

# Forest plots
forest(meta_sens1, main = "Sensitivity - Subgroup 1")
forest(meta_spec1, main = "Specificity - Subgroup 1")
forest(meta_sens2, main = "Sensitivity - Subgroup 2")
forest(meta_spec2, main = "Specificity - Subgroup 2")

# 📌 Print overall pooled estimates (random effects model)
cat("📍 Overall Pooled Sensitivity (Subgroup 1):\n")
cat(sprintf("  %.2f%% (95%% CI: %.2f%%–%.2f%%)\n",
            meta_sens1$TE.random, meta_sens1$lower.random, meta_sens1$upper.random))

cat("📍 Overall Pooled Specificity (Subgroup 1):\n")
cat(sprintf("  %.2f%% (95%% CI: %.2f%%–%.2f%%)\n",
            meta_spec1$TE.random, meta_spec1$lower.random, meta_spec1$upper.random))

cat("📍 Overall Pooled Sensitivity (Subgroup 2):\n")
cat(sprintf("  %.2f%% (95%% CI: %.2f%%–%.2f%%)\n",
            meta_sens2$TE.random, meta_sens2$lower.random, meta_sens2$upper.random))

cat("📍 Overall Pooled Specificity (Subgroup 2):\n")
cat(sprintf("  %.2f%% (95%% CI: %.2f%%–%.2f%%)\n",
            meta_spec2$TE.random, meta_spec2$lower.random, meta_spec2$upper.random))
# ==============================================================================================
