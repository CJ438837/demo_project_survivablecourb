# ============================================
# 03_baseline_table.R
# Table 1 baseline characteristics
# ============================================

library(tidyverse)
library(gtsummary)
library(gt)

# Import dataset clean
clinical_data <- read_csv("D:/Programation/Demo_traitement/clinical_data_clean.csv")

# Conversion en facteurs si nécessaire
clinical_data <- clinical_data %>%
  mutate(
    sex = as.factor(sex),
    treatment = as.factor(treatment),
    status = as.factor(status)
  )

# ============================================
# Table 1 baseline
# ============================================

table1 <- clinical_data %>%
  select(age, sex, biomarker_baseline, treatment) %>%
  tbl_summary(
    by = treatment,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p() %>%                # p-values pour comparaison
  add_overall() %>%          # colonne total
  modify_header(label = "**Variable**") %>%
  modify_caption("**Table 1. Baseline characteristics by treatment group**")

# Visualiser dans RStudio
table1 %>% as_gt()

# ============================================
# Sauvegarde en HTML (publication-ready)
# ============================================

gtsave(table1 %>% as_gt(), "D:/Programation/Demo_traitement/Table1_baseline.html")

# Optionnel : sauvegarde en PDF si LaTeX installé
# gtsave(table1 %>% as_gt(), "outputs/figures/Table1_baseline.pdf")
