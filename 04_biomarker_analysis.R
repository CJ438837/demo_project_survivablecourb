# ============================================
# 04_biomarker_analysis.R
# Analyse de l'effet du traitement sur le biomarker
# ============================================

library(tidyverse)
library(gtsummary)
library(ggpubr)
library(broom)

# Import dataset clean
clinical_data <- read_csv("D:/Programation/Demo_traitement/clinical_data_clean.csv")

# ============================================
# 1️⃣ Vérification distribution log
# ============================================

ggplot(clinical_data, aes(x = treatment, y = biomarker_baseline)) +
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Boxplot biomarker baseline par traitement",
       y = "Biomarker baseline",
       x = "Groupe")

# ============================================
# 2️⃣ Test statistique : t-test ou Wilcoxon
# ============================================

# Test normalité par groupe
shapiro_placebo <- shapiro.test(clinical_data$biomarker_baseline[clinical_data$treatment=="Placebo"])
shapiro_treat <- shapiro.test(clinical_data$biomarker_baseline[clinical_data$treatment=="Treatment"])

print(shapiro_placebo)
print(shapiro_treat)

# Si normalité ok → t-test
t_test_result <- t.test(biomarker_baseline ~ treatment, data = clinical_data)
print(t_test_result)

# Sinon → Wilcoxon
wilcox_result <- wilcox.test(biomarker_baseline ~ treatment, data = clinical_data)
print(wilcox_result)

# ============================================
# 3️⃣ Modèle linéaire ajusté âge & sexe
# ============================================

lm_model <- lm(biomarker_baseline ~ treatment + age + sex, data = clinical_data)
summary(lm_model)

# tidy pour tableau propre
lm_table <- broom::tidy(lm_model, conf.int = TRUE)
print(lm_table)

# ============================================
# 4️⃣ Visualisation publication-ready
# ============================================

ggplot(clinical_data, aes(x = treatment, y = biomarker_baseline)) +
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Effet du traitement sur le biomarker",
    y = "Biomarker baseline",
    x = "Groupe"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black")
