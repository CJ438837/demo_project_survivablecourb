# ============================================
# 02_cleaning_QC.R
# Nettoyage & Quality Control
# ============================================

library(tidyverse)

# Import
clinical_data <- read_csv("D:/Programation/Demo_traitement/clinical_data.csv")

# ============================================
# 1. Vérification des valeurs manquantes
# ============================================

na_summary <- clinical_data %>%
  summarise(across(everything(),
                   ~ sum(is.na(.))))

print("Résumé des NA :")
print(na_summary)

# ============================================
# 2. Vérification des types
# ============================================

glimpse(clinical_data)

# Conversion en facteurs si nécessaire
clinical_data <- clinical_data %>%
  mutate(
    sex = as.factor(sex),
    treatment = as.factor(treatment),
    status = as.factor(status)
  )

# ============================================
# 3. Détection des outliers biomarker
# Méthode IQR
# ============================================

Q1 <- quantile(clinical_data$biomarker_baseline, 0.25)
Q3 <- quantile(clinical_data$biomarker_baseline, 0.75)
IQR_value <- IQR(clinical_data$biomarker_baseline)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- clinical_data %>%
  filter(biomarker_baseline < lower_bound |
           biomarker_baseline > upper_bound)

print("Nombre d'outliers détectés :")
print(nrow(outliers))

# ============================================
# 4. Distribution des variables continues
# ============================================

# Histogramme biomarker baseline
ggplot(clinical_data, aes(x = biomarker_baseline)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "Distribution Biomarker Baseline")

# Histogramme survival time
ggplot(clinical_data, aes(x = survival_time)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "Distribution Survival Time")

# ============================================
# 5. Test de normalité (Shapiro-Wilk)
# Attention : sensible si n > 50
# ============================================

shapiro_test <- shapiro.test(
  clinical_data$biomarker_baseline
)

print(shapiro_test)

# Transformation log si nécessaire
clinical_data <- clinical_data %>%
  mutate(
    biomarker_log = log(biomarker_baseline)
  )

shapiro_log <- shapiro.test(
  clinical_data$biomarker_log
)

print("Normalité après transformation log :")
print(shapiro_log)

# ============================================
# 6. Sauvegarde dataset clean
# ============================================

write_csv(clinical_data, "D:/Programation/Demo_traitement/clinical_data_clean.csv")
