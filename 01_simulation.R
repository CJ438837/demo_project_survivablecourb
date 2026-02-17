# ============================================
# 01_simulation.R
# Simulation d'un dataset clinique réaliste
# ============================================

set.seed(123)

library(tidyverse)

# Nombre de patients
n <- 200

# Variables démographiques
id <- 1:n
age <- round(rnorm(n, mean = 65, sd = 10))
sex <- sample(c("Male", "Female"), n, replace = TRUE)

# Randomisation
treatment <- sample(c("Placebo", "Treatment"), n, replace = TRUE)

# Biomarker baseline (distribution asymétrique)
biomarker_baseline <- rlnorm(n, meanlog = 2.5, sdlog = 0.4)

# Effet traitement sur biomarker
treatment_effect <- ifelse(treatment == "Treatment", -0.5, 0)
biomarker_post <- biomarker_baseline + 
  rnorm(n, mean = treatment_effect, sd = 0.8)

# Risque de décès influencé par :
# - âge
# - biomarker baseline
# - traitement protecteur

linear_predictor <- 0.03 * age +
  0.002 * biomarker_baseline +
  ifelse(treatment == "Treatment", -0.5, 0)

hazard <- exp(linear_predictor)

# Temps de survie simulé (exponentiel)
survival_time <- rexp(n, rate = hazard / 100)

# Censure administrative à 60 mois
status <- ifelse(survival_time > 60, 0, 1)
survival_time <- pmin(survival_time, 60)

# Dataset final
clinical_data <- tibble(
  id,
  age,
  sex,
  treatment,
  biomarker_baseline,
  biomarker_post,
  survival_time,
  status
)

# Aperçu
glimpse(clinical_data)

# Sauvegarde
write_csv(clinical_data, "D:/Programation/Demo_traitement/clinical_data.csv")

