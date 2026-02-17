# ============================================
# 05_survival_analysis_final.R
# Analyse de survie complète - KM + Cox + Forest plot robuste
# ============================================

library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(ggplot2)

# --------------------------------------------
# 1️⃣ Import dataset clean
# --------------------------------------------
clinical_data <- read_csv("D:/Programation/Demo_traitement/clinical_data_clean.csv")

# --------------------------------------------
# 2️⃣ Préparation des variables
# --------------------------------------------

clinical_data <- clinical_data %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    treatment = factor(treatment, levels = c("Placebo", "Treatment")),
    status = as.numeric(status)  # IMPORTANT pour Surv et forest plot
  )

# Vérification
str(clinical_data)
levels(clinical_data$sex)
levels(clinical_data$treatment)

# --------------------------------------------
# 3️⃣ Kaplan-Meier
# --------------------------------------------

surv_obj <- Surv(time = clinical_data$survival_time,
                 event = clinical_data$status)

km_fit <- survfit(surv_obj ~ treatment, data = clinical_data)

km_plot <- ggsurvplot(
  km_fit,
  data = clinical_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#E7B800", "#2E9FDF"),
  ggtheme = theme_minimal(),
  title = "Kaplan-Meier Survival Curves by Treatment",
  xlab = "Time (months)",
  ylab = "Survival Probability"
)

km_plot  # afficher

# --------------------------------------------
# 4️⃣ Log-rank test
# --------------------------------------------

logrank_test <- survdiff(surv_obj ~ treatment, data = clinical_data)
print("Log-rank test result:")
logrank_test

# --------------------------------------------
# 5️⃣ Modèle de Cox ajusté
# --------------------------------------------

cox_model <- coxph(Surv(survival_time, status) ~ 
                     treatment + age + sex + biomarker_baseline,
                   data = clinical_data)

summary(cox_model)

# Tableau tidy pour rapport
cox_table <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
print("Cox model tidy table:")
cox_table

# --------------------------------------------
# 6️⃣ Forest plot robuste (ggplot2)
# --------------------------------------------

# Ordre pour lecture facile
cox_table <- cox_table %>%
  mutate(term = str_replace_all(term, "`", "")) %>%
  arrange(desc(estimate))

# Forest plot
ggplot(cox_table, aes(x = reorder(term, estimate), y = estimate,
                      ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#2E9FDF", size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    x = "",
    y = "Hazard Ratio (95% CI)",
    title = "Forest plot - Cox proportional hazards model"
  ) +
  theme(axis.text.y = element_text(size = 10))
