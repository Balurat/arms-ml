
# change location of file according to your folder 
# load("../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.rda")
# 
# data_not_filtered <- da02760.0001
# 
# Using the .sav file for numeric values on each variable
library(haven)
# 
data_not_filtered <- read_sav("../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.sav")


library(dplyr)
library(stringr)


# Funktion definieren, um den Mittelwert zu berechnen, wenn mindestens x Werte (mind. 25% der Skala von Person i ausgefüllt) vorhanden sind

calculate_mean <- function(threshold, ...) {
  values <- c(...)
  values <- values[!is.na(values)]
  if (length(values) >= threshold) {
    return(mean(values))
  } else {
    return(NA)
  }
}


# Für Summenberechnung (indexialische Messung)

calculate_sum <- function(threshold, ...) {
  values <- c(...)
  values <- values[!is.na(values)]
  if (length(values) >= threshold) {
    return(sum(values))
  } else {
    return(NA)
  }
}

# Funktion, um numerischen Wert zu extrahieren

extract_numeric <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}



#### computing sum and mean scores of sclaes ####


# SUM_supplements_consumed

supplements_consumed_vector <- c("A1SA11A", "A1SA11B", "A1SA11C", "A1SA11D", "A1SA11E")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars((supplements_consumed_vector)), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA11A_transformed = case_when(
    A1SA11A_transformed == 1 ~ 1,
    A1SA11A_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA11B_transformed = case_when(
    A1SA11B_transformed == 1 ~ 1,
    A1SA11B_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA11C_transformed = case_when(
    A1SA11C_transformed == 1 ~ 1,
    A1SA11C_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA11D_transformed = case_when(
    A1SA11D_transformed == 1 ~ 1,
    A1SA11D_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA11E_transformed = case_when(
    A1SA11E_transformed == 1 ~ 1,
    A1SA11E_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(SUM_supplements_consumed = calculate_sum(2, A1SA11A_transformed, A1SA11B_transformed, A1SA11C_transformed, A1SA11D_transformed, A1SA11E_transformed))

# MEAN_physical_symptoms

physical_symptoms_vector <- c("A1SA12A", "A1SA12B", "A1SA12C", "A1SA12D", "A1SA12E", "A1SA12F", "A1SA12G", "A1SA12H", "A1SA12I")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(physical_symptoms_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12A_transformed = case_when(
    A1SA12A_transformed == 1 ~ 6,
    A1SA12A_transformed == 2 ~ 5,
    A1SA12A_transformed == 3 ~ 4,
    A1SA12A_transformed == 4 ~ 3,
    A1SA12A_transformed == 5 ~ 2,
    A1SA12A_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12B_transformed = case_when(
    A1SA12B_transformed == 1 ~ 6,
    A1SA12B_transformed == 2 ~ 5,
    A1SA12B_transformed == 3 ~ 4,
    A1SA12B_transformed == 4 ~ 3,
    A1SA12B_transformed == 5 ~ 2,
    A1SA12B_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12C_transformed = case_when(
    A1SA12C_transformed == 1 ~ 6,
    A1SA12C_transformed == 2 ~ 5,
    A1SA12C_transformed == 3 ~ 4,
    A1SA12C_transformed == 4 ~ 3,
    A1SA12C_transformed == 5 ~ 2,
    A1SA12C_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12D_transformed = case_when(
    A1SA12D_transformed == 1 ~ 6,
    A1SA12D_transformed == 2 ~ 5,
    A1SA12D_transformed == 3 ~ 4,
    A1SA12D_transformed == 4 ~ 3,
    A1SA12D_transformed == 5 ~ 2,
    A1SA12D_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12E_transformed = case_when(
    A1SA12E_transformed == 1 ~ 6,
    A1SA12E_transformed == 2 ~ 5,
    A1SA12E_transformed == 3 ~ 4,
    A1SA12E_transformed == 4 ~ 3,
    A1SA12E_transformed == 5 ~ 2,
    A1SA12E_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate( A1SA12F_transformed = case_when(
    A1SA12F_transformed == 1 ~ 6,
    A1SA12F_transformed == 2 ~ 5,
    A1SA12F_transformed == 3 ~ 4,
    A1SA12F_transformed == 4 ~ 3,
    A1SA12F_transformed == 5 ~ 2,
    A1SA12F_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12G_transformed = case_when(
    A1SA12G_transformed == 1 ~ 6,
    A1SA12G_transformed == 2 ~ 5,
    A1SA12G_transformed == 3 ~ 4,
    A1SA12G_transformed == 4 ~ 3,
    A1SA12G_transformed == 5 ~ 2,
    A1SA12G_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12H_transformed = case_when(
    A1SA12H_transformed == 1 ~ 6,
    A1SA12H_transformed == 2 ~ 5,
    A1SA12H_transformed == 3 ~ 4,
    A1SA12H_transformed == 4 ~ 3,
    A1SA12H_transformed == 5 ~ 2,
    A1SA12H_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA12I_transformed = case_when(
    A1SA12I_transformed == 1 ~ 6,
    A1SA12I_transformed == 2 ~ 5,
    A1SA12I_transformed == 3 ~ 4,
    A1SA12I_transformed == 4 ~ 3,
    A1SA12I_transformed == 5 ~ 2,
    A1SA12I_transformed == 6 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_physical_symptoms = calculate_mean(3, A1SA12A_transformed, A1SA12B_transformed, A1SA12C_transformed, A1SA12D_transformed, A1SA12E_transformed, A1SA12F_transformed, A1SA12G_transformed, A1SA12H_transformed, A1SA12I_transformed))

# MEAN_health_limits

health_limits_vector <- c("A1SA17A", "A1SA17B", "A1SA17C", "A1SA17D", "A1SA17E", "A1SA17F", "A1SA17G", "A1SA17H", "A1SA17I")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(health_limits_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17A_transformed = case_when(
    A1SA17A_transformed == 1 ~ 4,
    A1SA17A_transformed == 2 ~ 3,
    A1SA17A_transformed == 3 ~ 2,
    A1SA17A_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17B_transformed = case_when(
    A1SA17B_transformed == 1 ~ 4,
    A1SA17B_transformed == 2 ~ 3,
    A1SA17B_transformed == 3 ~ 2,
    A1SA17B_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17C_transformed = case_when(
    A1SA17C_transformed == 1 ~ 4,
    A1SA17C_transformed == 2 ~ 3,
    A1SA17C_transformed == 3 ~ 2,
    A1SA17C_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17D_transformed = case_when(
    A1SA17D_transformed == 1 ~ 4,
    A1SA17D_transformed == 2 ~ 3,
    A1SA17D_transformed == 3 ~ 2,
    A1SA17D_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17E_transformed = case_when(
    A1SA17E_transformed == 1 ~ 4,
    A1SA17E_transformed == 2 ~ 3,
    A1SA17E_transformed == 3 ~ 2,
    A1SA17E_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17F_transformed = case_when(
    A1SA17F_transformed == 1 ~ 4,
    A1SA17F_transformed == 2 ~ 3,
    A1SA17F_transformed == 3 ~ 2,
    A1SA17F_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17G_transformed = case_when(
    A1SA17G_transformed == 1 ~ 4,
    A1SA17G_transformed == 2 ~ 3,
    A1SA17G_transformed == 3 ~ 2,
    A1SA17G_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17H_transformed = case_when(
    A1SA17H_transformed == 1 ~ 4,
    A1SA17H_transformed == 2 ~ 3,
    A1SA17H_transformed == 3 ~ 2,
    A1SA17H_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA17I_transformed = case_when(
    A1SA17I_transformed == 1 ~ 4,
    A1SA17I_transformed == 2 ~ 3,
    A1SA17I_transformed == 3 ~ 2,
    A1SA17I_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_health_limits = calculate_mean(3, A1SA17A_transformed, A1SA17B_transformed, A1SA17C_transformed, A1SA17D_transformed, A1SA17E_transformed, A1SA17F_transformed, A1SA17G_transformed, A1SA17H_transformed, A1SA17I_transformed))

# SUM_drugs_used

drugs_used_vector <- c("A1SA40A", "A1SA40B", "A1SA40C", "A1SA40D", "A1SA40E", "A1SA40F", "A1SA40G", "A1SA40H", "A1SA40I", "A1SA40J")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(drugs_used_vector), list(transformed = ~extract_numeric(.)))

# recode scores

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40A_transformed = case_when(
    A1SA40A_transformed == 1 ~ 1,
    A1SA40A_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40B_transformed = case_when(
    A1SA40B_transformed == 1 ~ 1,
    A1SA40B_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40C_transformed = case_when(
    A1SA40C_transformed == 1 ~ 1,
    A1SA40C_transformed == 2 ~ 0,
    TRUE ~ NA
  ))
data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40D_transformed = case_when(
    A1SA40D_transformed == 1 ~ 1,
    A1SA40D_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40E_transformed = case_when(
    A1SA40E_transformed == 1 ~ 1,
    A1SA40E_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40F_transformed = case_when(
    A1SA40F_transformed == 1 ~ 1,
    A1SA40F_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40G_transformed = case_when(
    A1SA40G_transformed == 1 ~ 1,
    A1SA40G_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40H_transformed = case_when(
    A1SA40H_transformed == 1 ~ 1,
    A1SA40H_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40I_transformed = case_when(
    A1SA40I_transformed == 1 ~ 1,
    A1SA40I_transformed == 2 ~ 0,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SA40J_transformed = case_when(
    A1SA40J_transformed == 1 ~ 1,
    A1SA40J_transformed == 2 ~ 0,
    TRUE ~ NA
  ))


data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(SUM_drugs_used = calculate_sum(3, A1SA40A_transformed, A1SA40B_transformed, A1SA40C_transformed, A1SA40D_transformed, A1SA40E_transformed, A1SA40F_transformed, A1SA40G_transformed, A1SA40H_transformed, A1SA40I_transformed, A1SA40J_transformed))

# MEAN_maternal_affection

maternal_affection_vector <- c("A1SE14A", "A1SE14B", "A1SE14C", "A1SE14D", "A1SE14E", "A1SE14K")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(maternal_affection_vector), list(transformed = ~extract_numeric(.)))

# redoce scores

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14A_transformed = case_when(
    A1SE14A_transformed == 1 ~ 4,
    A1SE14A_transformed == 2 ~ 3,
    A1SE14A_transformed == 3 ~ 2,
    A1SE14A_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14B_transformed = case_when(
    A1SE14B_transformed == 1 ~ 4,
    A1SE14B_transformed == 2 ~ 3,
    A1SE14B_transformed == 3 ~ 2,
    A1SE14B_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14C_transformed = case_when(
    A1SE14C_transformed == 1 ~ 4,
    A1SE14C_transformed == 2 ~ 3,
    A1SE14C_transformed == 3 ~ 2,
    A1SE14C_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14D_transformed = case_when(
    A1SE14D_transformed == 1 ~ 4,
    A1SE14D_transformed == 2 ~ 3,
    A1SE14D_transformed == 3 ~ 2,
    A1SE14D_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14E_transformed = case_when(
    A1SE14E_transformed == 1 ~ 4,
    A1SE14E_transformed == 2 ~ 3,
    A1SE14E_transformed == 3 ~ 2,
    A1SE14E_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14K_transformed = case_when(
    A1SE14K_transformed == 1 ~ 4,
    A1SE14K_transformed == 2 ~ 3,
    A1SE14K_transformed == 3 ~ 2,
    A1SE14K_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_maternal_affection = calculate_mean(2, A1SE14A_transformed, A1SE14B_transformed, A1SE14C_transformed, A1SE14D_transformed, A1SE14E_transformed, A1SE14K_transformed))

# MEAN_maternal_discipline

maternal_discipline_vector <- c("A1SE14F", "A1SE14G", "A1SE14H", "A1SE14I")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(maternal_discipline_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14F_transformed = case_when(
    A1SE14F_transformed == 1 ~ 4,
    A1SE14F_transformed == 2 ~ 3,
    A1SE14F_transformed == 3 ~ 2,
    A1SE14F_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14G_transformed = case_when(
    A1SE14G_transformed == 1 ~ 4,
    A1SE14G_transformed == 2 ~ 3,
    A1SE14G_transformed == 3 ~ 2,
    A1SE14G_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14H_transformed = case_when(
    A1SE14H_transformed == 1 ~ 4,
    A1SE14H_transformed == 2 ~ 3,
    A1SE14H_transformed == 3 ~ 2,
    A1SE14H_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14I_transformed = case_when(
    A1SE14I_transformed == 1 ~ 4,
    A1SE14I_transformed == 2 ~ 3,
    A1SE14I_transformed == 3 ~ 2,
    A1SE14I_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_maternal_discipline = calculate_mean(1, A1SE14F_transformed, A1SE14G_transformed, A1SE14H_transformed, A1SE14I_transformed))

# MEAN_maternal_model_of_generosity

maternal_model_of_generosity_vector <- c("A1SE14L", "A1SE14M")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(maternal_model_of_generosity_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14L_transformed = case_when(
    A1SE14L_transformed == 1 ~ 4,
    A1SE14L_transformed == 2 ~ 3,
    A1SE14L_transformed == 3 ~ 2,
    A1SE14L_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SE14M_transformed = case_when(
    A1SE14M_transformed == 1 ~ 4,
    A1SE14M_transformed == 2 ~ 3,
    A1SE14M_transformed == 3 ~ 2,
    A1SE14M_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_maternal_model_of_generosity = calculate_mean(1, A1SE14L_transformed, A1SE14M_transformed))

# MEAN_stereotypical_attitudes_family

stereotypical_attitudes_family_vector <- c("A1SF2A", "A1SF2B", "A1SF2C", "A1SF2D", "A1SF2E", "A1SF2F", "A1SF2G", "A1SF2H", "A1SF2I", "A1SF2J", "A1SF2K")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(stereotypical_attitudes_family_vector), list(transformed = ~extract_numeric(.)))

# redoce scores (only 3 items recoded)


data_not_filtered <- data_not_filtered %>%
  mutate(A1SF2C_transformed = case_when(
    A1SF2C_transformed == 1 ~ 7,
    A1SF2C_transformed == 2 ~ 6,
    A1SF2C_transformed == 3 ~ 5,
    A1SF2C_transformed == 4 ~ 4,
    A1SF2C_transformed == 5 ~ 3,
    A1SF2C_transformed == 6 ~ 2,
    A1SF2C_transformed == 7 ~ 1,
    TRUE ~ NA
  ))


data_not_filtered <- data_not_filtered %>%
  mutate(A1SF2H_transformed = case_when(
    A1SF2H_transformed == 1 ~ 7,
    A1SF2H_transformed == 2 ~ 6,
    A1SF2H_transformed == 3 ~ 5,
    A1SF2H_transformed == 4 ~ 4,
    A1SF2H_transformed == 5 ~ 3,
    A1SF2H_transformed == 6 ~ 2,
    A1SF2H_transformed == 7 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SF2I_transformed = case_when(
    A1SF2I_transformed == 1 ~ 7,
    A1SF2I_transformed == 2 ~ 6,
    A1SF2I_transformed == 3 ~ 5,
    A1SF2I_transformed == 4 ~ 4,
    A1SF2I_transformed == 5 ~ 3,
    A1SF2I_transformed == 6 ~ 2,
    A1SF2I_transformed == 7 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_stereotypical_attitudes_family = calculate_mean(3, A1SF2A, A1SF2B, A1SF2C_transformed, A1SF2D, A1SF2E, A1SF2F, A1SF2G, A1SF2H_transformed, A1SF2I_transformed, A1SF2J, A1SF2K))

# MEAN_stress_at_home

stress_at_home_vector <- c("A1SI32A", "A1SI32B", "A1SI32C", "A1SI32D")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(stress_at_home_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SI32A_transformed = case_when(
    A1SI32A_transformed == 1 ~ 5,
    A1SI32A_transformed == 2 ~ 4,
    A1SI32A_transformed == 3 ~ 3,
    A1SI32A_transformed == 4 ~ 2,
    A1SI32A_transformed == 5 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SI32D_transformed = case_when(
    A1SI32D_transformed == 1 ~ 5,
    A1SI32D_transformed == 2 ~ 4,
    A1SI32D_transformed == 3 ~ 3,
    A1SI32D_transformed == 4 ~ 2,
    A1SI32D_transformed == 5 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_stress_at_home = calculate_mean(1, A1SI32A_transformed, A1SI32B, A1SI32C, A1SI32D_transformed))

# MEAN_rewards_at_work 

rewards_at_work_vector <- c("A1SI33A", "A1SI33B", "A1SI33C") 

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(rewards_at_work_vector), list(transformed = ~extract_numeric(.)))

# recode scores 

data_not_filtered <- data_not_filtered %>%
  mutate(A1SI33A_transformed = case_when(
    A1SI33A_transformed == 1 ~ 4,
    A1SI33A_transformed == 2 ~ 3,
    A1SI33A_transformed == 3 ~ 2,
    A1SI33A_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SI33B_transformed = case_when(
    A1SI33B_transformed == 1 ~ 4,
    A1SI33B_transformed == 2 ~ 3,
    A1SI33B_transformed == 3 ~ 2,
    A1SI33B_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SI33C_transformed = case_when(
    A1SI33C_transformed == 1 ~ 4,
    A1SI33C_transformed == 2 ~ 3,
    A1SI33C_transformed == 3 ~ 2,
    A1SI33C_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_rewards_at_work = calculate_mean(1, A1SI33A_transformed, A1SI33B_transformed, A1SI33C_transformed))

# SUM_social_group_meetings

# no transformation needed

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(SUM_social_group_meetings = calculate_sum(2, A1SK9A, A1SK9B, A1SK9C, A1SK9D, A1SK9E))

# MEAN_religiousness

religiousness_vector <- c("A1SR2A", "A1SR2B", "A1SR2C", "A1SR2D", "A1SR2E", "A1SR2F", "A1SR2G", "A1SR2H")

data_not_filtered <- data_not_filtered %>%
  mutate_at(vars(religiousness_vector), list(transformed = ~extract_numeric(.)))

# recode values

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2A_transformed = case_when(
    A1SR2A_transformed == 1 ~ 4,
    A1SR2A_transformed == 2 ~ 3,
    A1SR2A_transformed == 3 ~ 2,
    A1SR2A_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2B_transformed = case_when(
    A1SR2B_transformed == 1 ~ 4,
    A1SR2B_transformed == 2 ~ 3,
    A1SR2B_transformed == 3 ~ 2,
    A1SR2B_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2C_transformed = case_when(
    A1SR2C_transformed == 1 ~ 4,
    A1SR2C_transformed == 2 ~ 3,
    A1SR2C_transformed == 3 ~ 2,
    A1SR2C_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2D_transformed = case_when(
    A1SR2D_transformed == 1 ~ 4,
    A1SR2D_transformed == 2 ~ 3,
    A1SR2D_transformed == 3 ~ 2,
    A1SR2D_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2E_transformed = case_when(
    A1SR2E_transformed == 1 ~ 4,
    A1SR2E_transformed == 2 ~ 3,
    A1SR2E_transformed == 3 ~ 2,
    A1SR2E_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2F_transformed = case_when(
    A1SR2F_transformed == 1 ~ 4,
    A1SR2F_transformed == 2 ~ 3,
    A1SR2F_transformed == 3 ~ 2,
    A1SR2F_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2G_transformed = case_when(
    A1SR2G_transformed == 1 ~ 4,
    A1SR2G_transformed == 2 ~ 3,
    A1SR2G_transformed == 3 ~ 2,
    A1SR2G_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  mutate(A1SR2H_transformed = case_when(
    A1SR2H_transformed == 1 ~ 4,
    A1SR2H_transformed == 2 ~ 3,
    A1SR2H_transformed == 3 ~ 2,
    A1SR2H_transformed == 4 ~ 1,
    TRUE ~ NA
  ))

data_not_filtered <- data_not_filtered %>%
  rowwise() %>%
  mutate(MEAN_religiousness = calculate_mean(2, A1SR2A_transformed, A1SR2B_transformed, A1SR2C_transformed, A1SR2D_transformed, A1SR2E_transformed, A1SR2F_transformed, A1SR2G_transformed, A1SR2H_transformed))

#### create 'time' and 'censor' variables #### 

# exclude participants with missings in bearth year 


data_not_filtered <- data_not_filtered %>% filter(!is.na(A1PBYEAR_2019))

# change location of file according to your folder 
mortality <- read_sav("../common/MIDUS/MIDUS_Core_MortalityCauseData_N2459_20230531.sav")

# merge MIDUS1 and mortality data 

merged_data <- merge(data_not_filtered, mortality, by = "M2ID", all.x = TRUE)

# compute values for 'time' and 'censor'

merged_data <- merged_data %>%
  mutate(
    censor = is.na(DOD_Y),
    time = ifelse(censor, 2023 - A1PBYEAR_2019, DOD_Y - A1PBYEAR_2019),
    MARRIED_AGE = ifelse(
      is.na(A1PB18YR), NA, A1PB18YR - A1PBYEAR_2019
    )
  )


# create dataframe with selected variables for final survival analysis


selected_variables <- c(
  "time", "censor", "A1PAGE_M2", "A1PRSEX", "A1PA4", "A1PA5", "A1PA6", "A1PA7", "A1PA11", "A1PA13", "A1PHRTRS", 
  "A1PA14", "A1PA15", "A1PA16", "A1PA17", "A1PA18", "A1PA23", "A1PANGIN", "A1PA29", "A1PA33", 
  "A1PA34", "A1PA30", "A1PA31", "A1PA36", "A1PA37", "A1PCACRS", "A1PA52", "A1PA53", "A1PA54", 
  "A1PA55", "A1PA56", "A1PA57", "A1PA80", "A1PA81", "A1PA87", "A1PEDUCP", "A1PB2", "A1PB3A", 
  "A1PB3B", "A1PB3E", "A1PB3F", "A1PB17", "A1PB19", "A1PB35", "A1PB37", "A1PC1", 
  "A1PD1", "A1PD2", "A1PD3", "A1PD8", "A1PD9A", "A1SA1", "A1SHLTEX", "A1SA4", "A1SA5", "A1SSATIS", 
  "A1SHLOCS", "A1SHLOCO", "A1SAMOLI", "A1SCHRON", "A1SRXMED", "SUM_supplements_consumed", 
  "MEAN_physical_symptoms", "A1SNEGAF", "A1SA14", "A1SPOSAF", "A1SA16", "MEAN_health_limits", 
  "A1SVIGOR", "A1SMODER", "A1SDYSPN", "A1SA25", "A1SA26", "A1SA27", "A1SBMI", "A1SA31", "A1SA33", 
  "A1SUSEMD", "A1SA37B", "A1SUSEMH", "A1SA38A", "A1SALTER", "SUM_drugs_used", "A1SA44A", "A1SA45", 
  "A1SA46", "A1SD1", "A1SE3", "A1SE4", "A1SE5", "A1SE7", "A1SE8", "A1SE9", "A1SE11", "A1SE12", 
  "A1SE13", "MEAN_maternal_affection", "MEAN_maternal_discipline", "MEAN_maternal_model_of_generosity", 
  "A1SPWBA", "A1SPWBE", "A1SPWBG", "A1SPWBR", "A1SPWBU", "A1SPWBS", "A1SCONST", "A1SMASTE", 
  "MEAN_stereotypical_attitudes_family", "A1SPERSI", "A1SCHANG", "A1SREAPP", "A1SDIREC", "A1STODAY", 
  "A1SFORSG", "A1SINSGH", "A1SSUFFI", "A1SADVIC", "A1SAGENC", "A1SAGREE", "A1SEXTRA", "A1SNEURO", 
  "A1SCONS", "A1SOPEN", "A1SI2", "A1SI5", "A1SI6", "MEAN_stress_at_home", "MEAN_rewards_at_work", 
  "A1SJ1", "A1SJ4", "A1SJ5", "A1SJ6", "A1SJ7", "A1SHWEARN", "A1SHHTOT", "A1SGENER", "A1SPRIOB", 
  "A1SCVOB3", "A1SCVOB5", "A1SWKOB", "A1SALTRU", "SUM_social_group_meetings", "A1SPSUPE", "A1SRSUPE", 
  "A1SPSUPI", "A1SSWBMS", "A1SSWBSI", "A1SSWBAO", "A1SSWBSC", "A1SSWBSA", "A1SL1", "A1SL2", "A1SL3", 
  "A1SL4", "A1SHOMET", "A1SPIHOM", "A1SM1", "A1SFAMSO", "A1SKINPO", "A1SKINNE", "A1SM10", "A1SFDSOL", 
  "A1SFDSPO", "A1SFDSNE", "A1SQ1", "A1SQ4", "A1SQ5", "A1SQ6", "A1SQ8", "A1SR1", "MEAN_religiousness", 
  "A1SR3", "A1SR4", "A1SR5", "A1SR6", "A1SS8", "A1SS9", "A1SS10", "A1SS12", "A1SLFEDI", "A1SDAYDI", 
  "A1ST1", "A1ST4", "A1ST5", "MARRIED_AGE"
)

data_final_analysis <- subset(merged_data, select = selected_variables)

save(data_final_analysis, file = "./data_final_analysis.rda")

#### final feature engineering ####


