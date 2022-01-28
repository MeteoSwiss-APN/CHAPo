#### CALCULATION OF THE KAPPA METRIC

# load dependencies
library(purrr)
library(caret)
library(dplyr)

# Import daily measurements from the model and observations
load("/users/sadamov/RProjects/CHAPo/vignettes/data_daily_comp.Rdata")

# Define buckets according to: https://service.meteoswiss.ch/confluence/x/1ZG4
data_valid <- map(data_daily_comp, ~ .x %>%
  mutate(
    conc_obs = case_when(
      taxon == "Alnus" & obs < 1 ~ "nothing",
      taxon == "Alnus" & obs >= 1 & obs <= 10 ~ "weak",
      taxon == "Alnus" & obs >= 11 & obs <= 69 ~ "medium",
      taxon == "Alnus" & obs >= 70 & obs <= 249 ~ "strong",
      taxon == "Alnus" & obs >= 250 ~ "verystrong",
      taxon == "Corylus" & obs < 1 ~ "nothing",
      taxon == "Corylus" & obs >= 1 & obs <= 10 ~ "weak",
      taxon == "Corylus" & obs >= 11 & obs <= 69 ~ "medium",
      taxon == "Corylus" & obs >= 70 & obs <= 249 ~ "strong",
      taxon == "Corylus" & obs >= 250 ~ "verystrong",
      taxon == "Betula" & obs < 1 ~ "nothing",
      taxon == "Betula" & obs >= 1 & obs <= 10 ~ "weak",
      taxon == "Betula" & obs >= 11 & obs <= 69 ~ "medium",
      taxon == "Betula" & obs >= 70 & obs <= 299 ~ "strong",
      taxon == "Betula" & obs >= 300 ~ "verystrong",
      taxon == "Poaceae" & obs < 1 ~ "nothing",
      taxon == "Poaceae" & obs >= 1 & obs <= 19 ~ "weak",
      taxon == "Poaceae" & obs >= 20 & obs <= 49 ~ "medium",
      taxon == "Poaceae" & obs >= 50 & obs <= 149 ~ "strong",
      taxon == "Poaceae" & obs >= 150 ~ "verystrong",
      taxon == "Ambrosia" & obs < 1 ~ "nothing",
      taxon == "Ambrosia" & obs >= 1 & obs <= 5 ~ "weak",
      taxon == "Ambrosia" & obs >= 6 & obs <= 10 ~ "medium",
      taxon == "Ambrosia" & obs >= 11 & obs <= 39 ~ "strong",
      taxon == "Ambrosia" & obs >= 40 ~ "verystrong"
    ),
    conc_value = case_when(
      taxon == "Alnus" & value < 1 ~ "nothing",
      taxon == "Alnus" & value >= 1 & value <= 10 ~ "weak",
      taxon == "Alnus" & value >= 11 & value <= 69 ~ "medium",
      taxon == "Alnus" & value >= 70 & value <= 249 ~ "strong",
      taxon == "Alnus" & value >= 250 ~ "verystrong",
      taxon == "Corylus" & value < 1 ~ "nothing",
      taxon == "Corylus" & value >= 1 & value <= 10 ~ "weak",
      taxon == "Corylus" & value >= 11 & value <= 69 ~ "medium",
      taxon == "Corylus" & value >= 70 & value <= 249 ~ "strong",
      taxon == "Corylus" & value >= 250 ~ "verystrong",
      taxon == "Betula" & value < 1 ~ "nothing",
      taxon == "Betula" & value >= 1 & value <= 10 ~ "weak",
      taxon == "Betula" & value >= 11 & value <= 69 ~ "medium",
      taxon == "Betula" & value >= 70 & value <= 299 ~ "strong",
      taxon == "Betula" & value >= 300 ~ "verystrong",
      taxon == "Poaceae" & value < 1 ~ "nothing",
      taxon == "Poaceae" & value >= 1 & value <= 19 ~ "weak",
      taxon == "Poaceae" & value >= 20 & value <= 49 ~ "medium",
      taxon == "Poaceae" & value >= 50 & value <= 149 ~ "strong",
      taxon == "Poaceae" & value >= 150 ~ "verystrong",
      taxon == "Ambrosia" & value < 1 ~ "nothing",
      taxon == "Ambrosia" & value >= 1 & value <= 5 ~ "weak",
      taxon == "Ambrosia" & value >= 6 & value <= 10 ~ "medium",
      taxon == "Ambrosia" & value >= 11 & value <= 39 ~ "strong",
      taxon == "Ambrosia" & value >= 40 ~ "verystrong"
    )
  ) %>%
  mutate_at(
    vars(conc_obs, conc_value),
    ~ factor(., levels = c("nothing", "weak", "medium", "strong", "verystrong"))) %>%
    # Days without pollen (measurements) are excluded as they are numerous and less important.
    filter(conc_obs != "nothing"))

# Calculate various comparison metrics
confusion_matrix <- map(data_valid, ~ confusionMatrix(.x$conc_value, .x$conc_obs))

# Retrieve the Kappa metric
metrics_categoric  <- confusion_matrix %>%
  map(~ .x$overall[2]) %>%
  bind_rows() %>%
  mutate(model = names(confusion_matrix))

# Display the Kappa metric
metrics_categoric
