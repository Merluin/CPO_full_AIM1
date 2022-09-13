###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_full_AMIM1
#
#  Update:      06/09/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
set.seed(seed)

# Loading Model -----------------------------------------------------------

fit_list <- load_models("models/intensity")

# Model Summary -----------------------------------------------------------

tidy_list_fit <- lapply(fit_list, tidy_brm)
tidy_list_priors <- lapply(fit_list, tidy_priors)
fit_info <- lapply(fit_list, get_model_info)

# Posterior Draws ---------------------------------------------------------

# fit_ri_int

data_grid_fit_ri_int <- expand_grid(
  group = unique(fit_list$fit_ri_int$data$group),
  emotion = unique(fit_list$fit_ri_int$data$emotion),
  intensity = unique(fit_list$fit_ri_int$data$intensity)
)

# getting posterior predictions

post_fit_ri_int <- epred_draws(fit_list$fit_ri_int, newdata = data_grid_fit_ri_int,
                               re_formula = NA)

post_fit_ri_int <- post_fit_ri_int %>% 
  rename("int" = .epred)

# computing relevant posterior transformations, group_ratio and group_diff
# difference = moebius - online

post_fit_ri_diff_group <- post_fit_ri_int %>%
  ungroup() %>%
  select(group, emotion, intensity, int, .draw, -.row) %>%
  pivot_wider(names_from = group, values_from = int) %>%
  mutate(int_diff = moebius - online)

# Adding Information Criteria ---------------------------------------------

fit_list$fit_ri_int <- add_criterion(fit_list$fit_ri_int, "loo")


loo_list <- list(fit_ri_int = fit_list$fit_ri_int$criteria$loo)

# Bayes Factor ------------------------------------------------------------

# bf_list <- list(
#   tas = hypothesis(fit_list$fit_ri_tas_group, "tas = 0"),
#   tas_group = hypothesis(fit_list$fit_ri_tas_group, "group_e1:tas = 0"),
#   tas_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "tas = 0"),
#   tas_group_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "group_e1:tas = 0"),
#   aq = hypothesis(fit_list$fit_ri_aq_group, "aq = 0"),
#   aq_group = hypothesis(fit_list$fit_ri_aq_group, "group_e1:aq = 0"),
#   aq_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "aq = 0"),
#   aq_group_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "group_e1:aq = 0")
# )

# Saving ------------------------------------------------------------------

intensity <- list(
  fit_info = fit_info,
  tidy_fit = tidy_list_fit,
  priors = tidy_list_priors,
  tidy_post = list(post_fit_ri_int = post_fit_ri_int,
                   post_fit_ri_diff_group = post_fit_ri_diff_group),
  loo = loo_list#,
  # bf = bf_list
)

saveRDS(intensity, file = file.path("objects", "intensity_objects.rds"))
#################################################
# 
# END
#
#################################################