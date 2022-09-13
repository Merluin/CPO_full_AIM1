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

library(tidyr)
library(dplyr)
library(brms)
library(tidybayes)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
set.seed(seed)

# Loading Model -----------------------------------------------------------

# Loading Model -----------------------------------------------------------

fit_list <- load_models("models/theta")

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

# getting posterior predictions. Here the inverse of the link functions are applied
# tan-half for mu and exp for kappa

post_fit_ri_int <- epred_draws(fit_list$fit_ri_int, newdata = data_grid_fit_ri_int,
                               re_formula = NA,
                               dpar = "kappa")

# getting posterior in degrees. Given that is a linear transformation, results are
# conceptually the same

post_fit_ri_int$angle <- rad_to_deg(post_fit_ri_int$.epred)


post_fit_ri_int <- post_fit_ri_int %>% 
  rename("theta" = .epred) %>% 
  mutate(kappa_inv = kappa_to_var(kappa)) # inverse of concentration

# computing relevant posterior transformations, group_ratio and group_diff
# the ratio is moebius - online
# the difference is moebius - online

post_fit_ri_diff_group <- post_fit_ri_int %>%
  ungroup() %>%
  select(group, emotion, intensity, angle, kappa, kappa_inv, .draw) %>%
  pivot_wider(names_from = group, values_from = c(kappa, angle, kappa_inv)) %>%
  mutate(kappa_inv_ratio = kappa_inv_moebius/kappa_inv_online,
         kappa_log_ratio_inv = log(kappa_inv_ratio),
         kappa_ratio = kappa_moebius/kappa_online,
         kappa_log_ratio = log(kappa_ratio),
         angle_diff = angle_moebius - angle_online)

post_fit_ri_diff_int <- post_fit_ri_int %>%
  ungroup() %>%
  select(group, emotion, intensity, angle, kappa, kappa_inv, .draw) %>%
  pivot_wider(names_from = intensity, values_from = c(kappa, angle, kappa_inv)) %>%
  mutate(kappa_inv_ratio = kappa_inv_full/kappa_inv_subtle,
         kappa_log_ratio_inv = log(kappa_inv_ratio),
         kappa_ratio = kappa_full/kappa_subtle,
         kappa_log_ratio = log(kappa_ratio),
         angle_diff = angle_full - angle_subtle)

# Adding Information Criteria ---------------------------------------------

fit_list$fit_ri_int <- add_criterion(fit_list$fit_ri_int, "loo", ndraws = 5000, force_save = TRUE)
fit_list$fit_ri_no3int <- add_criterion(fit_list$fit_ri_no3int, "loo", ndraws = 5000, force_save = TRUE)

int_effect_weights <- model_weights(fit_list$fit_ri_int, fit_list$fit_ri_no3int, ndraws = 5000)

int_effect_loo_diff <- loo_compare(fit_list$fit_ri_int, fit_list$fit_ri_no3int)

loo_list <- list(
  fit_ri_int = fit_list$fit_ri_int$criteria$loo,
  fit_ri_no3int = fit_list$fit_ri_no3int$criteria$loo,
  weights = int_effect_weights,
  diff = int_effect_loo_diff
)

# Bayes Factor ------------------------------------------------------------
# 
# bf_list <- list(
#   bias = list(
#     tas = hypothesis(fit_list$fit_ri_tas_group, "tas = 0"),
#     tas_group = hypothesis(fit_list$fit_ri_tas_group, "group_e1:tas = 0"),
#     tas_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "tas = 0"),
#     tas_group_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "group_e1:tas = 0"),
#     aq = hypothesis(fit_list$fit_ri_aq_group, "aq = 0"),
#     aq_group = hypothesis(fit_list$fit_ri_aq_group, "group_e1:aq = 0"),
#     aq_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "aq = 0"),
#     aq_group_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "group_e1:aq = 0")
#   ),
#   uncertainty = list(
#     tas = hypothesis(fit_list$fit_ri_tas_group, "kappa_tas = 0"),
#     tas_group = hypothesis(fit_list$fit_ri_tas_group, "kappa_group_e1:tas = 0"),
#     tas_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "kappa_tas = 0"),
#     tas_group_subtle = hypothesis(fit_list$fit_ri_tas_group_subtle, "kappa_group_e1:tas = 0"),
#     aq = hypothesis(fit_list$fit_ri_aq_group, "kappa_aq = 0"),
#     aq_group = hypothesis(fit_list$fit_ri_aq_group, "kappa_group_e1:aq = 0"),
#     aq_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "kappa_aq = 0"),
#     aq_group_subtle = hypothesis(fit_list$fit_ri_aq_group_subtle, "kappa_group_e1:aq = 0")
#   )
# )

# Saving ------------------------------------------------------------------

circular <- list(
  fit_info = fit_info,
  tidy_fit = tidy_list_fit,
  priors = tidy_list_priors,
  tidy_post = list(post_fit_ri_int = post_fit_ri_int,
                   post_fit_ri_diff_group = post_fit_ri_diff_group,
                   post_fit_ri_diff_int = post_fit_ri_diff_int),
  loo = loo_list#,
  #bf = bf_list
)

saveRDS(circular, file = file.path("objects", "circular_objects.rds"))

#################################################
# 
# END
#
#################################################