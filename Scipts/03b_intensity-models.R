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

library(tidyverse)
library(brms)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
chains <- 15
iter <- 4000
cores <- chains  
samp_prior <- "yes"

# Data --------------------------------------------------------------------
datasetname<-"full"

dat_fit <- readRDS(file = file.path("data",paste0(datasetname,"_fit.rds")))%>%
  mutate(group = as.factor(Exp.group))
dat <- readRDS(file = file.path("data",paste0(datasetname,"_valid.rds")))%>%
  mutate(group = as.factor(Exp.group))
dat_fit_full <- dat_fit %>% 
  filter(intensity == "full")
dat_fit_subtle <- dat_fit %>% 
  filter(intensity == "subtle")

# Legend ------------------------------------------------------------------

# ri = by-subject random intercept
# int = 3 way interaction (group * emotion * intensity)
# no3int = no 3 way interaction (group * emotion + intensity)
# tas_group = tas * group
# aq_group = aq * group
# tas_groupint = tas * group * int
# aq_groupint = aq * group * int
# neu = group (only neutral)

# Model 1 - Emotion * group * intensity ------------------------------------

prior_gaussian <- c(
  prior(normal(150, 100), class = "b", coef = "Intercept"),
  prior(normal(0, 50), class = "b")
)

form_ri_int <- bf(
  int ~  0 + Intercept + emotion * group * intensity + (1|id)
)

fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_int",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_int)

# Model 2 - Emotion * group + intensity ----------------------------------------

form_ri_no3int <- bf(int ~ 0 + Intercept + emotion + group + intensity + emotion:group + emotion:intensity + group:intensity + (1|id))

fit_ri_no3int <- brm(form_ri_no3int,
                     data = dat_fit,
                     prior = prior_gaussian,
                     family = gaussian(),
                     chains = chains,
                     cores = cores,
                     iter = iter,
                     file = "models/intensity/fit_ri_no3int",
                     save_pars = save_pars(all = TRUE),
                     sample_prior = samp_prior,
                     seed = seed)

success_step(fit_ri_no3int)

# # Model 3a - tas * group -----------------------------------------------------
# 
# prior_gaussian_tas_group <- c(
#   # int
#   prior(normal(150, 100), class = "b", coef = "Intercept"),
#   prior(normal(0, 50), class = "b", dpar = "", coef = "group_e1"),
#   prior(normal(0, 5), class = "b", dpar = "", coef = "tas"),
#   prior(normal(0, 5), class = "b", dpar = "", coef = "group_e1:tas")
# )
# 
# form_ri_tas_group <- bf(
#   int ~  0 + Intercept + group_e * tas + (1|id)
# )
# 
# fit_ri_tas_group <- brm(form_ri_tas_group,
#                        data = dat_fit,
#                        prior = prior_gaussian_tas_group,
#                        family = gaussian(),
#                        chains = chains,
#                        cores = cores,
#                        iter = iter,
#                        file = "models/intensity/fit_ri_tas_group",
#                        backend = "cmdstanr",
#                        save_pars = save_pars(all = TRUE),
#                        sample_prior = samp_prior,
#                        seed = seed)
# 
# success_step(fit_ri_tas_group)
# 
# # Model 3b - tas * group (subtle) -----------------------------------------
# 
# fit_ri_tas_group_subtle <- brm(form_ri_tas_group,
#                               data = dat_fit_subtle,
#                               prior = prior_gaussian_tas_group,
#                               family = gaussian(),
#                               chains = chains,
#                               cores = cores,
#                               iter = iter,
#                               file = "models/intensity/fit_ri_tas_group_subtle",
#                               backend = "cmdstanr",
#                               save_pars = save_pars(all = TRUE),
#                               sample_prior = samp_prior,
#                               seed = seed)
# 
# success_step(fit_ri_tas_group_subtle)
# 
# # Model 4a - aq * group ------------------------------------------------------
# 
# form_ri_aq_group <- bf(
#   int ~  0 + Intercept + group_e * aq + (1|id)
# )
# 
# prior_gaussian_aq_group <- c(
#   # int
#   prior(normal(150, 100), class = "b", coef = "Intercept"),
#   prior(normal(0, 50), class = "b", dpar = "", coef = "group_e1"),
#   prior(normal(0, 5), class = "b", dpar = "", coef = "aq"),
#   prior(normal(0, 5), class = "b", dpar = "", coef = "group_e1:aq")
# )
# 
# fit_ri_aq_group <- brm(form_ri_aq_group,
#                       data = dat_fit,
#                       prior = prior_gaussian_aq_group,
#                       family = gaussian(),
#                       chains = chains,
#                       cores = cores,
#                       iter = iter,
#                       file = "models/intensity/fit_ri_aq_group",
#                       backend = "cmdstanr",
#                       save_pars = save_pars(all = TRUE),
#                       sample_prior = samp_prior,
#                       seed = seed)
# 
# success_step(fit_ri_aq_group)
# 
# # Model 4b - aq * group (subtle) ------------------------------------------------------
# 
# fit_ri_aq_group_subtle <- brm(form_ri_aq_group,
#                              data = dat_fit_subtle,
#                              prior = prior_gaussian_aq_group,
#                              family = gaussian(),
#                              chains = chains,
#                              cores = cores,
#                              iter = iter,
#                              file = "models/intensity/fit_ri_aq_group_subtle",
#                              backend = "cmdstanr",
#                              save_pars = save_pars(all = TRUE),
#                              sample_prior = samp_prior,
#                              seed = seed)
# 
# success_step(fit_ri_aq_group_subtle)


# Model 5 - group (neutral faces) ------------------------------------------

form_ri_neu <- bf(
  int ~  0 + Intercept + group + (1|id)
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality") %>% 
  mutate(id = as.numeric(Pt.code),
    group_e = factor(group))

contrasts(dat_neutral$group_e) <- contr.sum(2)/2

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = 4,
                  cores = cores,
                  iter = 10000,
                  file = "models/intensity/fit_ri_neu",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_neu)
#################################################
# 
# END
#
#################################################