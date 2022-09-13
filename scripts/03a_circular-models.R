###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/)  and psychopy3
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
cores <- 6 
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
# tas_gp = tas * group
# aq_gp = aq * group
# tas_gpint = tas * group * int
# aq_gpint = aq * group * int
# gp = group (only neutral)

#Circular Models ----------------------------------------------------------
  
  prior_von_mises <- c(
    prior(normal(0, 2), class = "b", dpar = ""), # betas prior
    prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
  )

# Model 1 - Emotion * group * intensity ------------------------------------

form_ri_int <- bf(diff_theta ~ emotion * group * intensity + (1|id), 
                  kappa ~  emotion * group * intensity + (1|id))


fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_int",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_int)

# Model 1a - Emotion * group * intensity ------------------------------------

form_ri_3int <- bf(diff_theta ~ emotion * group * intensity + (1|id))

prior_von_mises <- c( prior(normal(0, 2), class = "b", dpar = ""))


fit_ri_3int <- brm(form_ri_3int,
                   data = dat_fit,
                   prior = prior_von_mises,
                   family = von_mises(link = "tan_half"),
                   chains = chains,
                   cores = cores,
                   iter = iter,
                   sample_prior = samp_prior,
                   file = "models/theta/fit_ri_4int",
                   save_pars = save_pars(all = TRUE),
                   seed = seed)

success_step(fit_ri_3int)


summary(fit_ri_3int)
m1<-emmeans(fit_ri_3int, ~group|intensity|emotion)
s1<-summary(m1)
s1%>%
  as.data.frame()%>%
  flextable()%>% 
  colformat_double(digits = 2) %>% 
  theme_vanilla()

# Model 1b - Emotion * group * intensity * sunnybrook------------------------------------

form_ri_4int <- bf(diff_theta ~ emotion * group * intensity * Pt.sb + (1|id))

prior_von_mises <- c(
  prior(normal(0, 2), class = "b", dpar = ""))

fit_ri_4int <- brm(form_ri_4int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_4int",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_4int)


summary(fit_ri_4int)
m1<-emtrends(fit_ri_4int, ~group|intensity|emotion ,var = "Pt.sb")
s1<-summary(m1)
s1%>%
  as.data.frame()%>%
  flextable()%>% 
  colformat_double(digits = 2) %>% 
  theme_vanilla()

# Model 2 - Emotion + group + intensity ------------------------------------

form_ri_no3int <- bf(diff_theta ~ emotion + group + intensity + emotion:group + emotion:intensity + group:intensity + (1|id), 
                     kappa ~ emotion + group + intensity + emotion:group + emotion:intensity + group:intensity + (1|id))

fit_ri_no3int <- brm(form_ri_no3int,
                     data = dat_fit,
                     prior = prior_von_mises,
                     family = von_mises(link = "tan_half", link_kappa = "log"),
                     chains = chains,
                     cores = cores,
                     iter = iter,
                     sample_prior = samp_prior,
                     file = "models/theta/fit_ri_no3int",
                     save_pars = save_pars(all = TRUE),
                     seed = seed)

success_step(fit_ri_no3int)

# # Model 3a - tas * group -----------------------------------------------------
# 
# prior_von_mises_tas_group <- c(
#   # theta
#   prior(normal(0, 2), class = "b", dpar = "", coef = "Intercept"),
#   prior(normal(0, 2), class = "b", dpar = "", coef = "group_e1"),
#   prior(normal(0, 0.5), class = "b", dpar = "", coef = "tas"),
#   prior(normal(0, 0.5), class = "b", dpar = "", coef = "group_e1:tas"),
#   
#   # kappa
#   prior(normal(0, 2), class = "b", dpar = "kappa", coef = "Intercept"),
#   prior(normal(0, 2), class = "b", dpar = "kappa", coef = "group_e1"),
#   prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "tas"),
#   prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "group_e1:tas")
# )
# 
# form_ri_tas_group <- bf(diff_theta ~ 0 + Intercept + group_e * tas + (1|id), 
#                        kappa ~ 0 + Intercept + group_e * tas + (1|id))
# 
# fit_ri_tas_group <- brm(form_ri_tas_group,
#                        data = dat_fit,
#                        prior = prior_von_mises_tas_group,
#                        family = von_mises(link = "tan_half", link_kappa = "log"),
#                        chains = chains,
#                        cores = cores,
#                        iter = iter,
#                        #backend = "cmdstanr",
#                        sample_prior = samp_prior,
#                        file = "models/theta/fit_ri_tas_group",
#                        save_pars = save_pars(all = TRUE),
#                        seed = seed)
# 
# success_step(fit_ri_tas_group)
# 
# # Model 3b - tas * group (subtle) -----------------------------------------
# 
# # here we use a different approach because the fitting process is more
# # difficult
# 
# prior_von_mises_tas_group_subtle <- c(
#   # theta
#   prior(normal(0, 5), class = "b", dpar = "", coef = "Intercept"),
#   prior(normal(0, 5), class = "b", dpar = "", coef = "group_e1"),
#   prior(normal(0, 1), class = "b", dpar = "", coef = "tas"),
#   prior(normal(0, 1), class = "b", dpar = "", coef = "group_e1:tas"),
#   
#   # kappa
#   prior(normal(0, 5), class = "b", dpar = "kappa", coef = "Intercept"),
#   prior(normal(0, 5), class = "b", dpar = "kappa", coef = "group_e1"),
#   prior(normal(0, 1), class = "b", dpar = "kappa", coef = "tas"),
#   prior(normal(0, 1), class = "b", dpar = "kappa", coef = "group_e1:tas")
# )
# 
# chains_subtle <- 4
# iter_subtle <- 10000
# cores_subtle <- chains_subtle
# 
# fit_ri_tas_group_subtle <- brm(fit_ri_tas_group$formula,
#                               data = dat_fit,
#                               family = von_mises(link = "tan_half", link_kappa = "log"),
#                               chains = chains_subtle,
#                               prior = prior_von_mises_tas_group_un,
#                               cores = cores_subtle,
#                               iter = iter_subtle,
#                               backend = "cmdstanr",
#                               threads = threading(6),
#                               sample_prior = samp_prior,
#                               file = "models/theta/fit_ri_tas_group_subtle",
#                               save_pars = save_pars(all = TRUE),
#                               seed = seed)
# 
# success_step(fit_ri_tas_group_subtle)
# 
# # Model 4a - aq * group ------------------------------------------------------
# 
# prior_von_mises_aq_group <- c(
#   # theta
#   prior(normal(0, 2), class = "b", dpar = "", coef = "Intercept"),
#   prior(normal(0, 2), class = "b", dpar = "", coef = "group_e1"),
#   prior(normal(0, 0.5), class = "b", dpar = "", coef = "aq"),
#   prior(normal(0, 0.5), class = "b", dpar = "", coef = "group_e1:aq"),
#   
#   # kappa
#   prior(normal(0, 2), class = "b", dpar = "kappa", coef = "Intercept"),
#   prior(normal(0, 2), class = "b", dpar = "kappa", coef = "group_e1"),
#   prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "aq"),
#   prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "group_e1:aq")
# )
# 
# form_ri_aq_group <- bf(diff_theta ~ 0 + Intercept + group_e * aq + (1|id), 
#                       kappa ~ 0 + Intercept + group_e * aq + (1|id))
# 
# fit_ri_aq_group  <- brm(form_ri_aq_group,
#                        data = dat_fit,
#                        prior = prior_von_mises_aq_group,
#                        family = von_mises(link = "tan_half", link_kappa = "log"),
#                        chains = chains,
#                        cores = cores,
#                        iter = iter,
#                        backend = "cmdstanr",
#                        sample_prior = samp_prior,
#                        file = "models/theta/fit_ri_aq_group",
#                        save_pars = save_pars(all = TRUE),
#                        seed = seed)
# 
# success_step(fit_ri_aq_group)
# 
# # Model 4b - aq * group (subtle) ------------------------------------------------------
# 
# fit_ri_aq_group_subtle  <- brm(form_ri_aq_group,
#                               data = dat_fit_subtle,
#                               prior = prior_von_mises_aq_group,
#                               family = von_mises(link = "tan_half", link_kappa = "log"),
#                               chains = chains,
#                               cores = cores,
#                               iter = iter,
#                               backend = "cmdstanr",
#                               sample_prior = samp_prior,
#                               file = "models/theta/fit_ri_aq_group_subtle",
#                               save_pars = save_pars(all = TRUE),
#                               seed = seed)
# 
# success_step(fit_ri_aq_group_subtle)


# Model 5 - group (neutral faces) ------------------------------------------

prior_von_mises_neu <- c(
  prior(uniform(-3.141593, 3.141593), class = "b", dpar = "", lb = -3.141593, ub = 3.141593), # betas prior
  prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality") %>% 
  mutate(id = as.numeric(Pt.code),
         group_e = factor(group),
         theta_cen = theta - pi) # centering pi

contrasts(dat_neutral$group_e) <- contr.sum(2)/2

form_ri_neu <- bf(theta_cen ~ 0 + Intercept + group_e + (1|id), 
                  kappa ~  0 + Intercept + group_e + (1|id))

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_von_mises_neu,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_neu",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_neu)

#################################################
# 
# END
#
#################################################