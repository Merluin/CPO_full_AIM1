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

# Functions ---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

datasetname<-"full"

dat <- readRDS(file = file.path("data",paste0(datasetname,"_valid.rds")))

dat <- dat%>% 
  mutate(id = as.numeric(Pt.code))

dat_fit <- dat %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,emotion != "neutrality") %>% 
  mutate(diff_theta = unname(deg_to_rad(diff)),
         emotion = factor(emotion),
         intensity = Video.intensity)

# Setting sum to 0 contrast 

contrasts(dat_fit$emotion) <- contr.sum(length(unique(dat_fit$emotion)))
contrasts(dat_fit$intensity) <- contr.sum(length(unique(dat_fit$intensity)))

# Saving ------------------------------------------------------------------

saveRDS(dat_fit, file = file.path("data",paste0(datasetname,"_fit.rds")))

#################################################
# 
# END
#
#################################################