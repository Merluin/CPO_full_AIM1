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
datasetname <-"full"

dataset_moebius("moebius")
load(file.path("data", "moebius.rds"))
moebius<-data
dataset_online("online")
load(file.path("data", "online.rds"))
data<-rbind(data,moebius)


# Coordinates -------------------------------------------------------------

dat <- data %>% 
  mutate(x_cen = Wheel.x,
         y_cen = Wheel.y,
         int = calc_dist(point_x = x_cen, point_y = y_cen),
         x_cen = ifelse(x_cen == 0, x_cen + 0.0001, x_cen),
         y_cen = ifelse(y_cen == 0, y_cen + 0.0001, y_cen),
         theta = atan(y_cen/x_cen),
         theta = correct_angle(theta, x_cen, y_cen), # correcting for quadrant
         angle = rad_to_deg(theta)) # convert to degrees

# Here we create a dataframe with the "correct" value for each emotion. In this way
# we can calculate the difference between the pressed angle and the correct one

coords <- tibble(
  emotion = c("satisfaction", "happiness", "elation", "pride", "anger", "contempt",
          "disgust", "envy", "guilt", "shame", "fear", "sadness", "surprise", 
          "interest", "hope", "relief")
)

coords <- coords %>% 
  mutate(theta_emo = (2 * 0:(nrow(.) - 1) * pi)/nrow(.), # see https://math.stackexchange.com/a/206662
         theta_emo = theta_emo + deg_to_rad((360/nrow(.))/2), # adding the shift for centering emotions
         x_emo = 300 * cos(theta_emo),
         y_emo = 300 * sin(theta_emo),
         angle_emo = rad_to_deg(theta_emo))

# Here we calculate the angular difference between the correct and the pressed angle

names(coords$angle_emo) <- coords$emotion # renaming for expanding

dat <- dat %>% 
  mutate(emotion_angle = coords$angle_emo[.$emotion], # expanding the coords
         diff = ang_diff(emotion_angle, angle))

# combining the coords with the dat

dat <- left_join(dat, coords, by = "emotion")

# assigning order to coords
coords$order <- c(4, 3, 2, 1, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5)
coords$emo_order <- coords$emotion[coords$order]


# Adding label to emotion/intensity ---------------------------------------

dat<- dat%>% 
  mutate(resp_emotion_label = seg_position(round(angle, 1)),
         resp_intensity_label = level_int_position(int),
         resp_intensity_ord = parse_number(resp_intensity_label))

# Saving ------------------------------------------------------------------
saveRDS(dat,file.path("data", paste0(datasetname,"_valid.rds")))
saveRDS(coords, file = file.path("objects", "emo_coords.rds"))

#################################################
# 
# END
#
#################################################