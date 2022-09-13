###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) and psychopy3
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
library(ggplot2)
library(cowplot)
library(purrr)
library(magick)

# Functions ---------------------------------------------------------------

devtools::load_all()


ggsave_plot <- function(plot, name, device = c("png", "tiff", "eps", "pdf", "svg"),
                        width = width, height = height){
  device <- match.arg(device)
  name <- paste0(name, ".", device)
  ggsave(plot, filename = name, device = device, width = width, height = height)
}

theme_paper <- function(font_size = 12){
  cowplot::theme_minimal_grid(font_size = font_size)
}

put_note <- function(x, y, text, size = 3, ...){
    annotate("label", x, y, label = text, 
             fontface = 2, 
             fill = "white", 
             label.size = NA,
             size = size,
             ...)
}

w_stat_halfeye <- function(alpha = 0.8){
  stat_halfeye(alpha = alpha, 
               size = 2.5, 
               .width = 0.95)
}
  

# Importing Data ----------------------------------------------------------
datasetname<-"full"

intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))
emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
dat <- readRDS(file.path("data",paste0(datasetname,"_valid.rds")))

# EDA Plots ---------------------------------------------------------------

bg <- magick::image_read("files/gew_low_res.png")
bg <- magick::image_modulate(bg, brightness = 80)

gew_legend <- emo_coords %>%   
  mutate(mask = "Legend",
         flip = ifelse(x_emo < 0, angle_emo + 180, angle_emo),
         emotion = stringr::str_to_title(emotion)) %>% 
  ggplot() +
  ggpubr::background_image(bg) +
  geom_text(aes(x = x_emo*0.75, y = y_emo*0.75, 
                label = emotion, 
                angle = flip),
            size = 6, fontface = "bold",
            check_overlap = TRUE) +
  facet_grid(. ~ mask) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))

dat_plot <- dat %>% 
  select(Exp.group,Wheel.task,emotion,Wheel.name, Video.intensity, x_cen, y_cen) %>% 
  mutate( intensity = stringr::str_to_title(Video.intensity),
          emotion = stringr::str_to_title(emotion),
          emotion = ifelse(emotion == "Neutrality", "Neutral", emotion),
          emotion = factor(emotion),
          emotion = forcats::fct_relevel(emotion, "Neutral"))

neutral_plot <- dat_plot %>% 
  filter(emotion == "Neutral") %>% 
  ggplot(aes(x = x_cen, y = y_cen, color= Exp.group)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, show.legend = FALSE, size = 3) +
  ggh4x::facet_nested(Wheel.name ~ emotion, switch="y") +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA))+
  facet_grid( Wheel.name~ Video.intensity)


plot_gew_legend_neutral <- plot_grid(neutral_plot, gew_legend, labels = "AUTO")

plot_gew_emotions <- dat_plot %>% 
  filter(emotion != "Neutral", Wheel.task == "task") %>% 
  ggplot(aes(x = x_cen, y = y_cen, color= Exp.group)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 2) +
  ggh4x::facet_nested(Wheel.name + intensity ~ emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("NA", "white"))

# -------------------------------------------------------------------------------------------
# 
# plot_gew_fulldots <- dat_plot %>% 
#   drop_na(emotion)%>%
#   filter( Wheel.task == "task") %>% 
#   ggplot(aes(x = x_cen, y = y_cen, color= Exp.group)) +
#   ggpubr::background_image(bg) +
#   geom_point(alpha = 0.5, size = 2) +
#   ggh4x::facet_nested(Wheel.name + intensity ~ emotion) +
#   coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         strip.text.x = element_text(size = 14, face = "bold"),
#         strip.text.y = element_text(size = 14, face = "bold"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white", color = NA)) +
#   scale_fill_manual(values = c("NA", "white"))

# Plot Angle subtle vs full ----------------------------------------------

plot_angle_intensity_a <- circular_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(group,emotion, intensity, .draw) %>% 
  summarise(angle = mean(angle)) %>% 
  ggplot(aes(x = angle, y = emotion, fill = intensity)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(alpha = 0.8, size = 3) +
  theme_paper() +
  facet_grid( .~ group)+
  theme(axis.title.y = element_blank(),
        legend.position = c(0.9, 0.15)) +
  xlab("Bias") +
  labs(fill = "Intensity")


plot_angle_intensity_b <- circular_objects$tidy_post$post_fit_ri_diff_int %>% 
  group_by(group,emotion, .draw) %>% 
  summarise(angle_diff = mean(angle_diff)) %>% 
  ggplot(aes(x = angle_diff, y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 3) +
  theme_paper() +
  facet_grid( .~ group)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{intensity}$ Bias$"))

plot_angle_intensity <- plot_grid(plot_angle_intensity_a, plot_angle_intensity_b, 
                             labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Plot Kappa subtle vs full ----------------------------------------------

plot_kappa_intensity_a <- circular_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(group, emotion, intensity, .draw) %>% 
  summarise(kappa_inv = mean(kappa_inv)) %>% 
  ggplot(aes(x = log(kappa_inv), y = emotion)) +
  stat_halfeye(aes(fill = intensity),
               alpha = 0.8, size = 3) +
  theme_minimal(base_size = 15) +
  theme_paper() +
  facet_grid( .~ group)+
  theme(axis.title.y = element_blank(),
        legend.position = c(0.90, 0.2)) +
  xlab("Uncertainty")

plot_kappa_intensity_b <- circular_objects$tidy_post$post_fit_ri_diff_int %>% 
  group_by(group, emotion, .draw) %>% 
  summarise(kappa_inv_ratio = mean(kappa_inv_ratio)) %>% 
  ggplot(aes(x = log(kappa_inv_ratio), y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 3,
               position = position_dodge(width = 0.9)) +
  theme_paper() +
  facet_grid( .~ group)+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("log $Ratio_{intensity}$")) +
  scale_x_continuous(n.breaks = 4)

plot_kappa_intensity <- plot_grid(plot_kappa_intensity_a, plot_kappa_intensity_b,
                             labels = "AUTO", rel_widths = c(3, 2), align = "hv")




# Plot Intensity subtle vs full ------------------------------------------

plot_int_intensity_a <- intensity_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(group, emotion, intensity, .draw) %>% 
  summarise(int = mean(int)) %>% 
  ggplot(aes(x = int, y = emotion, fill = intensity)) +
  w_stat_halfeye() +
  theme_paper() +
  facet_grid( .~ group)+
  xlab("Perceived Intensity") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.85, 0.10))

plot_int_intensity_b <- intensity_objects$tidy_post$post_fit_ri_diff_group %>% 
  select(-int_diff)%>%
  gather(group,score,4:5)%>%
  spread(intensity, score)%>%
  mutate(int_diff = full-subtle)%>%
  group_by(emotion,group, .draw) %>% 
  summarise(int_diff = mean(int_diff)) %>% 
  ggplot(aes(x = int_diff, y = emotion)) +
  w_stat_halfeye() +
  theme_paper() +
  facet_grid( .~ group)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{intensity}\\; Perceived \\;Intensity$"))

plot_int_intensity <- plot_grid(plot_int_intensity_a, plot_int_intensity_b, 
                           labels = "AUTO", rel_widths = c(3, 2), align = "hv")




# Categorical Responses ---------------------------------------------------

# order as the wheel
dat$resp_emotion_label <- factor(dat$resp_emotion_label, levels = emo_coords$emo_order)

dat_summ <- dat %>% 
  drop_na(emotion)%>%
  mutate(intensity = Video.intensity)%>%
  filter(emotion != "neutrality") %>% 
  group_by(emotion,intensity, resp_emotion_label) %>% 
  summarise(n = n())

plot_gew_discrete <- dat_summ %>% 
  drop_na(resp_emotion_label)%>%
  mutate(intensity = stringr::str_to_title(intensity)) %>% 
  clean_emotion_names(emotion) %>% 
  ggplot(aes(x = resp_emotion_label, y = n, fill = intensity)) +
  geom_col(position = position_dodge()) +
  facet_grid(emotion~intensity) +
  cowplot::theme_minimal_hgrid() +
  theme_paper(font_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
                                   face = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 "bold", "plain"),
                                   size = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 10, 8)),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank()) +
  labs(fill = "Intensity")

# Saving ------------------------------------------------------------------

plot_list <- make_named_list(plot_gew_legend_neutral, 
                             plot_gew_emotions,
                             plot_gew_discrete,
                             plot_angle_intensity, 
                             plot_kappa_intensity, 
                             plot_int_intensity )

saveRDS(plot_list, file = "objects/paper_plots.rds")

# Good size for posterior plots

mkdir_if("figures/png")
mkdir_if("figures/pdf")
mkdir_if("figures/tiff")

width <- 16
height = 10

for(i in 1:length(plot_list)){
  plot_name <- names(plot_list)[i]
  if(grepl("mask|int", names(plot_list[i]))){
    ggsave_plot(plot_list[[i]], 
                name = file.path("figures", "pdf", plot_name), 
                device = "pdf",
                width = width, 
                height = height)
    ggsave_plot(plot_list[[i]],
                device = "png",
                name = file.path("figures", "png", plot_name), 
                width = width, 
                height = height)
  }
}

# GEW Plots

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "png", "plot_gew_legend_neutral"),
            device = "png", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "pdf", "plot_gew_legend_neutral"),
            device = "pdf", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "png", "plot_gew_discrete"),
            device = "png", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "pdf", "plot_gew_discrete"),
            device = "pdf", width = 15, height = 10)

#################################################
# 
# END
#
#################################################