#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################
dataset_moebius <- function(dataset_name)
{
  
  # packages
  library(tidyverse)
  devtools::load_all()
  
  # find nb of file
  folder_dir<-file.path("original_data")
  
  #concatenate all file
  dataset<-list.files(path=folder_dir, full.names = TRUE,pattern='csv') %>%
    lapply(.,function(x) read.csv(x, sep=",", header=TRUE, stringsAsFactors = FALSE ))%>%
    lapply(clean_practice)%>%
    bind_rows()
  
  dataset<- dataset%>%
    mutate(subject = as.numeric(as.factor(id)))%>%
    group_by(id) %>% 
    mutate(trial = 1:n()) %>% 
    ungroup()
  
  info<-data.frame(subject = c(1:3), SB = c(12,17,9))
  
  dataset<-left_join(dataset,info, by = "subject")
  
  
Pct<-dataset%>%
  filter(loop_practice.thisRepN>=0)%>%
    select("date","trial","id","subject","sex","education", "age","SB","group",
           "practice","primary.time","primary.x","primary.y",
           "file_duration", "file", "file_emotion_level", "file_gender",
           "file_emotion", "file_id")%>%
    'colnames<-'(c("Exp.date","Exp.trial","Pt.Public.ID","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Exp.group",
                   "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                   "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
    mutate(Wheel.name = "GW1" ,
           Wheel.task = "practice")

  
Gw1<-dataset%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  select("date","trial","id","subject","sex","education", "age","SB","group",
         "practice","primary.time","primary.x","primary.y",
         "file_duration", "file", "file_emotion_level", "file_gender",
         "file_emotion", "file_id")%>%
    'colnames<-'(c("Exp.date","Exp.trial","Pt.Public.ID","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Exp.group",
                   "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                   "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
    mutate(Wheel.name = "GW1",
           Wheel.task = "task")%>%
    drop_na(Video.id)

Gw2<-dataset%>%
  filter(exp_blocks.thisRepN >= 0)%>%
  select("date","trial","id","subject","sex","education", "age","SB","group",
         "practice","secondary.time","secondary.x","secondary.y",
         "file_duration", "file", "file_emotion_level", "file_gender",
         "file_emotion", "file_id")%>%
  'colnames<-'(c("Exp.date","Exp.trial","Pt.Public.ID","Pt.code" ,"Pt.gender","Pt.study","Pt.age","Pt.sb","Exp.group",
                 "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                 "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
  mutate(Wheel.name = "GW2",
         Wheel.task = "task")%>%
  drop_na(Video.id)


data <- rbind(Pct,Gw1, Gw2)%>%
    mutate(
           Exp.trial = as.numeric(Exp.trial),
           Pt.Public.ID = as.factor(Pt.Public.ID),
           Pt.code = as.factor(Pt.code),
           Pt.gender = ifelse(Pt.gender == "f","Donna","Uomo"),
           Pt.gender = as.factor(Pt.gender),
           Pt.study = as.numeric(Pt.study),
           Pt.age = as.numeric(Pt.age),
           Wheel.name = as.factor(Wheel.name),
           Wheel.task = as.factor(Wheel.task),
           Wheel.x = str_remove_all(Wheel.x, "[\\[|\\] ']"),
           Wheel.y = str_remove_all(Wheel.y, "[\\[|\\] ']"),
           Wheel.rt = str_remove_all(Wheel.rt, "[\\[|\\] ']"),
           Wheel.x = as.numeric(Wheel.x),
           Wheel.y = as.numeric(Wheel.y),
           # Wheel.x = case_when( Wheel.name == "GW1" ~ (Wheel.x + 256),
           #                    Wheel.name == "practice" ~ (Wheel.x + 256),
           #                    Wheel.name == "GW2" ~ (Wheel.x - 256)),
           Wheel.rt = as.numeric(Wheel.rt),
           Wheel.rt = round(Wheel.rt*1000,1),
           Wheel.rt = replace_na(Wheel.rt,19999),
           Video.intensity = as.factor(Video.intensity),
           Video.gender = as.factor(Video.gender),
           emotion = as.factor(Video.emotion),
           Video.id = as.factor(Video.id))%>%
  group_by(Pt.code) %>% 
  mutate(Exp.trial = 1:n()) %>% 
  ungroup()%>%
  arrange(Pt.code)%>%
  mutate(emotion = case_when(emotion == "angry"~"anger",
                      emotion == "disgusted"~"disgust",  
                      emotion == "fear"~"fear",
                      emotion == "happy"~"happiness",
                      emotion == "neutral"~"neutrality",
                      emotion == "sad"~"sadness",
                      emotion == "surprised"~"surprise"),
         Exp.group = "moebius")

  

  
  
  write.csv2(data, file= file.path("data",paste0(dataset_name,".csv")))
  save(data,file= file.path("data",paste0(dataset_name,".rds")))
  
  
} #end function  

#################################################
# 
# END
#
#################################################