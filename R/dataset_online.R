dataset_online <- function(datasetname){
###########################################################################
#
#  Experiment:  CARIPARO ONLINE
#  Programmer:  QUETTIER THOMAS
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_online_AMIM1
#
#  Update:      13/08/2022
###########################################################################

# Packages ----------------------------------------------------------------
library(tidyverse)

# Parameters --------------------------------------------------------------
folder_dir<-  file.path("original_data")
filetask<-list.files(folder_dir,pattern= 'task')
fileq<-list.files(folder_dir,pattern= 'questionnaire')


# Info Dataset ------------------------------------------------------------
info<- read.csv(file.path(folder_dir,fileq), sep=",", header=TRUE,stringsAsFactors = FALSE)
info<-info%>%
  filter(Question.Key == "response-3"| Question.Key ==  "gender" | Question.Key == "response-1"| Question.Key == "respondent-email")%>%
  select( "Participant.Public.ID" ,"Participant.Private.ID","Question.Key","Response")%>%
  spread(Question.Key,Response)%>%
  'colnames<-'(c("Participant.Public.ID" ,"Participant.Private.ID","gender","mail", "study","ID"))%>%
mutate(ID = c("VICENZA" ,"FIRENZE",  "MATERA",  "VERONA",  "NAPOLI", "BARI","BOLOGNA", "ROMA","MILANO", "ANCONA", "TREVISO","TORINO", "PESCARA"),
       age = c(NA,50,37,59,62,61,41,NA,29,45,52,38,48),
       MADRS = c(18,8,14,9,5,25,0,0,11,8,4,9,13),
       SB = c(39,50,44,56,41,87,65,56,44,32,90,80,43))

# Task Dataset ------------------------------------------------------------

dataset <- read.csv(file.path(folder_dir,filetask), sep=",", header=TRUE,stringsAsFactors = FALSE)
dataset<-dataset%>%
  filter(Response == "click"| Zone.Type ==  "timelimit_screen")%>%
select( "Local.Date", "Participant.Public.ID" ,"Trial.Number",
  "Screen.Name", "Reaction.Time", "X.Coordinate", "Y.Coordinate", "display", 
  "Videos", "intensity", "file_gender", "duration","emotion", "identity")

# Final Dataset -----------------------------------------------------------
data<-left_join(info,dataset, by = "Participant.Public.ID")%>%
  select("Local.Date","Trial.Number","Participant.Public.ID","ID" ,"gender","study","age", "SB",
     "Screen.Name", "Reaction.Time", "X.Coordinate", "Y.Coordinate", "display", 
    "Videos", "intensity", "file_gender", "emotion", "identity")%>%
  'colnames<-'(c("Exp.date","Exp.trial","Pt.Public.ID","Pt.code" ,"Pt.gender","Pt.study", "Pt.age","Pt.sb",
                "Wheel.name", "Wheel.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
               "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))%>%
  mutate(Wheel.y = Wheel.y - 300,
         Wheel.y = Wheel.y * -1,  # flipping the y coordinates because gorilla use the upper left origin
         Wheel.x = Wheel.x - 300,
         Exp.trial = as.numeric(Exp.trial),
         Pt.Public.ID = as.factor(Pt.Public.ID),
         Pt.code = as.factor(Pt.code),
         Pt.gender = as.factor(Pt.gender),
         Pt.study = as.numeric(Pt.study),
         Pt.age = as.numeric(Pt.age),
         Wheel.name = as.factor(Wheel.name),
         Wheel.task = ifelse(Wheel.task == "practice","practice","task"),
         Wheel.task = as.factor(Wheel.task),
         Video.intensity = as.factor(Video.intensity),
         Video.gender = as.factor(Video.gender),
         emotion = as.factor(Video.emotion),
         Video.id = as.factor(Video.id),
         emotion = case_when(emotion == "angry"~"anger",
                             emotion == "disgusted"~"disgust",  
                             emotion == "fear"~"fear",
                             emotion == "happy"~"happiness",
                             emotion == "neutral"~"neutrality",
                             emotion == "sad"~"sadness",
                             emotion == "surprised"~"surprise"),
         Exp.group = "online")



# Save --------------------------------------------------------------------
save(data,file = file.path("data", paste0(datasetname,".rds")))

###########################################################################
#                                   END                                   #
###########################################################################
}