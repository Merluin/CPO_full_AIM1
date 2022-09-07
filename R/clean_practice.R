clean_practice <- function(x){
#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           23/03/2022
# Description:    clean extra columns (practice) from Qualiasoma experiment (psychopy3)
#
#################################################

  
  if(colnames(x[1]) == "practice_column" ){x <-x%>%
    select(multiple_practice_colums)
  }
  
  x <-x%>%
    mutate(participant = gsub( "\\D+","",participant),
      participant = as.numeric(as.character(participant)),
      id= paste0(experimenter,".",participant))
  
  return(x)

###########################################################################
#                                   END                                   #
###########################################################################
}