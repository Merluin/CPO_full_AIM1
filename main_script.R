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

# Functions --------------------------------------------------------------

devtools::load_all()

# Running the entire analysis

create_dir_structure()

# Pre-processing

run_script("scripts/01_calc-angles.R")
run_script("scripts/02_prepare-data-fit.R")

# Models

run_script("scripts/03a_circular-models.R")
run_script("scripts/03b_intensity-models.R")


# Post-processing Models

run_script("scripts/04a_post-processing_circular.R")
run_script("scripts/04b_post-processing-intensity.R")

# Tables

run_script("scripts/05_tables.R")

# Figures

run_script("scripts/06_plots.R")

#################################################
# 
# END
#
#################################################