#' calc_dist
#' @description Calculate the euclidean distance between two points
#' @param center_x numeric the \code{x} coordinate of the center. Default to 0
#' @param center_y numeric the \code{y} coordinate of the center. Default to 0
#' @param point_x numeric the \code{x} coordinate of the point
#' @param point_y numeric the \code{y} coordinate of the point
#'
#' @return numeric the euclidean distance
#' @export
#'
calc_dist <- function(center_x = 0, center_y = 0,
                      point_x, point_y){
  sqrt((center_x - point_x)^2 + (center_y - point_y)^2)
}

#' correct_angle
#' @description adjust the angle according to quadrants for a 0-centered circle
#' @param angle numeric the angle in degrees
#' @param x numeric the \code{x} coordinate of the point
#' @param y numeric the \code{y} coordinate of the point
#'
#' @return numeric vector of angles
#' @import dplyr
#' @export
#'
correct_angle <- function(angle, x, y){
  case_when(
    x > 0 & y > 0 ~ angle, # 1 quadrant
    x < 0 & y > 0 ~ angle + pi, # 2 quadrant
    x < 0 & y < 0 ~ angle + pi, # 3 quadrant
    x > 0 & y < 0 ~ angle + pi*2 # 4 quadrant
  )
}

#' rad_to_deg
#' @description convert from radians to degree
#' @param rad numeric the angle in radians
#'
#' @return numeric the angle in degree
#' @export
#'
rad_to_deg <- function(rad){
  rad * 180/pi
}

#' deg_to_rad
#' @description convert from radians to degree
#' @param deg numeric the angle in degree
#'
#' @return numeric the angle in radians
#' @export
deg_to_rad <- function(deg){
  deg * pi/180
}

#' ang_diff
#' @description compute the closest angular difference between two angles. Thanks
#' to thanks to https://stackoverflow.com/a/7869457
#' @param target numeric the first angle in degrees
#' @param pressed numeric the second angle in degrees
#'
#' @return numeric the difference in degrees
#' @export
#'
ang_diff <- function(target, pressed){
  
  ((target - pressed) + 180) %% 360 - 180
}


#' rescale
#' @description rescale a variable between 0 and 1
#' @param x a numeric or integer vector
#'
#' @return the rescaled numeric vector
#' @export
#'
rescale <- function(x){
  (x - min(x)) / (max(x) - min(x))
}


#' level_int_position
#' @description assing the intensity label according to the distance from the center of the GEW
#' @param level_int a numeric vector with distances from the GEW center
#'
#' @return a character vector with the corresponding intensity
#' @export
#'
level_int_position <- function(level_int){
  dplyr::case_when(
    between(level_int, 209.0, 300.0) ~ "4_hight",
    between(level_int, 149.0, 210.0) ~ "3_medium_hight",
    between(level_int, 99.0, 150.0) ~ "2_medium_low",
    between(level_int, 59.0, 100.0) ~ "1_low",
    between(level_int, 0, 60.0) ~ "0_neutrality") # better using also a number here
}


#' seg_position
#' @description assigning the emotion label according to the response angle
#' @param seg_ang a numeric vector with the response angles in degrees
#'
#' @return a character vector with the corresponding emotion label
#' @export
#' 
seg_position <- function(seg_ang){
  dplyr::case_when(
    between(seg_ang, 0, 22.5) ~ "satisfaction",
    between(seg_ang, 22.6, 45.0) ~ "happiness",
    between(seg_ang, 45.1, 67.5) ~ "elation",
    between(seg_ang, 67.6, 90.0) ~ "pride",
    between(seg_ang, 90.1, 112.5) ~ "anger",
    between(seg_ang, 112.6, 135.0) ~ "contempt",
    between(seg_ang, 135.1, 157.5) ~ "disgust",
    between(seg_ang, 157.6, 180.0) ~ "envy",
    between(seg_ang, 180.1, 202.5) ~ "guilt",
    between(seg_ang, 202.6, 225.0) ~ "shame",
    between(seg_ang, 225.1, 247.5) ~ "fear",
    between(seg_ang, 247.6, 270.0) ~ "sadness",
    between(seg_ang, 270.1, 292.5) ~ "surprise",
    between(seg_ang, 292.6, 315.0) ~ "interest",
    between(seg_ang, 315.1, 337.5) ~ "hope",
    between(seg_ang, 337.5, 360.0) ~ "relief")
}  


#' clean_env
#' @description clean the global environment
#' @export
#'
clean_env <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}


#' run_script
#' @description source a script and return a nice message
#' @param file a character indicating the file path and name
#'
#' @export
#'
run_script <- function(file){
  
  file_name <- basename(file)
  file_name <- deparse(substitute(file_name))
  
  clean_env() # clean everything
  
  source(file)
  
  success(paste(file_name, "finished!"))
  
}

#' mkdir_if
#' @description create a directory if it doesn't exist
#' @param dir character indicating the path and name of the folder
#'
#' @export
#'
mkdir_if <- function(dir){
  if(!dir.exists(dir)){
    dir.create(dir)
  }
}

#' create_dir_structure
#' @description create the directories structure for the project
#' @export
#'
create_dir_structure <- function(){
  mkdir_if("models")
  mkdir_if("models/theta")
  mkdir_if("models/intensity")
  mkdir_if("objects")
  mkdir_if("figures")
  mkdir_if("tables")
  cli::cli_alert_success("Project Structure Created!")
}

#' center_var
#' @description center a numeric variable on the mean
#' @param x a numeric vector
#'
#' @return the centered numeric vector
#' @export
#'
center_var <- function(x){
  x - mean(x)
}


#' success
#'
#' @param msg a character indicating the message to be printed
#'
#' @export
#'
success <- function(msg){
  cli::cli_alert_success(msg)
}


#' Title
#' @description converting from \code{k} to the \italic{circula variance}. See https://mathworld.wolfram.com/vonMisesDistribution.html
#' @param kappa the kappa value
#'
#' @return the circular variance
#' @export
#'
kappa_to_var <- function(kappa){
  1-(besselI(kappa, 1)/besselI(kappa, 0))
}

#' clean_emotion_names
#' @description clean the emotion names for tables and figures (use lazy evaluation)
#' @param data the target dataframe
#' @param col name for the target column for the emotions
#'
#' @return the dataframe with the modified column
#' @export
#'
clean_emotion_names <- function(data, col){
  col <- rlang::enexpr(col)
  data %>% 
    mutate(!!col := ifelse(!!col == "neutrality", "neutral", !!col),
           !!col := stringr::str_to_title(!!col))
}


#' tidy_brm
#'
#' @param fit an object fitted with \code{brms}
#'
#' @return a tibble with all model information
#' @export
#'
tidy_brm <- function(fit){
  # tidy params
  tidy_params <- fit %>% 
    spread_draws(`b_.*|sd_.*`, regex = TRUE) %>% 
    pivot_longer(starts_with(c("b_", "sd_")),
                 names_to = "param",
                 values_to = "value") %>% 
    group_by(param) %>% 
    summarise(se = sd(value),
              median_hdi(value)) %>% 
    rename("median" = y,
           "lower" = ymin,
           "upper" = ymax) %>% 
    select(param, median, se, lower, upper) %>% 
    mutate(param = case_when(param == "sd_id__Intercept" ~ "sd(Intercept)",
                             param == "sd_id__kappa_Intercept" ~ "sd(kappa_Intercept)",
                             TRUE ~ param),
           param = stringr::str_remove_all(param, "b_"))
  
  # get fitting information
  fit_summary <- summary(fit)
  fit_summary <- rbind(fit_summary$fixed, fit_summary$random[[1]])
  fit_summary$param <- rownames(fit_summary)
  rownames(fit_summary) <- NULL
  fit_summary <- select(fit_summary, param, Rhat, Bulk_ESS, Tail_ESS)
  
  # combining
  tidy_params %>% 
    left_join(., fit_summary, by = "param")
}

#' tidy_priors
#'
#' @param fit an object fitted with \code{brms}
#'
#' @return a tibble with all priors information
#' @export
#'
tidy_priors <- function(fit){
  out <- data.frame(prior_summary(fit, all = FALSE)) # only proper priors
  rownames(out) <- NULL
  return(out)
}

#' load_models
#' @description load all \code{.rds} models within a folder
#' @param path 
#'
#' @return
#' @export
#'
load_models <- function(path){
  fits <- list.files(path, full.names = TRUE, pattern = "*.rds")
  fit_list <- lapply(fits, readRDS)
  names(fit_list) <- tools::file_path_sans_ext(basename(fits))
  return(fit_list)
}

#' make_named_list
#'
#' @param ... a series of objects separated with comma
#'
#' @return a named list with objects and objects' names
#'
make_named_list <- function(...){
  list_names <- rlang::enexprs(...)
  list_values <- list(...)
  names(list_values) <- unlist(list_names)
  return(list_values)
}


#' success_step
#' @description return a nice message using the object name. Useful for tracking complex steps.
#' @param x a object
#' @param msg the message to be displayed
#'
#' @export
#'
success_step <- function(x, msg = "completed!"){
  obj_name <- deparse(substitute(x))
  success(paste(obj_name, msg))
}


#' get_model_info
#' @description return model fitting information in a dataframe
#' @param fit an object fitted with \code{brms}
#' @param info a character vector with information to be included
#'
#' @return a dataframe
#' @export
#'
get_model_info <- function(fit, info = c("formula", "version", "backend")){
  fit_info <- fit[info]
  fit_info$version <- lapply(fit_info$version, as.character)
  summ <- summary(fit)
  fitting <- list(chains = summ$chains,
                  iter = summ$iter,
                  warmup = summ$warmup,
                  samples = (summ$iter - summ$warmup)*summ$chains)
  fit_info$fitting <- fitting
  return(fit_info)
}

#' get_chr_formula
#'
#' @param fit_info the dataframe with fitting information using \code{get_model_info()}
#' @param collapse logical that indicate if multiple formulas need to be collapsed into a single character vector.
#'
#' @return a character vector
#' @export
#'
get_chr_formula <- function(fit_info, collapse = TRUE){
  forms <- deparse(fit_info$formula$formula)
  if(length(fit_info$formula$pforms) > 0){
    forms <- c(forms, deparse(fit_info$formula$pforms[[1]]))
  }
  if(collapse){
    paste0(forms, collapse = "\n")
  }
}


#' create_info_tab
#'
#' @param fit_info the dataframe with fitting information using \code{get_model_info()}
#'
#' @return a dataframe
#' @export
#'
create_info_tab <- function(fit_info){
  info <- data.frame(fit_info[-1])
  form <- get_chr_formula(fit_info)
  info$model <- form
  return(info)
}

#' get_captions
#' @description return all captions from a \code{txt} file. All captions need to be on a new line starting with \code{#} and the caption label.
#' @param file 
#'
#' @return a named character vector with captions and names
#' @export
#'
get_captions <- function(file){
  file <- readLines(file)
  file <- file[file != ""]
  cap_idx <- which(grepl("^#.*$", file))
  cap_name <- file[cap_idx]
  cap_name <- gsub("#", "", cap_name)
  captions <- file[cap_idx + 1]
  names(captions) <- cap_name
  return(captions)
}

#' get_all_packages
#' @description return all unique packages used in the project. Is a wrapper of \code{renv::dependencies()}
#' @return a character vector
#' @export
#'
get_all_packages <- function(){
  pkgs <- invisible(renv::dependencies())
  unique(pkgs$Package)
}