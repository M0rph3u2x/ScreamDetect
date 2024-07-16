#' Title Task Scheduler for Pig Scream Detector
#'
#' @param path Path to detection project folder
#' @param script_name Name of R script that will be executed on a user defined time schedule
#'
#' @return Set time schedule for automated pig scream detection
#' @export
#'
#' @examples task_scheduler_psd("C:/Users/tjard/OneDrive/Desktop/PSD_Project/","psd.R")
#' @examples task_scheduler_psd(path,script_name)
task_scheduler_psd <- function(path,script_name){

  #Source: https://www.r-bloggers.com/2020/05/how-to-schedule-r-scripts/

  # Load necessary R packages ----------------------------------------------------

  # Define necessary package manager
  packages <- c("pacman")

  # Install package manager if not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  #Install and/or load all necessary packages via package manager "pacman"
  pacman::p_load(taskscheduleR)

  #-------------------------------------------------------------------------------

  # Get and save software location -----------------------------------------------

  #Set working directory
  setwd(path)

  #Create PSD_Software location path
  PSD_Path <- paste(path,script_name,sep="")

  #-------------------------------------------------------------------------------

  #Load parameters from folder "parameters" --------------------------------------
  parameters <- load_program_parameters(path)

  #Get test run setting (if setting is "off", the detector is only run once!)
  test_run <- parameters[20,2]

  #Get user defined time window from parameters (analyse all records within the defined time window)
  interval1 <- parameters[21,2]

  #Get user defined time window from parameters (analyse all records within the defined time window)
  interval2 <- as.numeric(parameters[22,2]) #1 equals a window of one hour in the past
  #if it is 8:34 the detector will search for
  #mp4 files with a time stamp between 7:00 and 7:59:59

  #Release memory
  rm(parameters)

  #-------------------------------------------------------------------------------

  if(test_run == "on"){

    #Reseting/Deleting tasks from the scheduler
    taskscheduler_delete("test_run")

    #Run script once (start time is 60 seconds after execution)
    taskscheduler_create(taskname = "test_run", rscript = PSD_Path,
                         schedule = "ONCE", starttime = format(Sys.time() + 60, "%H:%M"))
  }else{

    #Reseting/Deleting tasks from the scheduler
    taskscheduler_delete("PSD_run")

    #Run script periodically based on user setting
    taskscheduler_create(taskname = "PSD_run", rscript = PSD_Path,
                         schedule = interval1, starttime = format(Sys.time() + 60, "%H:%M"), modifier = interval2)
  }
}

