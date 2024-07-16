#' Title ScreamDetect
#'
#' @param path Path to R script
#'
#' @return Detected pig screams
#' @export
#'
#' @examples ScreamDetect("path_to_R_script")
#' @examples ScreamDetect("C:/Users/tjard/OneDrive/Desktop/PSD/")
ScreamDetect <-function(path){

  # 1) Define paths for R-Script -------------------------------------------------

  #Define path to R script
  R.Directory <- path

  #Path shortcut function
  FullPath = function(FileName){ return( paste(R.Directory,FileName,sep="") ) }

  # Set the working directory to the R script location
  setwd(R.Directory)
  work_path <- getwd()

  # Setup directory where data output is stored ------------------

  #Create main output folder
  out.path <- FullPath("output")
  dir.create(out.path, showWarnings = FALSE)

  #-------------------------------------------------------------------------------

  # 2) Install/Load packages into RStudio ----------------------------------------

  # Define package manager
  packages <- c("pacman")

  # Install package manager if not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  #---------------------------

  # Define package email client manager
  packages <- c("devtools","RDCOMClient")

  # Install package manager if not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    if(installed_packages[1] == FALSE){
      install.packages("devtools")
      library("devtools")
      install_github('omegahat/RDCOMClient')
    }else{
      library("devtools")
      install_github('omegahat/RDCOMClient')
    }
  }

  #---------------------------

  #Install and/or load all necessary packages via package manager "pacman"
  pacman::p_load(base,              #functions(list.files,sub,file.path,dir.create,print,paste0,format,as.POSIX,Sys.Date,options,dput,dget): Basic data processing functions
                 fs,                #functions(fs::dir_ls): Find and list files of a specific type (faster than list.files)
                 ggplot2,
                 RDCOMClient,       #function(COMCreate): Send outlook email messages
                 readxl,            #function(read_excel): Reads excel files
                 seewave,           #functions(spectro, fir): Audiodata analysis
                 stringr,           #functions(str_replace)
                 TAF,               #function(rmdir): Remove empty folders in output directory
                 tuneR,             #function(readWave): Audiodata analysis
                 xlsx)              #function(write.xlsx): Data export to xlsx file

  #-------------------------------------------------------------------------------

  # 3) Load program parameters ---------------------------------------------------

  # Create log book entry
  log_entry <- paste("Start_Time_Detector: ", Sys.time(), sep="")
  create_logbook(out.path,log_entry)

  #Load parameters from folder "parameters"
  parameters <- load_program_parameters(R.Directory)

  #Transfer parameters to easier understandable variables

  #Add keywords to better identify mp4 location
  #(e.g.: "Kamera" (first directory layer), "AM|PM" (second directory layer))
  #AM|PM -> keyword can be either AM or PM ("|" is the "or" symbol for regular expression search patterns in R)
  keywords <- parameters[1,2]

  #Reference calls switch
  #If "TRUE" threshold parameters will be generated based on
  #the reference audio files
  ref_switch <- parameters[2,2] #Use of reference: "on" or "off"

  #Reference focus (can be on quiet, mean or loudest reference call [dB])
  ref_call <- parameters[3,2]

  #Loudness threshold (%) - Only calls above threshold are listed (amplitude is measured)
  loudness_threshold <- as.numeric(parameters[4,2])

  #Time_gap_parameter (Defines minimal gap time between two measurements above threshold in ms)
  #Important to calculate on/offset values
  time_gap <- as.numeric(parameters[5,2])

  #Call_duration_parameter (Defines minimal call duration in ms)
  #Important to calculate minimal required call duration (filters out intense short noises)
  call_dur <- as.numeric(parameters[6,2])

  #Get the minimal length each call video must have (in ms)
  mp4_dur <- as.numeric(parameters[7,2])

  #Band-Pass filter (kHz)
  filter              <- parameters[8,2] #Filter "on" or "off"
  lower_cut_off_freq  <- as.numeric(parameters[9,2]) #Delete audio frequency below frequency threshold
  higher_cut_off_freq <- as.numeric(parameters[10,2]) #Delete audio frequency above frequency threshold

  #Create audio reference filter (on/off)
  audio_ref_filter <- parameters[11,2]

  #Load all audio files for detection (yes/no)
  get_all_mp4 <- parameters[12,2]

  #Scream frequency threshold (number of screams per video)
  #If frequency is above threshold the user will be notified per email
  scream_freq_thres <- as.numeric(parameters[13,2]) #Notify user if more than five screams are detected

  #Create activity plot (on/off)
  act_switch <- parameters[14,2]

  #Create activity plot for a specific time period
  filter_date <- parameters[15,2]

  #Start activity plot from:
  start_date  <- parameters[16,2]

  #End activity plot at:
  end_date    <- parameters[17,2]

  #Switch for erasing data (yes/no)
  erase_data <- parameters[18,2]

  #Erasing date (all datafiles before the date are deleted)
  erase_date <- parameters[19,2]

  #Define email receiver for warning messages
  email_receiver <- parameters[20,2] #Can be more than one address (e.g. "user1@recipient.com, user2@recipient.com")

  #time window (analyse all records within the defined time window)
  time_window <- as.numeric(parameters[23,2]) #1 equals a window of one hour in the past
  #if it is 8:34 the detector will search for
  #mp4 files with a time stamp between 7:00 and 7:59:59

  #Release memory
  rm(parameters)

  #-------------------------------------------------------------------------------

  # 4) Identify new mp4 entries (defined by negative timer "past_time") ----------

  print("Identify new mp4 entries")

  #Get all mp4 files in folder "surveillance"
  mp4_files <- get_mp4_files(work_path,keywords)

  #Locate latest mp4 entries
  new_mp4       <- find_new_mp4(mp4_files,time_window,get_all_mp4)
  new_mp4_names <- new_mp4[[1]]
  new_mp4_files <- new_mp4[[2]]
  new_mp4_paths <- new_mp4[[3]]

  #Release memory
  rm(new_mp4)

  #-------------------------------------------------------------------------------

  # Optional) Create reference scream threshold ----------------------------------

  #Get reference data (if activated)
  if(ref_switch == "on"){

    #Identify loudness threshold of reference calls
    scream_threshold   <- identify_ref_thres(work_path,
                                             ref.files,
                                             ref_call,
                                             filter,
                                             lower_cut_off_freq,
                                             higher_cut_off_freq,
                                             loudness_threshold)
  }

  #-------------------------------------------------------------------------------

  # 5) Scan videos for pig screams -----------------------------------------------

  if(length(new_mp4_names)>=1){#Control if recordings are detected
    for(dataset.nr in 1:length(new_mp4_names)){#dataset.nr<-1

      # 5A) Extract audio data from mp4 file -------------------------------------

      #Create wav from mp4
      mp4_to_wav_converter(new_mp4_paths[dataset.nr])

      #Get wav input files
      wav_path <- str_replace(new_mp4_paths[dataset.nr], ".mp4", ".wav")

      #---------------------------------------------------------------------------

      #Create data specific output folder
      out.dir <- file.path(out.path,new_mp4_names[dataset.nr])
      dir.create(out.dir, showWarnings = FALSE)

      # 5B) Identify calls above threshold ---------------------------------------

      #Identify pig screams that fit the parameters provided by the user
      pig_scream_table <- identify_pig_screams(wav_path,
                                               filter,
                                               lower_cut_off_freq,
                                               higher_cut_off_freq,
                                               loudness_threshold,
                                               time_gap,
                                               call_dur,
                                               ref_switch,
                                               scream_threshold)

      #Delete wav files from hard drive
      #Check its existence
      if (file.exists(wav_path)) {

        #Check input duration
        info    <- readWave(wav_path, units="seconds", header = TRUE)
        wav_dur <- info$samples/info$sample.rate

        #Delete file if it exists
        file.remove(wav_path)
      }

      # 5C) Create log book entry ------------------------------------------------

      log_entry <- paste("Time: ", Sys.time(), "; ",length(pig_scream_table$Onset_ms), " calls detected in video: ",new_mp4_names[dataset.nr], sep="")
      create_logbook(out.path,log_entry)

      #---------------------------------------------------------------------------

      #Go to next dataset if no scream has been detected
      if(length(pig_scream_table$Onset_ms)==0){
        print(paste0("Continue with next file: ", new_mp4_names[dataset.nr+1]))
        next
      }

      #---------------------------------------------------------------------------

      # 5D) Create video files of detected calls ---------------------------------

      #Check if calls have been detected
      #Format call on/offset if detected
      if(length(pig_scream_table$Onset_ms)>0){

        create_mp4_of_calls(pig_scream_table,
                            audio_ref_filter,
                            new_mp4_paths[dataset.nr],
                            new_mp4_files[dataset.nr],
                            mp4_dur,
                            out.dir,
                            wav_dur)
      }

      #---------------------------------------------------------------------------

      # 5E) Create overview table ------------------------------------------------

      #Create xlsx summary table (contains on/offsets of detected calls)
      xlsx.table.name  <- "Pig_call_table.xlsx"
      table.path       <- file.path(out.dir, xlsx.table.name)
      write.xlsx(pig_scream_table,table.path,row.names=FALSE)

      #---------------------------------------------------------------------------

      # 5F) Notify user if scream frequency is higher than threshold -------------

      #Send warning message when call count equals or surpasses threshold
      if(length(pig_scream_table$Onset_ms)>=scream_freq_thres){

        #Create warning message and send via outlook
        send_message(table.path,new_mp4_names[dataset.nr],pig_scream_table,new_mp4_files[dataset.nr],email_receiver)
      }

      #---------------------------------------------------------------------------

      #Write feedback info about the next selected dataset
      if(dataset.nr<length(new_mp4_names)){
        print(paste0("Continue with next file: ", new_mp4_names[dataset.nr+1]))
      }
    }
  }

  #Remove empty folders of files without detected screams
  #Dependent on package(TAF)
  rmdir(out.path, recursive = TRUE)

  #Create activity plot (if activated) -------------------------------------------
  if(act_switch == "on"){

    #Filter data for user defined time period
    if(filter_date == "on"){
      activity_plot(out.path,scream_freq_thres,filter_date,start_date,end_date)
    }else{
      activity_plot(out.path,scream_freq_thres,filter_date)
    }
  }

  #-------------------------------------------------------------------------------

  #Erase old data ----------------------------------------------------------------

  #Filter data for user defined time period
  if(erase_data == "yes"){
    erase_data(out.path,erase_date)
  }

  #-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  print("Finished processing the data!")
  #-------------------------------------------------------------------------------
}
