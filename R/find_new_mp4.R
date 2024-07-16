#' Filter mp4 for files created in the last hour
#'
#' @param mp4_files   List of mp4 files
#' @param time_window Time window for definition new data
#' @param get_all_mp4 Switch to decide if all or only a specific set mp4 is analyzed
#'
#' @return List of newly created mp4
#' @export
#'
#' @examples find_new_mp4(mp4_files,time_window)
find_new_mp4 <- function(mp4_files,time_window,get_all_mp4){
  #mp4_files<-mp4.files

  if(get_all_mp4 == "no"){ #Load only mp4 files with a specific time frame for detection

    #get current time
    current_time <- Sys.time()

    #Split time data into date and time
    time <- c()
    time <- str_split(current_time, " ")
    time <- time[[1]]

    date_time <- time[1]
    date_time <- gsub("-", "", date_time)

    clock_time <- time[2]
    clock_time <- gsub(":", "", clock_time)

    # #Get the user time window
    hour <- str_sub(clock_time, start = 1, end = 2)
    hour <- as.numeric(hour) - time_window

    if (hour < 0) {#Time window is in previous day

      #Set time window for previous day
      hour <- 24 + hour
      clock_start <- as.numeric(hour)

      #Set date for previous day
      current_time <- Sys.time() - 86400
      time <- c()
      time <- str_split(current_time, " ")
      time <- time[[1]]
      date_time <- time[1]
      date_time <- gsub("-", "", date_time)

    }else{#Time window is today
      clock_start <- as.numeric(hour)
    }

    #Filter mp4_files by date
    #date_time <- "20220323" #DELETE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mp4_files <- mp4_files[grep(date_time, mp4_files)]

    #Get basename of mp4 file
    mp4_names <- basename(mp4_files)

    #Get timecode for hour
    mp4_timestamps <- as.numeric(str_sub(mp4_names, 18, 19))

    #Extract mp4 path from files
    new_mp4_paths <- mp4_files[mp4_timestamps >= clock_start &
                                 mp4_timestamps <= clock_start + time_window - 1]

    #Extract files that match time window
    new_mp4_files <- mp4_names[mp4_timestamps >= clock_start &
                                 mp4_timestamps <= clock_start + time_window - 1]

    #Extract name from mp4 file
    new_mp4_names <- sub(pattern = "(.*)\\.mp4$", replacement = "\\1",
                         new_mp4_files)

  }else{ #Load all mp4 files for detection

    #Extract mp4 path from files
    new_mp4_paths <- mp4_files

    #Extract files that match time window
    new_mp4_files <- basename(mp4_files)

    #Extract name from mp4 file
    new_mp4_names <- sub(pattern = "(.*)\\.mp4$", replacement = "\\1",
                         new_mp4_files)
  }

  #Create list with mp4 names and path to mp4 files
  new_mp4 <- list(new_mp4_names, new_mp4_files, new_mp4_paths)
  return(new_mp4)
}
