#' Calculate call detection threshold based on reference calls
#'
#' @param work_path           Path to data
#' @param ref.files           Name list of reference files
#' @param ref_call            Name list of reference calls
#' @param filter              Filter
#' @param lower_cut_off_freq  Lower cutoff frequency parameter
#' @param higher_cut_off_freq Higher cutoff frequency parameter
#' @param loudness_threshold  Loudness threshold
#'
#' @return Detected pig scream calls
#' @export
#'
#' @examples identify_ref_thres(work_path,ref.files,ref_call,filter,lower_cut_off_freq,higher_cut_off_freq,loudness_threshold)
identify_ref_thres <- function(work_path,
                               ref.files,
                               ref_call,
                               filter,
                               lower_cut_off_freq,
                               higher_cut_off_freq,
                               loudness_threshold){

  #Create correct path to reference data
  ref.path <- paste(work_path, "reference_calls", sep = "/")

  #Get wav reference files
  ref.files <- list.files(ref.path, pattern = "\\.(wav|WAV)$")

  #Get wav/mp4 path
  ref.paths <- file.path(ref.path,ref.files)

  #Store threshold of each reference
  ref_thresholds <- data.frame(threshold=integer(),samplerate=integer())
  ref_threshold  <- ref_thresholds

  for(data_path in ref.paths){#data_path<-ref.paths[1]

    #Load audio file
    info     <- readWave(data_path, units="seconds", header = TRUE)
    duration <- info$samples/info$sample.rate
    audio    <- readWave(data_path, from=0, to=duration, units="seconds")

    #Band-Pass filter audio according to user settings (fir=seewave)
    if(filter=="on"){

      #Test frequency bandwidth to adapt low/highpass if necessary
      freq.test <- readWave(data_path, from=0, to=0.1, units="seconds") #Get test fragment of audio file
      freq.band <- spectro(freq.test,plot=FALSE)                       #Extract frequency bandwidth
      if(higher_cut_off_freq>floor(max(freq.band$freq))){                  #Reduce max bandpass filter if it is above the limit
        higher_cut_off_freq <- floor(max(freq.band$freq))
      }
      if(lower_cut_off_freq<floor(min(freq.band$freq))){                   #Increase min bandpass filter if it is below the limit
        lower_cut_off_freq <- floor(min(freq.band$freq))
      }

      #Apply low/high bandpass filters to audio data
      audio <- fir(audio, from=lower_cut_off_freq*1000, to=higher_cut_off_freq*1000, output="Wave")
    }

    #Amplitude data in db
    amp_data <- audio@left

    #Calculate threshold
    ref_thresholds[nrow(ref_thresholds)+1,] = c((max(amp_data)/100)*loudness_threshold,(info$sample.rate/1000))
  }

  #Get max/mean/min threshold depending on user setting
  for(kHz in unique(ref_thresholds$samplerate)){#kHz<-16
    if(ref_call == "max"){
      ref_threshold[nrow(ref_threshold)+1,] <- c(max(ref_thresholds$threshold[ref_thresholds$samplerate==kHz]),kHz) #Get max reference threshold
    }else if(ref_call == "mean"){
      ref_threshold[nrow(ref_threshold)+1,] <- c(mean(ref_thresholds$threshold[ref_thresholds$samplerate==kHz]),kHz) #Get mean reference threshold
    }else{
      ref_threshold[nrow(ref_threshold)+1,] <- c(min(ref_thresholds$threshold[ref_thresholds$samplerate==kHz]),kHz) #Get min reference threshold
    }
  }
  return(ref_threshold)
}
