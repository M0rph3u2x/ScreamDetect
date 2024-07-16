#' Identify pig screams that fit the parameters provided by the user
#'
#' @param wav_path             Path to wave files
#' @param filter               Information if audio filter function is active
#' @param lower_cut_off_freq   Lower cutoff frequency
#' @param higher_cut_off_freq  Higher cutoff frequency
#' @param loudness_threshold   Loudness threshold (only calls above the threshold value are detected)
#' @param time_gap             Time gap between calls
#' @param call_dur             Minimal duration time a call must have
#' @param ref_switch           Reference switch (if on reference audio files are created)
#' @param scream_threshold     Scream threshold (a warning message is send if the number of detected calls is above the threshold)
#'
#' @return Return table with pig screams
#' @export
#'
#' @examples identify_pig_screams(wav_path,filter,lower_cut_off_freq,higher_cut_off_freq,loudness_threshold,time_gap,call_dur,ref_switch,scream_threshold=NULL)
identify_pig_screams <- function(wav_path,
                                 filter,
                                 lower_cut_off_freq,
                                 higher_cut_off_freq,
                                 loudness_threshold,
                                 time_gap,
                                 call_dur,
                                 ref_switch,
                                 scream_threshold=NULL){

  #Load audio file
  info     <- readWave(wav_path, units="seconds", header = TRUE)
  duration <- info$samples/info$sample.rate
  audio    <- readWave(wav_path, from=0, to=duration, units="seconds")

  #Band-Pass filter audio according to user settings (fir=seewave)
  if(filter=="on"){

    #Test frequency bandwidth to adapt low/highpass if necessary
    freq.test <- readWave(wav_path, from=0, to=0.1, units="seconds") #Get test fragment of audio file
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

  #Create time data in ms
  values_per_ms     <- info$sample.rate/1000 #db-values per ms in audio file
  sec_switch        <- 1
  samplerate_switch <- 1
  time_data         <- c()

  for(i in 1:length(amp_data)){
    time_data[i]      <- sec_switch
    samplerate_switch <- samplerate_switch+1
    if(samplerate_switch>values_per_ms){
      sec_switch <- sec_switch+1
      samplerate_switch <- 1
    }
  }

  #Create audio data table
  audio_table <- data.frame(amp_dB=amp_data,time_ms=time_data)

  if(ref_switch == "off"){
    #Calculate threshold
    scream_threshold <- (max(amp_data)/100)*loudness_threshold

    #Get positions where amplitude is above threshold
    db_above_thres <- audio_table[audio_table$amp_dB>scream_threshold,]
  }else{

    #Get samplerate of query video
    sr <- info$sample.rate/1000

    if(any(scream_threshold$samplerate==sr)==TRUE){ #Check if sample rate fits with reference
      #Get positions where amplitude is above threshold
      pos            <- audio_table$amp_dB>scream_threshold$threshold[scream_threshold$samplerate==sr]
      db_above_thres <- audio_table[pos,]
    }else{#If video samplerate is different than reference samplerate
      if(sr<min(scream_threshold$samplerate)){#Get lowest reference threshold
        #Get positions where amplitude is above threshold
        pos            <- audio_table$amp_dB>scream_threshold$threshold[min(scream_threshold$samplerate)==scream_threshold$samplerate]
        db_above_thres <- audio_table[pos,]
      }else if(sr>min(scream_threshold$samplerate)){#Get highest reference threshold
        #Get positions where amplitude is above threshold
        pos            <- audio_table$amp_dB>scream_threshold$threshold[max(scream_threshold$samplerate)==scream_threshold$samplerate]
        db_above_thres <- audio_table[pos,]
      }else{#Calculate mean reference threshold
        #Get positions where amplitude is above threshold
        pos            <- audio_table$amp_dB>mean(scream_threshold$threshold)
        db_above_thres <- audio_table[pos,]
      }
    }
  }

  #Sort "db_above_thres database" by time
  db_above_thres <- db_above_thres[order(db_above_thres$time_ms, db_above_thres$amp_dB),]
  #db_above_thres$Pos <- 1:length(db_above_thres$amp_dB)
  #Create on/offset data of detected screams
  onset      <- c() #Start time of call
  offset     <- c() #End time of call
  detections <- c() #Number of peaks above scream threshold
  call_id    <- 1   #On/offset event id

  #Get on/offset values
  for(i in 1:length(db_above_thres$amp_dB)){#i<-31
    if(i==1){ #Create first onset
      onset[call_id]      <- db_above_thres$time_ms[i]
      offset[call_id]     <- db_above_thres$time_ms[i]
      detection_counter   <- 1 #Number of observed peaks per call unit
      detections[call_id] <- detection_counter
    }
    if(i>1){ #Find time gap and create next on/offset pair
      timedif <- db_above_thres$time_ms[i]-db_above_thres$time_ms[i-1]
      if(timedif>time_gap){
        detection_counter   <- detection_counter+1
        detections[call_id] <- detection_counter
        offset[call_id]     <- db_above_thres$time_ms[i-1]
        call_id             <- call_id+1
        detection_counter   <- 1
        onset[call_id]      <- db_above_thres$time_ms[i]
        offset[call_id]     <- db_above_thres$time_ms[i]
        detections[call_id] <- detection_counter
      }else{
        offset[call_id]   <- db_above_thres$time_ms[i]
        detection_counter <- detection_counter+1
      }
    }
  }

  #Create on/offset table of detected screams
  pig_scream_table <- data.frame(Onset=onset,Offset=offset,Detections=detections)

  #Discard short intense noises based on minimal required call duration
  pig_scream_table <- pig_scream_table[(pig_scream_table$Offset-pig_scream_table$Onset)>call_dur,]

  if(length(pig_scream_table$Onset) > 0 ){
    #Modify names
    names(pig_scream_table) <- c("Onset_ms","Offset_ms","Detections")

    #Check if calls have been detected
    #Format call on/offset if detected
    if(length(pig_scream_table$Onset)>0){

      #Add two columns seconds
      pig_scream_table$Onset_sec  <- pig_scream_table$Onset_ms /1000
      pig_scream_table$Offset_sec <- pig_scream_table$Offset_ms/1000

      #Add two columns minutes
      options(digits.secs = 3) #Show milliseconds when listing time
      pig_scream_table$Onset_min  <- format(as.POSIXct(Sys.Date(), tz="GMT")+pig_scream_table$Onset_sec , "%M:%OS")
      pig_scream_table$Offset_min <- format(as.POSIXct(Sys.Date(), tz="GMT")+pig_scream_table$Offset_sec, "%M:%OS")
      options(digits.secs = 0) #Set options back to original configuration

      #Add duration of detected calls
      pig_scream_table$Duration_ms <- pig_scream_table$Offset_ms-pig_scream_table$Onset_ms

      #Rearrange order in dataframe
      pig_scream_table <- pig_scream_table[,c(1,2,4,5,6,7,3,8)]
    }
  }else{
    pig_scream_table <- data.frame(Onset=0,Offset=0,Detections=0)
  }

  #Delete NAs
  pig_scream_table <- na.omit(pig_scream_table)

  return(pig_scream_table)
}
