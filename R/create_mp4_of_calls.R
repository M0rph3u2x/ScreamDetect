#' Create video files of detected calls
#'
#' @param pig_scream_table Pig Scream Table data
#' @param audio_ref_filter Reference filter status (on/off)
#' @param mp4_path         Path to mp4 data
#' @param mp4_name         Names of mp4 data
#' @param mp4_dur          Duration of mp4 calls
#' @param out.dir          Directory of output folder
#' @param wav_dur          Audio file duration
#'
#' @return Return video files of detected calls
#' @export
#'
#' @examples create_mp4_of_calls(pig_scream_table,audio_ref_filter,mp4_path,mp4_name,mp4_dur,out.dir,wav_dur,cor_temps=NULL)
create_mp4_of_calls <- function(pig_scream_table,
                                audio_ref_filter,
                                mp4_path,
                                mp4_name,
                                mp4_dur,
                                out.dir,
                                wav_dur){

  #Exchange input with output path
  mp4_out <- file.path(out.dir, mp4_name)

  for (i in length(pig_scream_table$Onset_min):1) {#i<-35

    #Create call id label
    call_nr <- paste("_call_", i, ".mp4",sep="")

    #Switch ending of original file with call id
    this_mp4_out <- sub(".mp4$", call_nr, mp4_out)

    #Get duration of selected call event
    duration <- pig_scream_table$Offset_ms[i]-pig_scream_table$Onset_ms[i]

    #Check and edit time duration if smaller than required mp4 duration
    if(duration<mp4_dur){

      add_time <- (mp4_dur-duration)/2

      mod_start <- (pig_scream_table$Onset_ms[i] -add_time)/1000
      mod_end   <- (pig_scream_table$Offset_ms[i]+add_time)/1000

      #Check start time for validity and transform to "minutes:seconds:milliseconds"
      if(mod_start<0){
        begin <- 0
      }else{
        begin <- mod_start
      }

      #Convert time to day/hour/minute/seconds/milliseconds
      start_time <- convert_seconds_to_d_h_m_s_ms(begin)


      #Check end time for validity and transform to "hour:minutes:seconds:milliseconds"
      if(mod_end>wav_dur){

        #Convert time to day/hour/minute/seconds/milliseconds
        end_time  <- convert_seconds_to_d_h_m_s_ms(pig_scream_table$Offset_sec[i])

        #Convert time to day/hour/minute/seconds/milliseconds
        begin      <- (pig_scream_table$Onset_ms[i] -(mp4_dur-duration))/1000
        start_time <- convert_seconds_to_d_h_m_s_ms(begin)
      }else{
        #Convert time to day/hour/minute/seconds/milliseconds
        end      <- mod_end
        end_time <- convert_seconds_to_d_h_m_s_ms(end)
      }
    }else{
      start_time <- convert_seconds_to_d_h_m_s_ms(pig_scream_table$Onset_ms[i]/1000)
      end_time   <- convert_seconds_to_d_h_m_s_ms(pig_scream_table$Offset_ms[i]/1000)
    }

    #Create short control video from original mp4 -------------

    #Create call cutting command for ffmpeg (cmd line function)
    cmd <- paste("ffmpeg",
                 " -ss ",
                 start_time,    #Start time
                 " -to ",
                 end_time,      #Stop time
                 " -i ",
                 mp4_path,      #Path to mp4 file
                 " -c copy ",
                 this_mp4_out,  #Output path + mp4 name
                 sep="")

    #Execute video cutting of selected call
    system(cmd)

    if(audio_ref_filter=="on"){

      #Create short reference video from original mp4 -------------

      start_time <- format(as.POSIXct(Sys.Date(), tz="GMT")+((pig_scream_table$Onset_ms[i]-100)/1000), "%M:%OS")
      end_time   <- format(as.POSIXct(Sys.Date(), tz="GMT")+((pig_scream_table$Offset_ms[i]+2000)/1000), "%M:%OS")

      #Create call cutting command for ffmpeg (cmd line function)
      call_name <- paste("call_", i, ".wav",sep="")
      call_name <- sub(basename(mp4_out), call_name, mp4_out)

      cmd <- paste("ffmpeg",
                   " -ss ",
                   start_time,    #Start time
                   " -to ",
                   end_time,      #Stop time
                   " -i ",
                   mp4_path,      #Path to mp4 file
                   " ",
                   call_name,     #Output path + wav name
                   sep="")

      #Execute mp4 conversion to wav
      system(cmd)

      #Create picture of detected call  -------------

      hit_call <- readWave(call_name)
      windows() #Create plot in new window
      viewSpec(hit_call) # Show spectrogram
      this_png_out <- str_replace(call_name, ".wav",".png")
      dev.copy(png,this_png_out)
      while (!is.null(dev.list()))  dev.off() #Save plot & close window

    }#End audio_ref_filter
  }#End call loop
}
