#' Convert mp4 to wav format
#'
#' @param mp4_paths Path to mp4 file
#'
#' @return wav files (from mp4)
#' @export
#'
#' @examples mp4_to_wav_converter(mp4_paths)
mp4_to_wav_converter <- function(mp4_paths){

  for(mp4_path in mp4_paths){#mp4_name<-mp4.files[1]
    wav_path <- str_replace(mp4_path, ".mp4", ".wav")

    #Create call cutting command for ffmpeg (cmd line function)
    cmd <- paste("ffmpeg -i ",
                 mp4_path,      #Input path + mp4 name
                 " ",
                 wav_path,  #Output path + mp4 name
                 sep="")

    #Execute video cutting of selected call
    system(cmd)
  }
}
