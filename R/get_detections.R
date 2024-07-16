#Subfunction of "create_mp4_of_calls"
#' Searches for matches between templates and detected calls
#'
#' @param wav_path  Path to wave data
#' @param cor_temps Template data
#'
#' @return Return matches between calls and templates
#' @export
#'
#' @examples get_detections(wav_path,cor_temps)
get_detections <- function(wav_path,cor_temps){

  #Calculate correlation score between the templates and each time bin in the detected call
  cor_scores <- corMatch(wav_path, cor_temps, time.source = "fileinfo")
  #cor_scores <- binMatch(wav_path, cor_temps, time.source = "fileinfo")

  #------------------------------------------------------------------------------------------------------------------------------

  #Identify "peaks" in the scores
  cor_peaks <- findPeaks(cor_scores)

  #------------------------------------------------------------------------------------------------------------------------------

  #Determine which, if any, score peaks exceed the score cutoff
  cor_detects <- getDetections(cor_peaks)

  return(cor_detects)
}
