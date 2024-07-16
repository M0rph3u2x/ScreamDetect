#' Get all mp4 files in folder "surveillance"
#'
#' @param work_path A path to the input mp4 files
#' @param keywords Add search keywords to identify mp4 directories
#'
#' @return List of mp4 file locations
#' @export
#'
#' @examples get_mp4_files(work_path)
get_mp4_files <- function(work_path,keywords){

  #Create path to camera folders
  mp4_paths  <- sub(pattern = "(.*/).*$", replacement = "\\1", work_path)

  #Separate keywords
  keywords <- strsplit(keywords, "_", fixed=T)

  for(keyword in keywords){

    pattern <- paste("(",keyword,")",sep="")

    #Get path of all folders including "AM|PM" in their folder name
    mp4_paths <- dir(path = mp4_paths, pattern = pattern, full.names = TRUE, recursive = FALSE)

  }

  #Filter camera paths for mp4 file paths
  mp4.files    <- fs::dir_ls(path = mp4_paths, type = "file", glob = "*.mp4", recurse = TRUE)

  return(mp4.files)
}
