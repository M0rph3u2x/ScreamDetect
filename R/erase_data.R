#Erase (user defined) all directories and content based on recording date
#' Title
#'
#' @param input_path Directory to input data
#' @param erase_date Erasing date (all output data of the detector from recordings older than the erasing date is erased)
#'
#' @return Erase old data (user defined)
#' @export
#'
#' @examples erase_data(out.path,erase_date)
erase_data <- function(input_path,erase_date){#input_path<-out.path

  #Get paths with a specific folder name, find folder
  dir_list <- fs::dir_ls(path = input_path, type = "directory", regexp = "(Kamera*)", recurse = FALSE)

  #Extract dates (e.g. "20220323" [Year|Month|Day])
  extr_dates <- str_sub(dir_list, start= -26,end=-19)

  #Convert data from string to numeric
  extr_dates <- as.numeric(extr_dates)
  erase_date <- as.numeric(erase_date)
  #erase_date <- "20220324"

  #Find directories that should be deleted
  erase_pos <- extr_dates<erase_date

  #Erase directories
  unlink(dir_list[erase_pos], recursive=TRUE)
}
