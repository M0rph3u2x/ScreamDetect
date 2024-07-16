#' Load parameters from folder "parameters"
#'
#' @param path Path to project folder
#'
#' @return Return user parameters
#' @export
#'
#' @examples load_program_parameters()
#'
#'
load_program_parameters <- function(path){

  #Create path to parameters
  parameter.path <- paste(path,"parameters",sep="")

  #Get parameter files
  parameter.csv   <- list.files(parameter.path, pattern = "\\.(csv)$")
  parameter.xlsx  <- list.files(parameter.path, pattern = "\\.(xlsx)$")

  #Load parameters
  if(length(parameter.csv)>=1){#csv
    config.files <- list.files(parameter.path, pattern = "\\.(csv)$")
    path.config  <- file.path(parameter.path, max(config.files))
    parameters   <- read.csv(path.config, header = FALSE)
  }else if(length(parameter.xlsx)>=1){#xlsx
    config.files      <- list.files(parameter.path, pattern = "\\.(xlsx)$")
    path.config       <- file.path(parameter.path, max(config.files))
    parameters        <- read_excel(path.config, col_names = FALSE)
    names(parameters) <- c("1","2")
  }else{#no parameters
    print("No parameters identified!")
    quit
  }
  return(parameters)
}
