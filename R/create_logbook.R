#' Create log book entry
#'
#' @param out.path   Output directory path
#' @param log_entry  Logbook data
#'
#' @return Return log book entry
#' @export
#'
#' @examples create_logbook(out.path,log_entry)
create_logbook <- function(out.path,log_entry){

  #Create csv summary table (contains cummulative data of all processed MNTBs)
  table.path <- file.path(out.path, "Detector_Log-Book.csv")

  #Create csv summary table (contains cummulative data of all processed MNTBs)
  if(!file.exists(table.path)){#File does not exist (create new table)
    #Create new table
    write.table(log_entry, file=table.path, append=FALSE, sep=",", row.names=FALSE, col.names=FALSE,  na="NA")
  }else{#File exists (append data to existing table)
    #Append data to table
    write.table(log_entry, file=table.path, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE,  na="NA")
  }

}
