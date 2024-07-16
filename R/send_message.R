#Source: https://www.seancarney.ca/2020/10/07/sending-email-from-outlook-in-r/
#' Send warning email
#'
#' @param table.path       Path to table
#' @param data_name        Name of data
#' @param pig_scream_table Table that lists detected calls
#' @param mp4_name         Name of mp4 file
#' @param email_receiver   email address or adresses of data recipients
#'
#' @return Return warning email
#' @export
#'
#' @examples send_message(table.path,data_name,pig_scream_table,mp4_name,email_receiver)
send_message <- function(table.path,data_name,pig_scream_table,mp4_name,email_receiver){

  #Depends on library(RDCOMClient)

  #Modify table path for outlook attachment
  subject_mail    <- paste("Pigscream warning: ", data_name,sep="")
  body_mail       <- paste(length(pig_scream_table$Onset_ms), " calls detected in video: ",mp4_name, sep="")
  table_path_mail <- str_replace_all(table.path, "[/]", "\\\\")

  # Open Outlook
  Outlook <- COMCreate("Outlook.Application")

  # Create a new message
  Email = Outlook$CreateItem(0)

  # Set the recipient, subject, and body
  Email[["to"]]      = email_receiver  #; recipient2@test.com; recipient3@test.com"
  Email[["cc"]]      = ""
  Email[["bcc"]]     = ""
  Email[["subject"]] = subject_mail
  Email[["body"]]    = body_mail
  Email[["Attachments"]]$Add(table_path_mail)

  # Send the message
  Email$Send()

  # Close Outlook, clear the message
  rm(Outlook, Email)
}
