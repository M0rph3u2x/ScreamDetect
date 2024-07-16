#' Converts seconds into days/hours/minutes/seconds/milliseconds
#'
#' @param x time value (seconds) that must be converted
#'
#' @return Return converted time format
#' @export
#'
#' @examples convert_seconds_to_d_h_m_s_ms(x)
convert_seconds_to_d_h_m_s_ms = function(x) {
  days    <- floor(x %/% (60 * 60 * 24))
  hours   <- floor((x - days*60*60*24) %/% (60 * 60))
  minutes <- floor((x - days*60*60*24 - hours*60*60) %/% 60)
  seconds <- floor(x - days*60*60*24 - hours*60*60 - minutes*60)
  ms      <- x - days*60*60*24 - hours*60*60 - minutes*60 - seconds
  ms      <- as.numeric(round(ms,4))
  ms      <- sub(pattern = ".*\\.(.*)$", replacement = "\\1", ms)
  days_str    <- ifelse((                                           days == 0), NA, paste0(days   ))
  hours_str   <- ifelse((                              hours == 0 & days == 0), NA, paste0(hours  ))
  minutes_str <- ifelse((               minutes == 0 & hours == 0 & days == 0), NA, paste0(minutes))
  seconds_str <- ifelse((seconds == 0 & minutes == 0 & hours == 0 & days == 0), NA, paste0(seconds))
  ms_str      <- ifelse((                                             ms == 0), NA, paste0(ms))
  time_str    <- paste(days_str, hours_str, minutes_str, seconds_str, sep=":")
  time_str    <- ifelse(ms_str == 0, time_str, paste(time_str, ms_str, sep="."))
  time_str    <- str_remove_all(time_str, "NA:")
  return(time_str)
}
