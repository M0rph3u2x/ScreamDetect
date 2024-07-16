#Create an overview activity plot of the pig scream frequency
#' Title
#'
#' @param input.path Path to input data
#' @param scream_thres Scream threshold (a value higher than the threshold will alarm the user)
#' @param filter_date If filter date is activated only data within a user defined period is analysed
#' @param start_date Start of the user defined time period of interest
#' @param end_date End of the user defined time period of interest
#'
#' @return Create activity plot of detected pig screams
#' @export
#'
#' @examples activity_plot(out.path,scream_thres,filter_date,start_date,end_date)
activity_plot <- function(input.path,scream_thres,filter_date,start_date=0,end_date=0){

  #Filter output path for xlsx files
  xlsx.files    <- fs::dir_ls(path = input.path, type = "file", glob = "*.xlsx", recurse = TRUE)

  #Load and rbind all xlsx data
  all_xlsx <- data.frame()
  for(file in xlsx.files){
    my_data <- read_excel(file)
    my_data$Filename <- basename(dirname(file))   #Extract name
    info <- strsplit(basename(dirname(file)), "-", fixed=T)      #Split string by symbol "-"
    my_data$Kamera    <- info[[1]][1]
    my_data$Date      <- info[[1]][2]
    my_data$Time      <- info[[1]][3]
    my_data$Hour      <- str_sub(info[[1]][3], start= 1,end=2)
    my_data$Frequency <- nrow(my_data)
    all_xlsx <- rbind(all_xlsx,my_data)
  }
  rm(my_data)

  #Filter data for user defined time window
  if(filter_date == "on"){
    date_list         <- all_xlsx$Date
    date_list         <- as.numeric(date_list)
    all_xlsx$Date_num <- date_list
    all_xlsx          <- subset(all_xlsx, Date_num >= start_date & Date_num <= end_date)
  }

  #-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------

  #Create Activity Plot for each Kamera
  cams <- unique(all_xlsx$Kamera)

  for (cam in cams){#cam<-cams[1]

    #Create Activity Plot for each Kamera
    cams <- unique(all_xlsx$Kamera)

    cam_data <- all_xlsx[all_xlsx$Kamera==cam,] #Extract data from a single camera
    #cam_data$Hour <- as.numeric(cam_data$Hour)  #Convert data from string to numeric
    #tidyverse
    #Working

    #Rotate date label if to more then 10 are plotted:
    if(length(unique(cam_data$Hour))>=100){
      ggplot(cam_data) +
        geom_bar(aes(x = Hour, group = Date, colour="Screams",fill="red"), show.legend = FALSE)+
        geom_hline(aes(yintercept=scream_thres,linetype ="dashed"), linewidth=2, color = "green", show.legend = FALSE)+ #Alarm threshold
        facet_grid(~Date, space="free_x", scales="free_x", switch="x") +
        theme(plot.title=element_text(margin=margin(t=40,b=-30),hjust = 0.5),
              strip.placement = "outside",
              strip.background = element_rect(fill=NA,colour="grey50"),
              strip.text.x = element_text(size=8, angle=90),
              axis.text.x  = element_text(color=c("transparent")),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.spacing=unit(0,"cm")) +
        labs(title = cam,
             x = 'Time[Hours/Recording date]',
             y = 'Number of detected screams')
      name <- paste("output/",cam,".jpg",sep="")
      ggsave(name, dpi=300, width=8, height=4)
    }else if(length(unique(cam_data$Hour))<100){
      ggplot(cam_data) +
        geom_bar(aes(x = Hour, group = Date, colour="Screams",fill="red"), show.legend = FALSE)+
        geom_hline(aes(yintercept=scream_thres,linetype ="dashed"), linewidth=2, color = "green", show.legend = FALSE)+ #Alarm threshold
        facet_grid(~Date, space="free_x", scales="free_x", switch="x") +
        theme(plot.title=element_text(margin=margin(t=40,b=-30),hjust = 0.5),
              strip.placement = "outside",
              strip.background = element_rect(fill=NA,colour="grey50"),
              strip.text.x = element_text(size=8, angle=90),
              axis.text.x  = element_text(size=6, angle=90),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.spacing=unit(0,"cm")) +
        labs(title = cam,
             x = 'Time[Hours/Recording date]',
             y = 'Number of detected screams')
      name <- paste("output/",cam,".jpg",sep="")
      ggsave(name, dpi=300, width=8, height=4)
    }
  }
}
