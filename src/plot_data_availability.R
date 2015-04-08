library(ggplot2)
#this script plots data availability start and end dates for all files in all_data_summary

plot_data_availability <- function(all_data_summary){
    if(!"data.frame" %in% class(all_data_summary)) stop("all_data_summary must be data frame")
    if(!all(c("file", "min_date", "max_date") %in% names(all_data_summary))) stop("file, min_date, and max_date must be columns of all_data_summary")
    
    all_data_summary <- all_data_summary[,c("file", "min_date", "max_date", "dept")]   #reorder columns
    
    file_summary <- unique(all_data_summary[,c(1,2,3,4)])
    
    plot_a <- ggplot(file_summary) + geom_segment(aes(y = file,yend = file, x = min_date, xend =max_date, color=dept,fill=dept), size=3)  +
      theme_minimal() + ggtitle("Data Availability Over Time") +
      scale_x_datetime(breaks = date_breaks("7 day"), labels=date_format("%b-%d")) + xlab("Week") 
    suppressWarnings(print(plot_a))
}