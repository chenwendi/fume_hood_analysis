library(ggplot2)

plot_data_availability <- function(all_data_summary){
    file_summary <- unique(all_data_summary[,c(1,2,3)])
    wrong_year <-which(format(file_summary$min_date,"%Y")<2000)
    for(y in wrong_year){
        file_summary$min_date[y] <- paste0(as.numeric(substr(as.character(file_summary$min_date[y]),1,4)) +2000, substr(file_summary$min_date,5,19))
        file_summary$max_date[y] <- paste0(as.numeric(substr(as.character(file_summary$max_date[y]),1,4)) +2000, substr(file_summary$max_date,5,19))
    }
    plot_a <- ggplot(file_summary) + geom_segment(aes(y = file,yend = file, x = min_date, xend =max_date ), size=3)  +
        theme_minimal() + ggtitle("Data Availability by file") +
        scale_x_datetime(breaks = date_breaks("7 day"), labels=date_format("%y-%b-%d"))
    suppressWarnings(print(plot_a))
}