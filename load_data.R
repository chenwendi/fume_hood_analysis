options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)

#source all functions
wd_src <- "./src/"
sapply(list.files(wd_src), FUN = function(f){source(paste0(wd_src,f))}) 

# get filenames
wd_data    <- "../data/"                                           #csv data is saved in this folder
data_files <- list.files(wd_data,pattern = ".csv")

# set directory where all output will be saved
wd_output <- "../output/"

all_formatted_data <- list()
for(file in 1:length(data_files)){
    original_data <- readLines(paste0(wd_data, data_files[file])) #read lines from csv file
    
    ####### determine file formatting
    file_format <- check_format(original_data)    #check formatting of file
    
    ###### choose formatting function based on current file formatting
    if(file_format=="wide_date format"){          formatted_data <- format_wide_data(original_data)
    }else if(file_format=="long_date format"){    formatted_data <- format_long_data(original_data)
    }else{                                        warning(paste(data_files[file],": data not in recognizable format"))
    }    
       
    all_formatted_data[[file]]<- formatted_data
}

#begin pdf creation for all graphs
pdf(paste0(wd_output,"sash_data.pdf"), width=12, height=4)

#begin to loop through each set of fume hood data
all_data_summary <- lapply(1:length(all_formatted_data), FUN=function(c){
    print(c)
    ##### basic calculations, summary of the data included in a given file
    data_c <- all_formatted_data[[c]]
    min_date <- min(data_c$dttm)
    max_date <- max(data_c$dttm)
    range    <- max_date-min_date
    
    intervals <- data_c$dttm[1:10]- lag(data_c$dttm[1:10],1)
    interval <- median(intervals, na.rm=T)
    
    report_summary <- data.frame(file=c, min_date, max_date, days=round(range,2), interval)
    
    #### summary of individual fume hoods
    fume_hood_names <- setdiff(names(data_c),"dttm")
    hood_summary <- lapply(fume_hood_names, FUN=function(h){
        c_index  <- which(names(data_c)==h)
        h_data   <- data_c[,c_index]
        max_v    <- max(h_data, na.rm=T)
        min_v    <- min(h_data, na.rm=T)
        median_v <- median(h_data, na.rm=T)
        
        #set threshold based on max_v
        threshold <- if(max_v <=0){             0
                        }else if(max_v<=1){     0.5
                        }else if(max_v<=150){   0.5*max_v     #this one is a guess
                        }else{                  125 + quantile(h_data, 0.05, na.rm=TRUE)   
                        }

        pct_open   <- round(sum(h_data>=threshold, na.rm=T)/length(h_data),3)
        pct_na     <- round(sum(is.na(h_data))/length(h_data),3)
        pct_closed <- round(sum(h_data<threshold, na.rm=T)/length(h_data),3)
        if(min_v==max_v & max_v >1){    #for datastreams that represent max, min flows
            pct_open   <- NA
            pct_na     <- NA
            pct_closed <- NA
        }
        
        sash <- rep("no data", length(h_data))
        sash[h_data>=threshold] <- "open"
        sash[h_data<threshold]  <- "closed"
        run_length            <- data.frame(lengths=rle(sash)$lengths, values=rle(sash)$values)
        run_length$cumsum     <- cumsum(run_length$lengths)
        run_length$exceeds_5  <- run_length$values=="open" & run_length$lengths>(5*60/as.numeric(interval))
        run_length$exceeds_8  <- run_length$values=="open" & run_length$lengths>(8*60/as.numeric(interval))
        run_length$exceeds_24 <- run_length$values=="open" & run_length$lengths>(24*60/as.numeric(interval))
        
        total_openings  <- sum(run_length$values=="open", na.rm=T)
        if(total_openings==0){
            med_opening_hrs <- 0
            max_opening_hrs <- 0
            openings_over_5 <- 0
        }else{
            med_opening_hrs <- round(median(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
            max_opening_hrs <- round(max(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
            openings_over_5 <- round(sum(run_length$exceeds_5, na.rm=T))
        }
        
        hood_sash_data <-data.frame(dttm=data_c$dttm, h_data, sash)
        full_summary <- data.frame(report_summary, hood=h, 
                              min_v=min_v, median_v, max_v, threshold,
                              pct_open, pct_na, pct_closed,
                              total_openings, med_opening_hrs, max_opening_hrs, openings_over_5
        )
        plot_fume_hood(hood_sash_data, run_length, full_summary)
        
        return(full_summary)
    })
    hood_summary <- dplyr::rbind_all(hood_summary)
    
})
all_data_summary <- dplyr::rbind_all(all_data_summary)
write.csv(all_data_summary, file=paste0(wd_output,"all_hood_summary.csv"), row.names=F)

#end pdf creation
graphics.off()
closeAllConnections()


