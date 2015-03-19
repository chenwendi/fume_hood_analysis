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

# read and format all data
all_formatted_data <- list()
for(file in 1:length(data_files)){
    tryCatch({
    original_data <- readLines(paste0(wd_data, data_files[file])) #read lines from csv file
    original_data <- gsub("\"","",original_data)                  #get rid of random quotation marks
    
    ## determine file formatting
    file_format <- check_format(original_data)    #check formatting of file
    
    ## choose formatting function based on current file formatting
    if(file_format$type=="wide_date format"){          formatted_data <- format_wide_data(original_data, file_format)
    }else if(file_format$type=="long_date format"){    formatted_data <- format_long_data(original_data, file_format)
    }else{                                        warning(paste(data_files[file],": data not in recognizable format"))
    }    
       
    all_formatted_data[[file]]<- formatted_data
    },error=function(e){ print(paste(file, data_files[file], e))
    })
}

##### remove data for hoods that are not relevant
# get all available hood names
all_hood_names <- sapply(1:length(all_formatted_data), FUN=function(c){ 
    fume_hood_names <- setdiff(names(all_formatted_data[[c]]),"dttm") 
})
all_hood_names <- unlist(all_hood_names)

#names of hoods to exclude because they're not relevant or they represent aggregated hoods
hood_endings_to_exclude <- c("TOTAL MAX$", "TOTAL MIN$", 
                             "TOTAL.HOOD.FLOW$", "TOT.HOOD.FLOW$", "TOT.HOODS.OPEN$", #"TOT.FLOW$" ,
                             "FLOW STPT$",
                             "EXHAUST", "^Var",
                             
                             # labs with multiple hoods
                             "CHEM.BALSKUS","CHEM.BETLEY","CHEM.COREY","CHEM.GORDON", "CHEM.JACOBSEN", "CHEM.KAHN","CHEM.KISHI",
                             "CHEM.LIU", "CHEM.MYERS","CHEM.NOCERA", "CHEM.PARK","CHEM.RITTER", "CHEM.SHAIR","CHEM.WHITESIDES" , "CHEM.XIE",
                             "MCKAY.WEITZ" , "NW.TEACHLABS"                        
)
hoods_to_exclude <- sapply(1:length(hood_endings_to_exclude), FUN=function(c){
    return(all_hood_names[grepl(hood_endings_to_exclude[c], all_hood_names)])
})
hoods_to_exclude <- unlist(hoods_to_exclude)

# begin pdf creation for all graphs
pdf(paste0(wd_output,"sash_data.pdf"), width=12, height=4)

# begin to loop through each set of fume hood data (each set corresponds to a csv file)
all_data_summary <- lapply(1:length(all_formatted_data), FUN=function(c){
    print(c)
    ##### basic calculations, summary of the data included in a given file
    data_c <- all_formatted_data[[c]]
    min_date <- min(data_c$dttm)
    max_date <- max(data_c$dttm)
    range    <- max_date-min_date
    
    intervals <- data_c$dttm[1:10]- lag(data_c$dttm[1:10],1)
    interval <- median(intervals, na.rm=T)
    
    file_summary <- data.frame(file=data_files[c], min_date, max_date, days=round(range,2), interval)
    
    #### summary of individual fume hoods
    fume_hood_names <- setdiff(names(data_c),"dttm")
    hood_summary <- lapply(fume_hood_names, FUN=function(h){
        if(h %in% hoods_to_exclude)    return(NULL)     #don't evaluate non-relevant hoods
        print(h)
        c_index  <- which(names(data_c)==h)
        h_data   <- data.frame(dttm=data_c[,"dttm"], hood=data_c[,c_index])
        max_v    <- max(h_data$hood, na.rm=T)
        min_v    <- min(h_data$hood, na.rm=T)
        median_v <- median(h_data$hood, na.rm=T)
        mean_v   <- round(mean(h_data$hood, na.rm=T),3)
                
        #set threshold based on max_v
        threshold <- if(max_v <=0){             0
                        }else if(max_v<=1){     0.5
                        }else if(max_v<=150){   0.5*max_v     #this one is a guess
                        }else{                  125 + quantile(h_data$hood, 0.05, na.rm=TRUE)   
                        }
        if(threshold>max_v){ threshold <- max_v}

        h_data$wday <- strftime(h_data$dttm,'%u') %in% c(1,2,3,4,5) #determine weekday or weekend
        pct_open_wd <- round(sum(h_data$hood[h_data$wday]>=threshold, na.rm=T)/sum(h_data$wday),3)
        pct_open_we <- round(sum(h_data$hood[!h_data$wday]>=threshold, na.rm=T)/sum(!h_data$wday),3)
        
        pct_open   <- round(sum(h_data$hood>=threshold, na.rm=T)/nrow(h_data),3)
        pct_na     <- round(sum(is.na(h_data$hood))/nrow(h_data),3)
        pct_closed <- round(sum(h_data$hood<threshold, na.rm=T)/nrow(h_data),3)
        
        h_data$sash <- "no data"
        h_data$sash[h_data$hood>=threshold] <- "open"
        h_data$sash[h_data$hood<threshold]  <- "closed"
        run_length            <- data.frame(lengths=rle(h_data$sash)$lengths, values=rle(h_data$sash)$values)
        run_length$cumsum     <- cumsum(run_length$lengths)
        run_length$exceeds_5  <- run_length$values=="open" & run_length$lengths>(5*60/as.numeric(interval))
        run_length$exceeds_8  <- run_length$values=="open" & run_length$lengths>(8*60/as.numeric(interval))
        run_length$exceeds_24 <- run_length$values=="open" & run_length$lengths>(24*60/as.numeric(interval))
        
        total_openings  <- sum(run_length$values=="open", na.rm=T)
        if(total_openings==0){
            med_opening_hrs <- 0
            max_opening_hrs <- 0
            openings_over_5 <- 0
            openings_over_8 <- 0
            openings_over_24 <- 0
        }else{
            med_opening_hrs <- round(median(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
            max_opening_hrs <- round(max(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
            openings_over_5 <- round(sum(run_length$exceeds_5, na.rm=T))
            openings_over_8 <- round(sum(run_length$exceeds_8, na.rm=T))
            openings_over_24 <- round(sum(run_length$exceeds_24, na.rm=T))
        }
        
        full_summary <- data.frame(file_summary, hood=h, 
                              min_v=min_v, median_v, mean_v, max_v, threshold,
                              pct_open_wd, pct_open_we, pct_open, pct_na, pct_closed,
                              total_openings, med_opening_hrs, max_opening_hrs, 
                              openings_over_5, openings_over_8, openings_over_24
        )
        plot_fume_hood(hood_sash_data=h_data, run_length, full_summary)
        
        return(full_summary)
    })
    hood_summary <- dplyr::rbind_all(hood_summary)
    
})
all_data_summary <- dplyr::rbind_all(all_data_summary)

all_data_summary <- all_data_summary %>%
    mutate(score=openings_over_5*5 + openings_over_8*5 + openings_over_24*90) %>%
    mutate(ratio_mean_min_flow= round(mean_v/min_v,3))

all_data_summary$ratio_mean_min_flow[all_data_summary$min_v %in% c(0,1)] <- 0
write.csv(all_data_summary, file=paste0(wd_output,"all_hood_summary.csv"), row.names=F)

# end pdf creation
graphics.off()
closeAllConnections()

# plot data availability
plot_data_availability(all_data_summary)

#compare 2 files with ovelapping data for consistency (and combine?)
check_overlapping_hood_data(all_data_summary)

# need to set up grouping variables
grouping <- data.frame(file =data_files)
building <- sapply(grouping$file, FUN=function(f){
    if(grepl("esl",tolower(f))) return("ESL")
    if(grepl("ccb",tolower(f))) return("CCB")
    if(grepl("sherman|sf",tolower(f))) return("SF")
    if(grepl("bio",tolower(f))) return("BIO")
})
grouping$building <- unname(building)
all_data_summary <- dplyr::inner_join(all_data_summary,grouping)

# is there any correlation between hoods within a group (here calculated as data provided in same file)
correlations_hoods <- lapply(1:length(all_formatted_data), FUN=function(c){
    data_c <- all_formatted_data[[c]]
    fume_hood_names <- setdiff(names(data_c),"dttm")
    fume_hood_names <- fume_hood_names[!(fume_hood_names %in% hoods_to_exclude)]
    cor_list <- lapply(1:(length(fume_hood_names)-1), FUN=function(h1){
        cor_list_sub <- lapply((h1+1):length(fume_hood_names), FUN=function(h2){
            data_1 <- data_c[,fume_hood_names[h1]]
            data_2 <- data_c[,fume_hood_names[h2]]
            correlation <- cor(data_1, data_2, use="pairwise.complete.obs")
            return(data.frame(h1=fume_hood_names[h1], h2=fume_hood_names[h2], correlation))
        })
        cor_list_sub <- dplyr::rbind_all(cor_list_sub)
    })
    cor_list <- dplyr::rbind_all(cor_list)
})
correlations_hoods <- dplyr::rbind_all(correlations_hoods)
correlations_hoods$correlation <- correlations_hoods$correlation[!is.na(correlations_hoods$correlation)]
t.test(correlations_hoods$correlation)    
#mean is not equal to zero => there is correlation between hoods in the same lab
    
#paired t-test between weeks (can we make the assumption that different weeks are the comparable?)
week_differences <- lapply(1:length(all_formatted_data), FUN=function(c){
    data_c <- all_formatted_data[[c]]
    data_c$wk <- as.numeric(strftime(data_c$dttm, "%V"))
    fume_hood_names <- setdiff(names(data_c),c("dttm", "wk"))
    fume_hood_names <- fume_hood_names[!(fume_hood_names %in% hoods_to_exclude)]
    wks <- unique(data_c$wk)[order(unique(data_c$wk))]
    data_by_wk <- list()
    for(w in 1:length(wks)){
        data_by_wk[[w]] <- as.vector(as.matrix(data_c[data_c$wk==wks[w],fume_hood_names]))
    }
    w1_list <- lapply(1:(length(wks)), FUN=function(w1){
        other_wks <- setdiff(1:length(wks),w1)
        other_wks_data <- c()
        for(w2 in other_wks){
            other_wks_data <- c(other_wks_data, data_by_wk[[w2]])
        }
        t_results <- t.test(data_by_wk[[w1]],other_wks_data)
        return(data.frame(w1=wks[w1], p=t_results$p.value, estimate_1=t_results$estimate[1], estimate_2=t_results$estimate[2],
                          ci_1=t_results$conf.int[1],  ci_2=t_results$conf.int[2]))
    })
    w1_list <- dplyr::rbind_all(w1_list)
    return(print(data.frame(file=c,w1_list)))
})
week_differences <- dplyr::rbind_all(week_differences)
week_differences$sig <- week_differences$p<0.01
sum(week_differences$sig)/length(week_differences$sig)
table(week_differences$w1, week_differences$sig)    #what are the weekly differences across labs/buildings
table(week_differences$file, week_differences$sig)  #are there weekly differences by labs/buildings

#####comparison of data
files_to_compare <- c("BIO_2.csv", "CCB_1.csv", "CCB_shair2.csv", "ESL_1.csv", "ESL_2.csv", "ESL_3.csv", "SF_1.csv")
relevant_data <- all_data_summary[all_data_summary$file %in% files_to_compare,]
# comparisons of fume hoods in different groups (labs, rooms, etc...)
plot_setup <- ggplot(relevant_data) + theme_minimal()
plot_setup + geom_bar(aes(y=..count.., x=building)) + ggtitle("Hood distribution across buildings")   

plot_setup + geom_boxplot(aes(x=building, y= total_openings)) + geom_point(aes(x=building, y= openings_over_5), position="jitter", color="red") + 
    ggtitle("Boxplot of total openings, \ncompared to openings over 5 hours") + coord_cartesian(ylim=c(0, unname(quantile(relevant_data$total_openings,0.99)))) 
plot_setup + geom_point(aes(x=pct_open, y= total_openings, color=building)) + ggtitle("Total openings vs Percent of time open")

plot_setup + geom_density(aes(x=pct_open, fill=building), alpha=0.1)   + ggtitle("Percent of time open, by building")
plot_setup + geom_density(aes(x=pct_open_we, fill=building), alpha=0.1) + ggtitle("Percent of time open on weekends, by building")

ggplot(relevant_data[relevant_data$ratio_mean_min_flow>0,]) + theme_minimal() + geom_density(aes(x=ratio_mean_min_flow)) + 
    ggtitle("Ratio of mean flow to minimum flow (for sites with data available)")

#two sample t-test for equal means
variable     <- "pct_open"
experimental <- relevant_data[relevant_data$building=="CCB",variable]
control      <- relevant_data[!relevant_data$building=="CCB",variable]
t_results    <- t.test(experimental, control, alternative="two.sided") 

