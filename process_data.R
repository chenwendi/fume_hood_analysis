options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)
library(gplots)

#source all functions
wd_src <- "./src/"
sapply(list.files("./src/",pattern="*.R$", full.names=TRUE,ignore.case=TRUE),source,.GlobalEnv)

# get filenames
wd_data    <- "../data/"                                           #csv data is saved in this folder
wd_output  <- "../output/"   # set directory where all output will be saved

hood_mapping <- read.csv("hood_mapping.csv")      #load data frame containing details about each hood
data_files <- list.files(wd_data,pattern = ".csv")
if(length(data_files)==0) stop("There are not fume hood data files to analyze.")

##### STEP 1: read and format all data
all_formatted_data <- etl(data_files)
saveRDS(all_formatted_data, file = paste0(wd_data,"all_formatted_data.rds"))
saveRDS(data_files, file = paste0(wd_data,"data_files.rds"))
# all_formatted_data <- readRDS(file = paste0(wd_data,"all_formatted_data.rds"))
# data_files <- readRDS(file = paste0(wd_data,"data_files.rds"))

start_dttm <- "2015-02-16"     #set earliest date to include in analysis, anything earlier will be excluded
start_dttm <- as.POSIXct(start_dttm)
all_formatted_data <- lapply(all_formatted_data, FUN=function(d){   d[d$dttm>=start_dttm,]  })

##### STEP 2: Summarize all files, list hoods within them, compress datasets
compressed_data               <- compress_data(data_files, all_formatted_data, hood_mapping, wd_output)

compressed_data_files         <- compressed_data$compressed_data_files
compressed_all_formatted_data <- compressed_data$compressed_all_formatted_data
compressed_file_summary       <- compressed_data$compressed_file_summary


##### STEP 3: Calculate individual hood metrics, graph time series data
compressed_hood_summary <- calculate_hood_metrics(compressed_data_files, compressed_all_formatted_data, 
                                                  compressed_file_summary, wd_output, plot_ts=TRUE)
    
##### STEP 4: Visualization of metrics for all fume hoods by treatment/ bldg/ lab
data_summary_temp <- compressed_hood_summary
wd_output_temp    <- wd_output
source("./visualize_data_summary_by_dept.R")

##### STEP 5: Analyse hood data at the weekly level (and save results)
start_dttm <- "2015-02-16"
weeks <- seq(from=as.POSIXct(start_dttm), to=Sys.time(), by=as.difftime(7, units="days"))
weeks <- weeks[weeks< max(compressed_file_summary$max_date)]
weeks <- weeks[weeks>=min(compressed_file_summary$min_date)]

#summarize and plot every available hood-week combination 
weekly_data_files     <- compressed_data_files
weekly_hood_summary_w <- list()
for(w in 1:(length(weeks)-1)){
    print(w)
    weekly_formatted_data <- reduce_timeframe_hood_data(all_data_summary=compressed_file_summary, 
                                                         all_formatted_data=compressed_all_formatted_data,
                                                         start=weeks[w], end=weeks[w+1])
    weekly_file_summary <- calculate_file_metrics(all_formatted_data=weekly_formatted_data, data_files=weekly_data_files)
    
    weekly_hood_summary <- calculate_hood_metrics(weekly_data_files, weekly_formatted_data, weekly_file_summary, 
                                                   wd_output=paste0(wd_output,"reduced",w,"_"), plot_ts=TRUE)  
    
    weekly_hood_summary_w[[length(weekly_hood_summary_w)+1]] <- data.frame(weekly_hood_summary, wk=w)
}
weekly_hood_summary <- dplyr::rbind_all(weekly_hood_summary_w)
write.csv(weekly_hood_summary, paste0(wd_output,"weekly_hood_summary.csv"))
# weekly_hood_summary <- read.csv(paste0(wd_output,"weekly_hood_summary.csv"))


#### STEP 6: Sampling of fume hoods for analysis, visualization of sampled hoods
groups <- unique(compressed_hood_summary$dept)
sample_size <- 30   #number of hoods per group
sampled_data_summary <- lapply(groups, FUN=function(g){      #this step already done, don't resample
    data_subset   <- compressed_hood_summary %>% filter(dept==g)
    data_sample_g <- dplyr::sample_n(data_subset, size=sample_size, replace = TRUE)
})
sampled_data_summary <- dplyr::rbind_all(sampled_data_summary)
write.csv(sampled_data_summary, paste0(wd_output,"sampled_data_summary.csv"), row.names=FALSE)
# sampled_data_summary <- read.csv(paste0(wd_output,"sampled_data_summary.csv"))

# visualization for sampled data
data_summary_temp <- sampled_data_summary
wd_output_temp    <- paste0(wd_output,"sampled_")
source("./visualize_data_summary_by_dept.R")

# from sampled hoods, get complete weekly data for analysis in paper
sampled_data_summary <- read.csv(paste0(wd_output,"sampled_data_summary.csv"))
sampled_data_summary$sample <- 1:nrow(sampled_data_summary)
sampled_data_analysis <- weekly_hood_summary %>% 
  filter(hood %in% sampled_data_summary$hood) %>%
  filter(days>6) %>%
  select(-file)
sampled_data_analysis<-dplyr::left_join(sampled_data_summary[,c("hood","sample")],sampled_data_analysis,by="hood")

# for each fume hood, sample 1 complete week
samples <- unique(sampled_data_analysis$sample)
sample_data <- lapply(samples, FUN=function(s){
  s_data <- sampled_data_analysis %>% filter(sample==s)
  s_datarow <- dplyr::sample_n(s_data, size = 1)
})
sample_data <- dplyr::rbind_all(sample_data)
write.csv(sample_data, paste0(wd_output,"sample_data.csv"))
# sample_data <- read.csv(paste0(wd_output,"sample_data.csv"))



##### STEP 7: Other analysis (not in paper or printed to pdf)

#do the same hoods perform poorly week to week?
week_comparison <- sampled_data_analysis       #this dataframe has all weeks for the sampled hoods
week_comparison <- dplyr::left_join(week_comparison, hood_mapping, by="hood")
week_comparison <- week_comparison %>%
  filter(dept=="fairchild")
week_comparison <- dplyr::left_join(week_comparison, hood_mapping, by="hood")
ggplot(week_comparison) + theme_minimal()+
  geom_point(aes(x=wk, y=pct_exceeding_5, color=hood)) + 
  geom_line(aes(x=wk, y=pct_exceeding_5, color=hood))


