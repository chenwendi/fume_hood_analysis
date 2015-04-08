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

##### STEP 2: Summarize all files, list hoods within them, compress datasets
compressed_data               <- compress_data(data_files, all_formatted_data, hood_mapping, wd_output)

compressed_data_files         <- compressed_data$compressed_data_files
compressed_all_formatted_data <- compressed_data$compressed_all_formatted_data
compressed_file_summary       <- compressed_data$compressed_file_summary

##### STEP 3: Calculate individual hood metrics, graph time series data
compressed_hood_summary <- calculate_hood_metrics(compressed_data_files, compressed_all_formatted_data, 
                                                  compressed_file_summary, wd_output)
    
##### STEP 4: Visualization of metrics for all fume hoods by treatment/ bldg/ lab
data_summary_temp <- compressed_hood_summary
wd_output_temp    <- wd_output
source("./visualize_data_summary_by_dept.R")

##### STEP 5: Analyse hood data at the weekly level (and save results)
weeks <- seq(from=as.POSIXct("2015-01-04"), to=Sys.time(), by=as.difftime(7, units="days"))
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
    weekly_file_summary <- calculate_file_metrics(all_formatted_data=reduced_formatted_data, data_files=reduced_data_files)
    
    weekly_hood_summary <- calculate_hood_metrics(reduced_data_files, reduced_formatted_data, reduced_file_summary, 
                                                   wd_output=paste0(wd_output,"reduced",w,"_"))  
    
    weekly_hood_summary_w[[length(weekly_hood_summary_w)+1]] <- data.frame(reduced_hood_summary, wk=w)
}
weekly_hood_summary <- dplyr::rbind_all(weekly_hood_summary_w)
write.csv(weekly_hood_summary, paste0(wd_output,"weekly_hood_summary.csv"))


#### STEP 6: Sampling of fume hoods for analysis, visualization of sampled hoods
groups <- unique(compressed_hood_summary$dept)
sample_size <- 20   #number of hoods per group
# sampled_data_summary <- lapply(groups, FUN=function(g){      #this step already done, don't resample
#     data_subset   <- compressed_hood_summary %>% filter(dept==g)
#     data_sample_g <- dplyr::sample_n(data_subset, size=sample_size, replace = TRUE)
# })
# sampled_data_summary <- dplyr::rbind_all(sampled_data_summary)
# write.csv(sampled_data_summary[,c("hood", "dept")], paste0(wd_output,"sampled_data_summary.csv"), row.names=FALSE)

#visualization for sampled data
data_summary_temp <- sampled_data_summary
wd_output_temp    <- paste0(wd_output,"sampled_")
source("./visualize_data_summary_by_dept.R")

#get complete weekly data for sample hoods
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

sample_data <- dplyr::left_join(sample_data, hood_mapping, by="hood")
sample_data$new_lab_name <- sapply(sample_data$lab, FUN=function(n){lab_mapping$lab_name[which(lab_mapping$lab==n)]})

cfm_rates <- sample_data %>% filter(max_v>1)
fit <- lm(data=cfm_rates, mean_v~pct_open)
new_data <-data.frame(pct_open=c(0.085, 0.2,0.475))
predict(fit, new_data)

##### STEP 5: Analyse metrics for significant differences 

#choose data set, variable for analysis
data_to_analyse <- compressed_hood_summary %>% filter(days>=14) 
value <- c("pct_open_over_5", "pct_exceeding_5", "score")

#get group stats, 95% confidence interval
group_summary <- summarize_groups(data=data_to_analyse, value="pct_open_over_5", grouping_var="dept")
print(group_summary)

#compare each pair of groupds for significant differences
fit <- aov(pct_open_over_5 ~ dept, data=data_to_analyse)
TukeyHSD(fit)

#two sample t-test for equal means
variable     <- "pct_open_over_5"
table(data_to_analyse[,"dept"])
experimental <- data_to_analyse[data_to_analyse$dept=="chem",variable]
control      <- data_to_analyse[data_to_analyse$dept %in% c("bio","esl"),variable]
t_results    <- t.test(experimental, control, alternative="less") 



#other questions
# what is data availability by file of hoods- try to limit by timeframe or perform 1-on-1 comparisons to CCB over similar time periods
# are there differences between weeks in the same lab/ department?
# if SF is automatic closure, can we compare under 5 and exceeding 5? is 5 hours an accurate threshold?



