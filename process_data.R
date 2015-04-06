options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)

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

##### STEP 2: Summarize all files, list hoods within them, compress datasets
compressed_data               <- compress_data(data_files, all_formatted_data, hood_mapping, wd_output)

compressed_data_files         <- compressed_data$compressed_data_files
compressed_all_formatted_data <- compressed_data$compressed_all_formatted_data
compressed_file_summary       <- compressed_data$compressed_file_summary

##### STEP 3: Calculate individual hood metrics, graph time series data
compressed_hood_summary <- calc_plot_hood_metrics(compressed_data_files, compressed_all_formatted_data, compressed_file_summary, wd_output)
    
##### STEP 4: Visualization of metrics for all fume hoods by treatment/ bldg/ lab
data_summary_temp <- compressed_hood_summary
wd_output_temp    <- wd_output
source("./visualize_data_summary_by_dept.R")

#### STEP 5: Sampling of fume hoods for analysis, visualization of sampled hoods
groups <- unique(compressed_hood_summary$dept)
sample_size <- 20   #number of hoods per group
sampled_data_summary <- lapply(groups, FUN=function(g){
    data_subset   <- compressed_hood_summary %>% filter(dept==g)
    data_sample_g <- dplyr::sample_n(data_subset, size=sample_size, replace = TRUE)
})
sampled_data_summary <- dplyr::rbind_all(sampled_data_summary)

data_summary_temp <- sampled_data_summary
wd_output_temp    <- paste0(wd_output,"sampled_")
source("./visualize_data_summary_by_dept.R")

##### STEP 6: Analyse hood data for differences from week to week
### try variations on the complete data set 
#focus on single week/ timeframe
weeks <- seq(from=as.POSIXct("2015-01-04"), to=Sys.time(), by=as.difftime(7, units="days"))
weeks <- weeks[weeks< max(compressed_file_summary$max_date)]
weeks <- weeks[weeks>=min(compressed_file_summary$min_date)]

reduced_data_files     <- compressed_data_files
reduced_hood_summary_w <- list()
for(w in 1:(length(weeks)-1)){
    print(w)
    reduced_formatted_data <- reduce_timeframe_hood_data(all_data_summary=compressed_file_summary, 
                                                         all_formatted_data=compressed_all_formatted_data,
                                                         start=weeks[w], end=weeks[w+1])
    reduced_file_summary <- calculate_file_metrics(all_formatted_data=reduced_formatted_data, data_files=reduced_data_files)
    
    reduced_hood_summary <- calc_plot_hood_metrics(reduced_data_files, reduced_formatted_data, reduced_file_summary, 
                                                   wd_output=paste0(wd_output,"reduced",w,"_"))  
    
    reduced_hood_summary_w[[length(reduced_hood_summary_w)+1]] <- data.frame(reduced_hood_summary, wk=w)
}
reduced_hood_summary <- dplyr::rbind_all(reduced_hood_summary_w)

shared_hoods <- reduced_hood_summary$hood
shared_hoods <- names(table(shared_hoods)[table(shared_hoods)>3])

analysis_data <- reduced_hood_summary_w_temp %>% filter(hood %in% shared_hoods)
ggplot(analysis_data) + geom_line(aes(x=min_date,y=score, group=hood)) + ylim(0,50)
group_summary <- summarize_groups(data=analysis_data, value="score", grouping_var="min_date")

analysis_data$min_date <- as.factor(analysis_data$min_date)
fit <- aov(score ~ min_date, data=analysis_data)
TukeyHSD(fit)

gplots::plotmeans(pct_exceeding_5 ~ min_date, data=analysis_data, xlab="Week", main="Mean Plot with 95% CI")


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



