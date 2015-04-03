options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)

#source all functions
wd_src <- "./src/"
sapply(list.files("./src/",pattern="*.R$", full.names=TRUE,ignore.case=TRUE),source,.GlobalEnv)

# get filenames
wd_data    <- "../data/"                                           #csv data is saved in this folder
data_files <- list.files(wd_data,pattern = ".csv")
if(length(data_files)==0) stop("There are not fume hood data files to analyze.")

# set directory where all output will be saved
wd_output <- "../output/"

hood_mapping <- read.csv("hood_mapping.csv")

##### STEP 1: read and format all data
all_formatted_data <- list()
for(file in 1:length(data_files)){
    tryCatch({
        print(file)
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

#### STEP 2: Summarize all files, list hoods within them, combine data by hood, resummarize data
all_file_summary <- calculate_file_metrics(all_formatted_data, data_files)

#map to hood data and exclude nonrelevant hoods
all_file_summary <- dplyr::left_join(all_file_summary,hood_mapping, by="hood")
all_file_summary <- all_file_summary[!is.na(all_file_summary$exclude) & all_file_summary$exclude==FALSE,]
select_hoods_formatted_data <- reduce_unnecessary_hood_data(all_file_summary, all_formatted_data)
write.csv(all_file_summary, file=paste0(wd_output,"initial_file_summary.csv"), row.names=F)

# plot data availability
plot_data_availability(all_data_summary=all_file_summary)

# compare files with ovelapping data for consistency and combine data for hoods
results                     <- check_merge_hood_data(all_file_summary, all_formatted_data=select_hoods_formatted_data, data_files) 
combined_data_files         <- results$data_files_new   #old object: data_files
combined_all_formatted_data <- results$all_data_new     #old object: all_formatted_data

all_file_summary <- calculate_file_metrics(combined_all_formatted_data, combined_data_files)   #recalc for combined files
all_file_summary <- dplyr::left_join(all_file_summary,hood_mapping, by="hood")
all_file_summary <- all_file_summary[all_file_summary$exclude==FALSE,]

if(!all(combined_data_files %in% unique(all_file_summary$file))) 
    stop(paste("no relevant data in file", unique(combined_data_files[!combined_data_files %in% unique(all_file_summary$file)])))

plot_data_availability(all_file_summary)

##### check: confirm all hood names are listed in hood_mapping
all_hood_names <- all_file_summary$hood
unmapped_hoods <- all_hood_names[!all_hood_names %in% hood_mapping$hood]
if(length(unmapped_hoods)>0) stop("there are unmapped hoods")

##### STEP 3: Perform individual hood calculations and graph time series data
#begin pdf creation for all time series graphs
pdf(paste0(wd_output,"fume_hood_time_series.pdf"), width=12, height=4)

#calculate metrics and save to file
all_hood_summary <- calculate_hood_metrics(all_formatted_data=combined_all_formatted_data, data_files=combined_data_files,
                                           file_summary=all_file_summary, plot_ts=T)
#combine with file summary data and save to file
all_hood_summary <- dplyr::left_join(all_file_summary, all_hood_summary, by="hood")
write.csv(all_hood_summary, file=paste0(wd_output,"all_hood_summary.csv"), row.names=FALSE)

# end pdf creation
graphics.off()         
closeAllConnections()

##### STEP 4: Comparison of data in plots
#################### need to pick a common 2 week time frame ##########################
pdf(paste0(wd_output,"dept_comparisons.pdf"), width=12, height=4)

relevant_data <- all_hood_summary %>%
    filter(days>=7) 
relevant_data$group <- paste(relevant_data$dept, relevant_data$lab)
duplicates <- relevant_data %>% group_by(hood) %>% dplyr::summarize(count=n()) %>% filter(count>1)
if(nrow(duplicates)>0) warning("duplicated hood data has not been combined")

# plot data availability
plot_data_availability(relevant_data)

# comparisons of fume hoods in different groups (labs, rooms, etc...)
plot_setup <- ggplot(relevant_data) + theme_minimal()
plot_a <- plot_setup + geom_bar(aes(y=..count.., x=group, fill=dept)) + 
    ggtitle("Hood distribution across building/lab") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_a))

plot_a2 <- plot_setup + geom_bar(aes(y=..count.., x=dept)) + 
    ggtitle("Hood distribution across department/building") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_a2))

plot_b <- plot_setup + geom_boxplot(aes(x=group, y= pct_open_under_5)) + 
    geom_point(aes(x=group, y= pct_open_under_5), position="jitter", color="red") + 
    ggtitle("Boxplot of percent time open under 5 hours") + 
    coord_cartesian(ylim=c(0, unname(quantile(relevant_data$pct_open_under_5,0.99, na.rm=T)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_b))

plot_c <- plot_setup + geom_boxplot(aes(x=group, y= pct_open_over_5)) + 
    geom_point(aes(x=group, y= pct_open_over_5), position="jitter", color="red") + 
    ggtitle("Boxplot of pct time open over 5 hours") + 
    coord_cartesian(ylim=c(0, unname(quantile(relevant_data$pct_open_over_5,0.99, na.rm=T)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_c))

plot_d <- plot_setup + geom_boxplot(aes(x=group, y= pct_exceeding_5)) + 
    geom_point(aes(x=group, y= pct_exceeding_5), position="jitter", color="red") + 
    ggtitle("Boxplot of total hours open beyond threshold of 5 hours") + 
    coord_cartesian(ylim=c(0, unname(quantile(relevant_data$pct_exceeding_5,0.99, na.rm=T)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_d))

plot_setup + geom_point(aes(x=total_openings, y= pct_exceeding_5, color=dept)) + 
    ggtitle("Number of total openings vs number of hours exceeding 5 hour threshold") +
    coord_cartesian(xlim=c(0, unname(quantile(relevant_data$total_openings,0.99, na.rm=T))),
                    ylim=c(0, unname(quantile(relevant_data$pct_exceeding_5,0.99, na.rm=T))))

#explore score
plot_setup + geom_bar(aes(x=score)) + ggtitle("Distribution of score for all hoods in all labs")
plot_setup + geom_bar(aes(x=score, fill=dept)) + facet_wrap(~dept, ncol=4)
plot_setup + geom_density(aes(x=score, fill=dept), alpha=0.1) + ggtitle("Score, by department")

#explore pct_exceeding_5
plot_setup + geom_bar(aes(x=pct_exceeding_5)) + ggtitle("Distribution of pct_exceeding_5 for all hoods in all labs")
plot_setup + geom_bar(aes(x=pct_exceeding_5, fill=dept)) + facet_wrap(~dept, ncol=4)
plot_setup + geom_density(aes(x=pct_exceeding_5, fill=dept), alpha=0.1) + ggtitle("pct_exceeding_5, by department")

#explore pct_open_over_5
plot_setup + geom_bar(aes(x=pct_open_over_5)) + ggtitle("Distribution of pct_open_over_5 for all hoods in all labs")
plot_setup + geom_bar(aes(x=pct_open_over_5, fill=dept)) + facet_wrap(~dept, ncol=4)
plot_setup + geom_density(aes(x=pct_open_over_5, fill=dept), alpha=0.1) + ggtitle("pct_open_over_5, by department")

graphics.off()
closeAllConnections()

#summarize groups
groups <- unique(relevant_data$dept)
var <- c("pct_open_over_5", "pct_exceeding_5", "score")
group_summary <- sapply(groups, FUN=function(g){
    var <- relevant_data[,c(var[2])]
    s <- sd(var[relevant_data$dept==g], na.rm=T)
    n <- length(!is.na(var[relevant_data$dept==g]))
    t <- t.test(var[relevant_data$dept==g])
    return(data.frame(mean=t$estimate, sd=s, n=n, conf_int1=t$conf.int[1], conf_int2=t$conf.int[2]))
})
print(group_summary)

#two sample t-test for equal means
variable     <- "score"
table(relevant_data[,"dept"])
experimental <- relevant_data[relevant_data$dept=="chem",variable]
control      <- relevant_data[relevant_data$dept %in% c("bio","esl"),variable]
t_results    <- t.test(experimental, control, alternative="less") 
conf_int     <- t.test(experimental)


#other questions
# what is data availability by file of hoods
