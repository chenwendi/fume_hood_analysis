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

hood_mapping <- read.csv("hood_mapping.csv")

##### read and format all data
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

##### get all available hood names, confirm they are listed in hood_mapping
all_hood_names <- sapply(1:length(all_formatted_data), FUN=function(c){ 
    fume_hood_names <- setdiff(names(all_formatted_data[[c]]),"dttm") 
})
all_hood_names <- unlist(all_hood_names)
unmapped_hoods <- all_hood_names[!all_hood_names %in% hood_mapping$hood]
if(length(unmapped_hoods)>0) stop("there are unmapped hoods")

#####  begin pdf creation for all time series graphs
pdf(paste0(wd_output,"fume_hood_time_series.pdf"), width=12, height=4)

#calculate metrics and save to file
hoods_to_exclude <- hood_mapping$hood[hood_mapping$exclude==TRUE]
all_data_summary <- calculate_hood_metrics(all_formatted_data, hoods_to_exclude, plot_ts=T)
write.csv(all_data_summary, file=paste0(wd_output,"all_hood_summary.csv"), row.names=F)

# end pdf creation
graphics.off()         
closeAllConnections()

#assign labs and department names to hoods
all_data_summary <- dplyr::inner_join(all_data_summary,hood_mapping, by="hood")

# plot data availability
plot_data_availability(all_data_summary)

# compare 2 files with ovelapping data for consistency (and combine?)
check_overlapping_hood_data(all_data_summary)    #combine functionality not yet added

##### comparison of data in plots
pdf(paste0(wd_output,"dept_comparisons.pdf"), width=12, height=4)

relevant_data <- all_data_summary %>%
    filter(days>6) 
relevant_data$group <- paste(relevant_data$dept, relevant_data$lab)
duplicates <- relevant_data %>% group_by(hood) %>% dplyr::summarize(count=n()) %>% filter(count>1)

# plot data availability
plot_data_availability(relevant_data)

# comparisons of fume hoods in different groups (labs, rooms, etc...)
plot_setup <- ggplot(relevant_data) + theme_minimal()
plot_setup + geom_bar(aes(y=..count.., x=group)) + 
    ggtitle("Hood distribution across buildings") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_setup + geom_boxplot(aes(x=group, y= pct_open_under_5)) + 
    geom_point(aes(x=group, y= pct_open_under_5), position="jitter", color="red") + 
    ggtitle("Boxplot of percent time open under 5 hours") + 
    coord_cartesian(ylim=c(0, unname(quantile(relevant_data$pct_open_under_5,0.99, na.rm=T)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_setup + geom_boxplot(aes(x=group, y= pct_open_over_5)) + 
    geom_point(aes(x=group, y= pct_open_over_5), position="jitter", color="red") + 
    ggtitle("Boxplot of pct time open over 5 hours") + 
    coord_cartesian(ylim=c(0, unname(quantile(relevant_data$pct_open_over_5,0.99, na.rm=T)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot_setup + geom_point(aes(x=pct_open_under_5, y= pct_open_over_5, color=dept)) + 
#     ggtitle("Pct time open over 5 hours vs under 5 hours") +
#     coord_cartesian(xlim=c(0, unname(quantile(relevant_data$pct_open_under_5,0.99, na.rm=T))),
#                     ylim=c(0, unname(quantile(relevant_data$pct_open_over_5,0.99, na.rm=T))))

#explore score
plot_setup + geom_bar(aes(x=score)) + ggtitle("Distribution of score for all hoods in all labs")
plot_setup + geom_bar(aes(x=score, fill=dept)) + facet_wrap(~dept, ncol=4)
plot_setup + geom_density(aes(x=score, fill=dept), alpha=0.1) + ggtitle("Score, by department")

#explore pct_open_over_5
plot_setup + geom_bar(aes(x=pct_open_over_5)) + ggtitle("Distribution of pct_open_over_5 for all hoods in all labs")
plot_setup + geom_bar(aes(x=pct_open_over_5, fill=dept)) + facet_wrap(~dept, ncol=4)
plot_setup + geom_density(aes(x=pct_open_over_5, fill=dept), alpha=0.1) + ggtitle("pct_open_over_5, by department")

graphics.off()
closeAllConnections()

#summarize groups
groups <- unique(relevant_data$dept)
group_summary <- sapply(groups, FUN=function(g){
    var <- relevant_data$pct_open_over_5
    s <- sd(var[relevant_data$dept==g], na.rm=T)
    n <- length(!is.na(var[relevant_data$dept==g]))
    t <- t.test(var[relevant_data$dept==g])
    return(data.frame(mean=t$estimate, sd=s, n=n, conf_int1=t$conf.int[1], conf_int2=t$conf.int[2]))
})

#two sample t-test for equal means
variable     <- "score"
table(relevant_data[,"dept"])
experimental <- relevant_data[relevant_data$dept=="chem",variable]
control      <- relevant_data[relevant_data$dept %in% c("bio","esl"),variable]
t_results    <- t.test(experimental, control, alternative="less") 
conf_int     <- t.test(experimental)
