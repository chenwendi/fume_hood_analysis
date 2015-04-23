library(ggplot2)

#creating ggplot figures by calling functions doesn't work so well, so this script does not call a main function
#instead, this is a script to create all the visuals, accessing all variables in the global environment

#check we have the required variables
if(!exists("data_summary_temp")) stop("data_summary_temp required to create visualizations")
if(!exists("wd_output_temp")) stop("wd_output required to create visualizations")


pdf(paste0(wd_output_temp,"explore_variation_within_depts.pdf"), width=8, height=4)

plot_data_availability(data_summary_temp)

#### barplot hood counts by dept, lab
data_summary_temp$group <- paste(data_summary_temp$dept, data_summary_temp$lab)
plot_setup <- ggplot(data_summary_temp) + theme_minimal()

plot_a <- plot_setup + geom_bar(aes(y=..count.., x=group, fill=dept)) + 
    ggtitle("Hood distribution across department/lab") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_a))

plot_a2 <- plot_setup + geom_bar(aes(y=..count.., x=dept)) + 
    ggtitle("Hood distribution across building") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
suppressWarnings(print(plot_a2))

# visualize metrics by department
plot_setup <- ggplot(data_summary_temp, aes(x=dept, fill=dept)) + theme_minimal()

plot_a <- plot_setup + geom_bar(aes(x=log(score))) 
plot_b <- plot_setup + geom_boxplot(aes(y=log(score))) + geom_point(aes(y=log(score)) , position="jitter") +coord_flip()
plot_c <- plot_setup + geom_density(aes(x=log(score)), alpha=0.1) 
suppressWarnings(print(plot_a+ggtitle("Distribution of log-score") + xlab("log(score)")))
suppressWarnings(print(plot_b+ggtitle("Distribution of log-score") + xlab("log(score)")))
suppressWarnings(print(plot_c+ggtitle("Distribution of log-score") + xlab("log(score)")))


variables <- c("total_intervals","pct_na", "pct_open", "pct_closed", "pct_open_over_5",
               "pct_open_under_5", "total_openings", "med_opening_hrs", "openings_over_5",
               "pct_exceeding_5", "score", "ratio_mean_min_flow")
for(var in variables){
    plot_setup <- ggplot(data_summary_temp, aes(x=dept, fill=dept)) + theme_minimal()
    plot_a <- plot_setup + geom_bar(aes(x=eval(parse(text=var)))) 
    plot_b <- plot_setup + geom_boxplot(aes(y=eval(parse(text=var)))) +  geom_point(aes(y=eval(parse(text=var))) , position="jitter")  +coord_flip()
    plot_c <- plot_setup + geom_density(aes(x=eval(parse(text=var))), alpha=0.1)
    suppressWarnings(print(plot_a+ggtitle(paste("Distribution of",var)) + xlab("")))
    suppressWarnings(print(plot_b+ggtitle(paste("Distribution of",var)) + xlab("")))
    suppressWarnings(print(plot_c+ggtitle(paste("Distribution of",var)) + xlab("")))
    rm(plot_a, plot_b, plot_c)
    
    formula <- paste(var,"~ dept")
    gplots::plotmeans(eval(parse(text=formula)), data=data_summary_temp, xlab="Department", main="Mean Plot with 95% CI")
    
}

graphics.off()         
closeAllConnections()
