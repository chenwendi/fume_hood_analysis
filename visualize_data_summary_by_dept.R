library(ggplot2)

#creating ggplot figures by calling functions doesn't work so well, so this script does not call a main function
#instead, this is a script to create all the visuals, accessing all variables in the global environment

#check we have the required variables
if(!exists("data_summary_temp"))        stop("data_summary_temp required to create visualizations")
if(!exists("wd_output_temp"))           stop("wd_output required to create visualizations")
if(!is.data.frame(data_summary_temp))   stop("data_summary_temp is not data.frame")
if(nrow(data_summary_temp)==0)          stop("data_summary_temp has no data")
if(!"dept" %in% names(data_summary_temp)) stop("data_summary_temp missing 'dept' field")
if(!"lab" %in% names(data_summary_temp))  stop("data_summary_temp missing 'lab' field")

pdf(paste0(wd_output_temp,"explore_variation_within_depts.pdf"), width=8, height=4)

if(!"file" %in% names(data_summary_temp)){
  files_data_summary_temp <- unique(data_summary_temp %>% select(dept, min_date, max_date))
  files_data_summary_temp$file <- 1:nrow(files_data_summary_temp)
  data_summary_temp <- inner_join(data_summary_temp,files_data_summary_temp)
  data_summary_temp$min_date <- as.POSIXct(data_summary_temp$min_date)
  data_summary_temp$max_date <- as.POSIXct(data_summary_temp$max_date)
}
plot_data_availability(data_summary_temp)

#position for labels based on largest number of hoods in a dept
y_label_pos <- max(data_summary_temp %>% group_by(dept) %>% dplyr::summarize(count=n()) %>% select(count), na.rm=T)

#### barplot hood counts by dept, lab
data_summary_temp$group <- paste(data_summary_temp$dept, data_summary_temp$lab)
plot_setup <- ggplot(data_summary_temp) + theme_minimal()

plot_a <- plot_setup + geom_bar(aes(y=..count.., x=group, fill=dept)) + 
    ggtitle("Hood distribution across department/lab") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
suppressWarnings(print(plot_a))

summary <- data_summary_temp %>% group_by(dept) %>% dplyr::summarize(count=n())
plot_a2 <- plot_setup + geom_bar(aes(y=..count.., x=dept)) + 
    ggtitle("Hood distribution across building") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("count:",count)))  #add labels
suppressWarnings(print(plot_a2))

### original visualization of metrics by department
# variables <- c("total_intervals","pct_na", "pct_open", "pct_closed", "pct_open_over_5",
#                "pct_open_under_5", "total_openings", "med_opening_hrs", "openings_over_5",
#                "pct_exceeding_5", "score", "ratio_mean_min_flow")
# for(var in variables){
#     plot_setup <- ggplot(data_summary_temp, aes(x=dept, fill=dept)) + theme_minimal()
#     plot_a <- plot_setup + geom_bar(aes(x=eval(parse(text=var)))) 
#     plot_b <- plot_setup + geom_boxplot(aes(y=eval(parse(text=var)))) +  geom_point(aes(y=eval(parse(text=var))) , position="jitter")  +coord_flip()
#     plot_c <- plot_setup + geom_density(aes(x=eval(parse(text=var))), alpha=0.1)
#     suppressWarnings(print(plot_a+ggtitle(paste("Distribution of",var)) + xlab("")))
#     suppressWarnings(print(plot_b+ggtitle(paste("Distribution of",var)) + xlab("")))
#     suppressWarnings(print(plot_c+ggtitle(paste("Distribution of",var)) + xlab("")))
#     rm(plot_a, plot_b, plot_c)
#     
#     formula <- paste(var,"~ dept")
#     gplots::plotmeans(eval(parse(text=formula)), data=data_summary_temp, xlab="Department", main="Mean Plot with 95% CI")  
# }

#=== show pct na in sampled data

if("pct_na" %in% names(data_summary_temp)){
  data_summary_temp$na_groups <- "[0]"   #no extended intervals 
  data_summary_temp$na_groups[data_summary_temp$pct_na > 0]    <- "(0,0.33)"   #no extended intervals 
  data_summary_temp$na_groups[data_summary_temp$pct_na >= 0.33] <- "[0.33,0.66)"
  data_summary_temp$na_groups[data_summary_temp$pct_na >= 0.66] <- "[0.66,1]"
  data_summary_temp$na_groups <-  factor(data_summary_temp$na_groups, 
                                         levels=c("[0]", "(0,0.33)", "[0.33,0.66)", "[0.66,1]"))
  
  summary <- data_summary_temp %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_na, na.rm=T),3))
  cbPalette <- c("#999999", "#009E73", "#F0E442", "#E69F00")
  plot_a <- ggplot() +geom_bar(data=data_summary_temp,aes(x=dept, fill=na_groups)) + scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods") + 
    geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("mean:",avg))) +  #add labels
    ggtitle("Percent missing data")
  suppressWarnings(print(plot_a))
}


#=== classify sash management practices===
if(all(c("pct_open","openings_over_5","openings_over_24") %in% names(data_summary_temp))){
  data_summary_temp$sash_management <- "no activity"   #no extended intervals 
  data_summary_temp$sash_management[data_summary_temp$pct_open > 0] <- "good"   #no extended intervals 
  data_summary_temp$sash_management[data_summary_temp$openings_over_5>0 & data_summary_temp$openings_over_24==0] <- "moderate"
  data_summary_temp$sash_management[data_summary_temp$openings_over_24>0] <- "poor"
  data_summary_temp$sash_management <-  factor(data_summary_temp$sash_management, 
                                               levels=c("no activity", "good", "moderate", "poor"))
  
  cbPalette <- c("#999999", "#009E73", "#F0E442", "#E69F00")
  plot_a <- ggplot() +geom_bar(data=data_summary_temp, aes(x=dept, fill=sash_management)) + scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods") + ggtitle("Sash Management Class")
  suppressWarnings(print(plot_a))
}

#=== med_open_hours
if("med_opening_hrs" %in% names(data_summary_temp)){
  data_summary_temp$med_open_hours_group <- "[0]"   #no extended intervals 
  data_summary_temp$med_open_hours_group[data_summary_temp$med_opening_hrs > 0] <- "(0,5)"   #no extended intervals 
  data_summary_temp$med_open_hours_group[data_summary_temp$med_opening_hrs >= 5] <- "[5,24)"
  data_summary_temp$med_open_hours_group[data_summary_temp$med_opening_hrs >= 24] <- "[24,72)"
  data_summary_temp$med_open_hours_group[data_summary_temp$med_opening_hrs >= 72] <- "[100,168]"
  data_summary_temp$med_open_hours_group <-  factor(data_summary_temp$med_open_hours_group, 
                                                    levels=c("[0]", "(0,5)", "[5,24)", "[24,72)", "[100,168]"))
  
  summary <- data_summary_temp %>% group_by(dept) %>% 
    dplyr::summarize(avg_med_hrs=round(mean(med_opening_hrs, na.rm=T),2))
  cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
  plot_a <- ggplot() +geom_bar(data=data_summary_temp, aes(x=dept, fill=med_open_hours_group)) + 
    scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods")+
    geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("mean:",avg_med_hrs))) + #add labels
    ggtitle("Median open period duration (hours)")
  suppressWarnings(print(plot_a))
  
  plot_a <- ggplot(summary) +geom_bar(stat="identity", aes(x=dept, y=avg_med_hrs), fill= cbPalette[4]) + 
    theme_minimal()+xlab("Department")+ylab("Hours")+
    geom_text(data=summary, aes(x = dept, y = avg_med_hrs+1, label = paste(avg_med_hrs, "hours"))) + #add labels
    ggtitle("Median Sash Opening Duration for an Average Hood")
  suppressWarnings(print(plot_a))
}

### total_openings
if(all(c("total_openings","med_opening_hrs") %in% names(data_summary_temp))){
  data_summary_temp$total_openings_group <- "[0]"   #no extended intervals 
  data_summary_temp$total_openings_group[data_summary_temp$total_openings > 0]     <- "(0,3)"   #no extended intervals 
  data_summary_temp$total_openings_group[data_summary_temp$total_openings >= 3] <- "[3, 7)"   #no extended intervals 
  data_summary_temp$total_openings_group[data_summary_temp$total_openings >= 7] <- "[7, 14)"   #no extended intervals 
  data_summary_temp$total_openings_group[data_summary_temp$total_openings >= 14] <- "[14,50)"
  data_summary_temp$total_openings_group <-  factor(data_summary_temp$total_openings_group, 
                                                    levels=c("[0]", "(0,3)", "[3, 7)" ,"[7, 14)", "[14,50)"))
  
  summary <- data_summary_temp %>% group_by(dept) %>% 
    dplyr::summarize(avg_num_openings=round(mean(total_openings, na.rm=T),2),
                     avg_med_hrs=round(mean(med_opening_hrs, na.rm=T),2))
  cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
  plot_a <- ggplot(data_summary_temp) +geom_bar(aes(x=dept, fill=total_openings_group)) + 
    scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods")+
    geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("mean:",avg_num_openings))) + #add labels
    ggtitle("Total number of recorded hood openings")
  suppressWarnings(print(plot_a))
  
  plot_a <- ggplot(summary) +geom_bar(stat="identity", aes(x=dept, y=avg_num_openings), fill= cbPalette[3]) + 
    theme_minimal()+xlab("Department")+ ylab("Count")+
    geom_text(data=summary, aes(x = dept, y = avg_num_openings+1, label = paste(avg_num_openings, "openings"))) + #add labels
    ylim(0,max(summary$avg_num_openings, na.rm=T)+4) + ggtitle("Average number of sash open periods")
  suppressWarnings(print(plot_a))
}

### pct_active
if("pct_active" %in% names(data_summary_temp)){
  data_summary_temp$pct_active_group <- "[0]"   #no extended intervals 
  data_summary_temp$pct_active_group[data_summary_temp$pct_active > 0]     <- "(0,0.10)"   #no extended intervals 
  data_summary_temp$pct_active_group[data_summary_temp$pct_active >= 0.10] <- "[0.10, 0.20)"   #no extended intervals 
  data_summary_temp$pct_active_group[data_summary_temp$pct_active >= 0.20] <- "[0.20,0.30)"
  data_summary_temp$pct_active_group <-  factor(data_summary_temp$pct_active_group, 
                                                levels=c("[0]", "(0,0.10)", "[0.10, 0.20)" ,"[0.20,0.30)"))
  
  summary <- data_summary_temp %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_active, na.rm=T),3))
  cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
  plot_a <- ggplot(data_summary_temp) +geom_bar(aes(x=dept, fill=pct_active_group)) + 
    scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods")+
    geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("mean:",avg))) + #add labels
    ggtitle("Percent of Active Intervals by hood")
  suppressWarnings(print(plot_a))
}


### pct_exceeding_5
if("pct_exceeding_5" %in% names(data_summary_temp)){
  data_summary_temp$pct_open_exceeding_5_group <- "[0]"   #no extended intervals 
  data_summary_temp$pct_open_exceeding_5_group[data_summary_temp$pct_exceeding_5 > 0] <- "(0,.25)"   #no extended intervals 
  data_summary_temp$pct_open_exceeding_5_group[data_summary_temp$pct_exceeding_5 >= 0.25] <- "[0.25,0.5)"
  data_summary_temp$pct_open_exceeding_5_group[data_summary_temp$pct_exceeding_5 >= 0.5] <- "[0.5,0.75)"
  data_summary_temp$pct_open_exceeding_5_group[data_summary_temp$pct_exceeding_5 >= 0.75] <- "[0.75,1]"
  data_summary_temp$pct_open_exceeding_5_group <-  factor(data_summary_temp$pct_open_exceeding_5_group, 
                                                          levels=c("[0]", "(0,.25)", "[0.25,0.5)", "[0.5,0.75)", "[0.75,1]"))
  
  summary <- data_summary_temp %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_exceeding_5, na.rm=T),3))
  cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
  plot_a <- ggplot(data_summary_temp) +geom_bar(aes(x=dept, fill=pct_open_exceeding_5_group)) + 
    scale_fill_manual(values=cbPalette) +
    theme_minimal()+xlab("Department")+ylab("Number of Fume Hoods")+
    geom_text(data=summary, aes(x = dept, y = y_label_pos+1, label = paste("mean:",avg))) + #add labels
    ggtitle("Percent Inactive Intervals")
  suppressWarnings(print(plot_a))
}

#==== breakdown of percentage of time
if(all(c("pct_open","pct_active","pct_exceeding_5") %in% names(data_summary_temp))){
  summary <- data_summary_temp %>% 
    group_by(dept) %>%
    dplyr::summarize(avg_pct_open=round(mean(pct_open, na.rm=T),3),
                     avg_pct_active=round(mean(pct_active, na.rm=T),3),
                     avg_pct_exceeding_5=round(mean(pct_exceeding_5, na.rm=T),3)) %>%
    mutate(check=avg_pct_open-(avg_pct_active+avg_pct_exceeding_5)) %>%    #differences arise due to missing data
    mutate(pct_closed=1-(avg_pct_active+avg_pct_exceeding_5)) %>%
    select(dept, pct_active=avg_pct_active, pct_inactive=avg_pct_exceeding_5, pct_closed)
  
  summary_long <- reshape2::melt(summary, id.vars = c("dept"))
  summary_long$pos <- summary_long$value 
  summary_long$pos[summary_long$variable=="pct_active" & summary_long$value < 0.05] <- 0.02
  summary_long$pos[summary_long$variable=="pct_active" & summary_long$value >= 0.05] <- 0.05
  summary_long$pos[summary_long$variable=="pct_closed"] <- 1-(summary_long$pos[summary_long$variable=="pct_closed"] /2)
  summary_long$pos[summary_long$variable=="pct_inactive" & summary_long$value < 0.05] <- 0.10
  summary_long$pos[summary_long$variable=="pct_inactive" & summary_long$value > 0.05] <- 0.17
  summary_long$pos[summary_long$variable=="pct_inactive" & summary_long$value > 0.3] <- 0.25
  
  cbPalette <- c("#009E73", "#E69F00", "#999999")
  plot_a <- ggplot(summary_long) +
    geom_bar(width = 1, position="fill", stat="identity", aes(x=factor(1), y=value, fill=variable)) + 
    scale_fill_manual(values=cbPalette) + 
    theme_minimal()+ facet_grid(facets=. ~ dept)+
    coord_polar(theta="y") + xlab('') +  ylab('') + labs(fill="value") + 
    geom_text(aes(x = 1.1, y = pos, label = paste(value*100, "%"))) + #add labels
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    ggtitle("Breakdown of intervals for an average hood")
  suppressWarnings(print(plot_a))
}

graphics.off()         
closeAllConnections()
