library(dplyr)

# "sampled_data_summary" is a df with sampled hood info only
# "sample_data" is a hood summary with sampled hood-weeks in each row

#=== prepare to anonymize data for report====
#table showing number of hoods in each building
hoods <- hood_mapping %>% filter(exclude==FALSE)
sum(table(hoods$lab[hoods$dept=="chem"]))

#map labs to different names for anonymity
lab_mapping <- data.frame(lab=c(setdiff(unique(hoods$lab),c("bio", "esl","fairchild")),c("bio", "esl","fairchild")))
lab_mapping$lab_num <- 1:nrow(lab_mapping)
lab_mapping$lab_name <- paste0("lab_",letters[lab_mapping$lab_num])
lab_mapping$lab_name[lab_mapping$lab %in% c("bio", "esl","fairchild")] <- lab_mapping$lab[lab_mapping$lab %in% c("bio", "esl","fairchild")]

#anonymize hood data
sample_data <- dplyr::left_join(sample_data, hood_mapping, by="hood")
sample_data$new_lab_name <- sapply(sample_data$lab, FUN=function(n){lab_mapping$lab_name[which(lab_mapping$lab==n)]})

#begin to produce visuas
wd_output_temp <- paste0(wd_output)
pdf(paste0(wd_output_temp,"report_visuals.pdf"), width=12, height=4)
y_label_pos <- 32 

#=== figure data availability===
plot_file_summary <- compressed_file_summary %>%
  mutate(min_date=as.POSIXct(as.Date(min_date)), max_date=as.POSIXct(as.Date(max_date))) %>%
  group_by(min_date, max_date, dept) %>%
  dplyr::summarize(count=n()) 
plot_file_summary <- plot_file_summary[order(plot_file_summary$dept, decreasing=TRUE),]
plot_file_summary$file <- 1:nrow(plot_file_summary)
plot_file_summary <- plot_file_summary[,c("file", "min_date", "max_date", "dept", "count")]   #reorder columns
plot_a <- ggplot(plot_file_summary) + geom_segment(aes(y = file,yend = file, x = min_date, xend =max_date, color=dept,fill=dept), size=3)  +
  theme_minimal() + ggtitle("Data Availability Over Time") +
  scale_x_datetime(breaks = date_breaks("7 day"), labels=date_format("%b-%d")) + xlab("Week") 
plot_a <- plot_a + geom_text(aes(x = min_date+as.difftime(4, units="days"), y = file+0.3, label = paste(count, "hoods")), size=4)
print(plot_a)


#=== compare sample to population===
if(!"lab" %in% names(sampled_data_summary)) sampled_data_summary <- dplyr::left_join(sampled_data_summary, hood_mapping, by=c("hood", "dept"))
plot_sample_data <- sampled_data_summary %>% filter(dept=="chem")
plot_sample_data$new_lab_name <- unlist(sapply(plot_sample_data$lab, FUN=function(n){lab_mapping$lab_name[which(lab_mapping$lab==n)]}))
plot_a <- ggplot(plot_sample_data) + geom_bar(aes(y=..count.., x=new_lab_name)) + 
  xlab("Labs")+ ylab("Number of hoods in sample")+
  theme_minimal()
suppressWarnings(print(plot_a))

plot_compressed_data <- compressed_hood_summary %>% filter(dept=="chem")
plot_compressed_data$new_lab_name <- unlist(sapply(plot_compressed_data$lab, FUN=function(n){lab_mapping$lab_name[which(lab_mapping$lab==n)]}))
plot_a <- ggplot(plot_compressed_data) + geom_bar(aes(y=..count.., x=new_lab_name)) + 
  xlab("Labs")+ ylab("Number of hoods in population")+
  theme_minimal()
suppressWarnings(print(plot_a))


#=== hood data descriptors
plot_data <- compressed_hood_summary %>% filter(dept=="chem")
means <- round(c(mean(plot_data$min_v), mean(plot_data$max_v)))
plot_a <- ggplot(plot_data) + theme_minimal() + 
  geom_boxplot(aes(x="min", y=min_v))  + geom_boxplot(aes(x="max", y=max_v)) +
  ylab("CFM") + xlab("Ventilation rate descriptors") +
  geom_text(aes(x = c("min"),  y = -200,  label = paste("mean =",means[1])), size=5) +
  geom_text(aes(x = c("max"),  y = -200,  label = paste("mean =",means[2])), size=5) 
suppressWarnings(print(plot_a))


#=== show pct na in sampled data
sample_data$na_groups <- "[0]"   #no extended intervals 
sample_data$na_groups[sample_data$pct_na > 0]    <- "(0,0.33)"   #no extended intervals 
sample_data$na_groups[sample_data$pct_na >= 0.33] <- "[0.33,0.66)"
sample_data$na_groups[sample_data$pct_na >= 0.66] <- "[0.66,1]"
sample_data$na_groups <-  factor(sample_data$na_groups, 
                                    levels=c("[0]", "(0,0.33)", "[0.33,0.66)", "[0.66,1]"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_na, na.rm=T),3))
cbPalette <- c("#999999", "#009E73", "#F0E442", "#E69F00")
plot_a <- ggplot() +geom_bar(data=sample_data,aes(x=dept, fill=na_groups)) + scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count") + 
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))

table(sample_data$na_groups, sample_data$dept)


#=== classify occupant behavior===
sample_data$occ_behavior <- "no activity"   #no extended intervals 
sample_data$occ_behavior[sample_data$pct_open > 0] <- "good"   #no extended intervals 
sample_data$occ_behavior[sample_data$openings_over_5>0 & sample_data$openings_over_24==0] <- "mediocre"
sample_data$occ_behavior[sample_data$openings_over_24>0] <- "poor"
sample_data$occ_behavior <-  factor(sample_data$occ_behavior, 
                                 levels=c("no activity", "good", "mediocre", "poor"))

cbPalette <- c("#999999", "#009E73", "#F0E442", "#E69F00")
plot_a <- ggplot() +geom_bar(data=sample_data, aes(x=dept, fill=occ_behavior)) + scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")
suppressWarnings(print(plot_a))

table(sample_data$occ_behavior, sample_data$dept)


#=== score distribution of scores
sample_data$score_groups <- "[0]"   #no extended intervals 
sample_data$score_groups[sample_data$score > 0] <- "(0,75)"   #no extended intervals 
sample_data$score_groups[sample_data$score >= 75] <- "[75,150)"
sample_data$score_groups[sample_data$score >= 150] <- "[150,225)"
sample_data$score_groups[sample_data$score >= 225] <- "[225,300)"
sample_data$score_groups <-  factor(sample_data$score_groups, 
                            levels=c("[0]" ,"(0,75)", "[75,150)", "[150,225)", "[225,300)"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(score, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=score_groups)) + scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))

#=== med_open_hours
sample_data$med_open_hours_group <- "[0]"   #no extended intervals 
sample_data$med_open_hours_group[sample_data$med_opening_hrs > 0] <- "(0,5)"   #no extended intervals 
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 5] <- "[5,24)"
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 24] <- "[24,72)"
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 72] <- "[100,168]"
sample_data$med_open_hours_group <-  factor(sample_data$med_open_hours_group, 
                                    levels=c("[0]", "(0,5)", "[5,24)", "[24,72)", "[100,168]"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(med_opening_hrs, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot() +geom_bar(data=sample_data, aes(x=dept, fill=med_open_hours_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))

#=== pct_open
sample_data$pct_open_group <- "[0]"   #no extended intervals 
sample_data$pct_open_group[sample_data$pct_open > 0] <- "(0,0.25)"   #no extended intervals 
sample_data$pct_open_group[sample_data$pct_open >= 0.25] <- "[0.25,0.5)"
sample_data$pct_open_group[sample_data$pct_open >= 0.5] <- "[0.5,0.75)"
sample_data$pct_open_group[sample_data$pct_open >= 0.75] <- "[0.75,1]"
sample_data$pct_open_group <-  factor(sample_data$pct_open_group, 
                                            levels=c("[0]", "(0,0.25)", "[0.25,0.5)", "[0.5,0.75)", "[0.75,1]"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_open, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=pct_open_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))

### pct_open_under_5
sample_data$pct_open_under_5_group <- "[0]"   #no extended intervals 
sample_data$pct_open_under_5_group[sample_data$pct_open_under_5 > 0] <- "(0,0.15)"   #no extended intervals 
sample_data$pct_open_under_5_group[sample_data$pct_open_under_5 >= 0.15] <- "[0.15,0.3)"
sample_data$pct_open_under_5_group <-  factor(sample_data$pct_open_under_5_group, 
                                      levels=c("[0]", "(0,0.15)", "[0.15,0.3)"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_open_under_5, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=pct_open_under_5_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))


### pct_exceeding_5
sample_data$pct_open_exceeding_5_group <- "[0]"   #no extended intervals 
sample_data$pct_open_exceeding_5_group[sample_data$pct_exceeding_5 > 0] <- "(0,.25)"   #no extended intervals 
sample_data$pct_open_exceeding_5_group[sample_data$pct_exceeding_5 >= 0.25] <- "[0.25,0.5)"
sample_data$pct_open_exceeding_5_group[sample_data$pct_exceeding_5 >= 0.5] <- "[0.5,0.75)"
sample_data$pct_open_exceeding_5_group[sample_data$pct_exceeding_5 >= 0.75] <- "[0.75,1]"
sample_data$pct_open_exceeding_5_group <-  factor(sample_data$pct_open_exceeding_5_group, 
                                      levels=c("[0]", "(0,.25)", "[0.25,0.5)", "[0.5,0.75)", "[0.75,1]"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_exceeding_5, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=pct_open_exceeding_5_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Count")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))

graphics.off()         
closeAllConnections()

#====== other calculations that aren't printed to pdf======
## investigating metrics here
sample_data[,c("sample", "dept" ,"med_opening_hrs","pct_open")]
t(sample_data[3,])

#get group stats, 95% confidence interval
group_summary <- summarize_groups(data=sample_data, value="pct_exceeding_5", grouping_var="dept")
print(group_summary)

#compare each pair of groupds for significant differences
fit <- aov(pct_exceeding_5 ~ dept, data=sample_data)
TukeyHSD(fit)


# average flow estimations
means <- c(300,600)   #mean ventilation rate of closed hood, open hood
no_treatment_hoods <- sample_data %>% filter(dept %in% c("fairchild"))
avg_pct_open <- mean(no_treatment_hoods$pct_open, na.rm=T)
avg_flow <- means[1]*(1-avg_pct_open)+ means[2]*(avg_pct_open)
print(paste("Pct open:",round(avg_pct_open,3), ".... CFM:",round(avg_flow), ".... Cost ($):",round(avg_flow*5)))

#full year of open sash
print(paste("Pct open:",1, ".... CFM:",means[2], ".... Cost ($):",round(means[2]*5)))
print(paste("Pct open:",0, ".... CFM:",means[1], ".... Cost ($):",round(means[1]*5)))
(round(means[2]*5)-round(means[1]*5))/365   #cost of a day open compared to a day closed

