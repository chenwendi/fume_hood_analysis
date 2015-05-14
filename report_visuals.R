library(dplyr)

# "sample_data" is a hood summary with sampled hood-weeks in each row

sample_data <- read.csv(paste0(wd_output,"sample_data.csv"))

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
sample_data$new_lab_name <- sapply(sample_data$lab, FUN=function(n){lab_mapping$lab_name[which(lab_mapping$lab==n)]})

#order depts in sample data
new_dept <- sapply(sample_data$dept, FUN=function(s){ switch(s, "chem"="CCB", "bio"="Biolabs", "esl"="ESL", "fairchild"="S.Fairchild")})
sample_data$dept <- new_dept
sample_data$dept <- factor(sample_data$dept, levels=c("Biolabs", "ESL","CCB","S.Fairchild"))

#begin to produce visuas
wd_output_temp <- paste0(wd_output)
pdf(paste0(wd_output_temp,"report_visuals.pdf"), width=12, height=4)
y_label_pos <- 32 

#=== figure data availability===
plot_file_summary <- compressed_file_summary %>%
  mutate(min_date=as.POSIXct(as.Date(min_date)), max_date=as.POSIXct(as.Date(max_date))) %>%
  group_by(min_date, max_date, dept) %>%
  dplyr::summarize(count=n()) 
plot_file_summary$dept <- sapply(plot_file_summary$dept, FUN=function(s){ switch(s, "chem"="CCB", "bio"="Biolabs", "esl"="ESL", "fairchild"="S.Fairchild")})
plot_file_summary <- plot_file_summary[order(plot_file_summary$dept, decreasing=TRUE),]
plot_file_summary$file <- 1:nrow(plot_file_summary)
plot_file_summary <- plot_file_summary[,c("file", "min_date", "max_date", "dept", "count")]   #reorder columns
plot_a <- ggplot(plot_file_summary) + geom_segment(aes(y = file,yend = file, x = min_date, xend =max_date, color=dept,fill=dept), size=3)  +
  theme_minimal() + ggtitle("Data Availability Over Time") +
  scale_x_datetime(breaks = date_breaks("7 day"), labels=date_format("%b-%d")) + xlab("Week") 
plot_a <- plot_a + geom_text(aes(x = min_date+as.difftime(4, units="days"), y = file+0.3, label = paste(count, "hoods")), size=4)
print(plot_a)


#=== compare sample to population- distribution among ccb labs===
if(!"lab" %in% names(sample_data)) sample_data <- dplyr::left_join(sample_data, hood_mapping, by=c("hood", "dept"))
plot_sample_data <- sample_data %>% filter(dept=="CCB")
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

#=== compare sample to population- distribution over time===
plot_sample_data <- sample_data
plot_sample_data$min_date <- as.POSIXct(plot_sample_data$min_date)
plot_a <- ggplot(plot_sample_data) + geom_point(aes(y=dept, x=min_date, group=dept, color=dept), position="jitter") + 
  xlab("Week beginning")+ ylab("Number of hoods in sample")+
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
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks") + 
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))


#=== classify sash management practices===
sample_data$sash_management <- "no activity"   #no extended intervals 
sample_data$sash_management[sample_data$pct_open > 0] <- "good"   #no extended intervals 
sample_data$sash_management[sample_data$openings_over_5>0 & sample_data$openings_over_24==0] <- "moderate"
sample_data$sash_management[sample_data$openings_over_24>0] <- "poor"
sample_data$sash_management <-  factor(sample_data$sash_management, 
                                 levels=c("no activity", "good", "moderate", "poor"))

cbPalette <- c("#999999", "#009E73", "#F0E442", "#E69F00")
plot_a <- ggplot() +geom_bar(data=sample_data, aes(x=dept, fill=sash_management)) + scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")
suppressWarnings(print(plot_a))


#=== med_open_hours
sample_data$med_open_hours_group <- "[0]"   #no extended intervals 
sample_data$med_open_hours_group[sample_data$med_opening_hrs > 0] <- "(0,5)"   #no extended intervals 
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 5] <- "[5,24)"
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 24] <- "[24,72)"
sample_data$med_open_hours_group[sample_data$med_opening_hrs >= 72] <- "[100,168]"
sample_data$med_open_hours_group <-  factor(sample_data$med_open_hours_group, 
                                    levels=c("[0]", "(0,5)", "[5,24)", "[24,72)", "[100,168]"))

summary <- sample_data %>% group_by(dept) %>% 
  dplyr::summarize(avg_med_hrs=round(mean(med_opening_hrs, na.rm=T),2))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot() +geom_bar(data=sample_data, aes(x=dept, fill=med_open_hours_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg_med_hrs)))  #add labels
suppressWarnings(print(plot_a))

plot_a <- ggplot(summary) +geom_bar(stat="identity", aes(x=dept, y=avg_med_hrs), fill= cbPalette[4]) + 
  theme_minimal()+xlab("Department")+ylab("Median Sash Opening Duration")+
  geom_text(data=summary, aes(x = dept, y = avg_med_hrs+5, label = paste(avg_med_hrs, "hours")))  #add labels
suppressWarnings(print(plot_a))

### total_openings
sample_data$total_openings_group <- "[0]"   #no extended intervals 
sample_data$total_openings_group[sample_data$total_openings > 0]     <- "(0,3)"   #no extended intervals 
sample_data$total_openings_group[sample_data$total_openings >= 3] <- "[3, 7)"   #no extended intervals 
sample_data$total_openings_group[sample_data$total_openings >= 7] <- "[7, 14)"   #no extended intervals 
sample_data$total_openings_group[sample_data$total_openings >= 14] <- "[14,50)"
sample_data$total_openings_group <-  factor(sample_data$total_openings_group, 
                                            levels=c("[0]", "(0,3)", "[3, 7)" ,"[7, 14)", "[14,50)"))

summary <- sample_data %>% group_by(dept) %>% 
  dplyr::summarize(avg_num_openings=round(mean(total_openings, na.rm=T),2),
                   avg_med_hrs=round(mean(med_opening_hrs, na.rm=T),2))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=total_openings_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg_num_openings)))  #add labels
suppressWarnings(print(plot_a))

plot_a <- ggplot(summary) +geom_bar(stat="identity", aes(x=dept, y=avg_num_openings), fill= cbPalette[3]) + 
  theme_minimal()+xlab("Department")+ylab("Average number of sash open periods")+
  geom_text(data=summary, aes(x = dept, y = avg_num_openings+1, label = paste(avg_num_openings, "openings"))) + #add labels
  ylim(0,62.5/4)
suppressWarnings(print(plot_a))

summary_long <- reshape2::melt(summary, id.vars = c("dept"))
summary_long$value[summary_long$variable=="avg_num_openings"] <- 4*summary_long$value[summary_long$variable=="avg_num_openings"]
cbPalette <- c("#009E73", "#F0E442")
plot_a <- ggplot(summary_long) +geom_bar(stat="identity", position="dodge", aes(x=dept, y=value, fill=variable)) + 
  scale_fill_manual(values=cbPalette)+
  theme_minimal()+xlab("Department")+ylab("Median duration of sash open periods")+
  geom_text(data=summary, aes(x = as.numeric(dept)-0.23, y = avg_num_openings*4+8, label = paste(avg_num_openings, "\nsash\nopenings"))) + #add labels
  geom_text(data=summary, aes(x = as.numeric(dept)+0.23, y = avg_med_hrs+8, label = paste(avg_med_hrs, "\nhours/\nopening"))) + #add labels
  ylim(0,max(summary_long$value)+15)
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
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
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
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))


### pct_active
sample_data$pct_active_group <- "[0]"   #no extended intervals 
sample_data$pct_active_group[sample_data$pct_active > 0]     <- "(0,0.10)"   #no extended intervals 
sample_data$pct_active_group[sample_data$pct_active >= 0.10] <- "[0.10, 0.20)"   #no extended intervals 
sample_data$pct_active_group[sample_data$pct_active >= 0.20] <- "[0.20,0.30)"
sample_data$pct_active_group <-  factor(sample_data$pct_active_group, 
                                              levels=c("[0]", "(0,0.10)", "[0.10, 0.20)" ,"[0.20,0.30)"))

summary <- sample_data %>% group_by(dept) %>% dplyr::summarize(avg=round(mean(pct_active, na.rm=T),3))
cbPalette <- c("#999999", "#009E73","#56B4E9",  "#F0E442", "#E69F00")
plot_a <- ggplot(sample_data) +geom_bar(aes(x=dept, fill=pct_active_group)) + 
  scale_fill_manual(values=cbPalette) +
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
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
  theme_minimal()+xlab("Department")+ylab("Number of Fume Hood- Weeks")+
  geom_text(data=summary, aes(x = dept, y = y_label_pos, label = paste("mean:",avg)))  #add labels
suppressWarnings(print(plot_a))


#==== breakdown of percentage of time
summary <- sample_data %>% 
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
  theme(axis.ticks = element_blank(), axis.text = element_blank())

suppressWarnings(print(plot_a))
graphics.off()         
closeAllConnections()

save(hoods, sample_data,
     file="report_visuals_objects.Rdata")

#====== other calculations that aren't printed to pdf======
## investigating metrics here
# sample_data[,c("sample", "dept" ,"med_opening_hrs","pct_open")]
t(sample_data[3,])



