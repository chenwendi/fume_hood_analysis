
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




###########
wd_output <-"../output/"
hood_summary <- read.csv(paste0(wd_output, "all_hood_summary.csv"))

# Prepare Data
hood_summary <- hood_summary[,7:ncol(hood_summary)]
hood_summary <- na.omit(hood_summary) # listwise deletion of missing
hood_summary <- scale(hood_summary) # standardize variables


# Determine number of clusters
wss <- (nrow(hood_summary)-1)*sum(apply(hood_summary,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(hood_summary, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(hood_summary, 5) # 5 cluster solution
# get cluster means 
aggregate(hood_summary,by=list(fit$cluster),FUN=mean)
# append cluster assignment
hood_summary <- data.frame(hood_summary, fit$cluster)


#plot cluster solutions

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(hood_summary, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
fpc::plotcluster(hood_summary, fit$cluster)
