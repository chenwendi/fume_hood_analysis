summarize_groups <- function(data, value, grouping_var){
    groups <- unlist(unique(data[,grouping_var]))
    group_summary <- sapply(groups, FUN=function(g){
        value_d <- unlist(data[,value])
        group_d <- unlist(data[,grouping_var])
        s <- sd(value_d[group_d==g], na.rm=T)
        n <- length(!is.na(value_d[group_d==g]))
        t <- t.test(value_d[group_d==g])
        return(data.frame(mean=t$estimate, sd=s, n=n, conf_int1=t$conf.int[1], conf_int2=t$conf.int[2]))
    })
    group_summary <- as.data.frame(group_summary)
    names(group_summary) <- names(groups)
    return(group_summary)
}