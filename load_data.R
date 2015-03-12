options(stringsAsFactors = FALSE)

library(ggplot2)
library(reshape2)

wd_data    <- "../data/"                                           #csv data is saved in this folder
data_files <- list.files(wd_data)

######### read files in format 1, with sash states provided
data_sashstate <- readLines(paste0(wd_data, data_files[3]))
start_chunks   <- c(which(grepl("Key|Time|<>Date",data_sashstate)))                          #count number of chunks

#reformat data: break file containing many chunks into single list 
all_data <- list()

meta_key <- lapply(data_sashstate[(start_chunks[1]+1):(start_chunks[2]-1)], FUN=function(r){              #keep metadata together
    r_spaces_removed <- gsub("([[:space:]]){2,}",",",r)
    new_line <- strsplit(r_spaces_removed,",")
    data.frame(key=(new_line[[1]][1]), hood=new_line[[1]][2], other=new_line[[1]][3])
})
meta_key <- dplyr::rbind_all(meta_key)
  
meta <- lapply(data_sashstate[(start_chunks[2]):(start_chunks[3]-1)], FUN=function(r){              #keep metadata together
    r_spaces_removed <- gsub("([[:space:]]){2,}",",",r)
    new_line <- strsplit(r_spaces_removed,",")
    data.frame(field=(new_line[[1]][1]), value=new_line[[1]][2])
})
meta <- dplyr::rbind_all(meta)

data <- lapply(data_sashstate[(start_chunks[3]):(length(data_sashstate)-1)], FUN=function(r){    #keep interval data together
    r_spaces_removed <- gsub("([[:space:]]){2,}",",",r)
    new_line <- strsplit(r_spaces_removed,",")
    data.frame(t(new_line[[1]]))
})
name_row <- data[[1]]
data <- dplyr::rbind_all(data[2:length(data)])
names(data) <- c("dttm",name_row[3:ncol(data)])
data$dttm <- as.POSIXct(data$dttm, format="%m/%d/%Y %H:%M")    #date formatting
data_long <- melt(data, id.vars = "dttm")    
data_long$value[data_long$value=="CLOSED"] <- 0                                                #number formatting
data_long$value[data_long$value=="OPEN"]   <- 1                                               #number formatting
data   <- dcast(data_long, dttm~ variable)  
data   <-data.frame(dttm=data[,1],apply(data[,2:ncol(data)],2,FUN=as.numeric))      #correct numberformatting 
all_data <- list(meta_key=meta_key, meta=meta, data=data)
################

#plot sash state data
pdf("sash_data.pdf", width=12, height=4)
for(c in 2:ncol(all_data$data)){
    
    fume_data <- data.frame(dttm=all_data$data$dttm, sash_pos=all_data$data[,c])
    
    fume_data$sash <- "black"
    fume_data$sash[fume_data$sash_pos==1] <-"red"
    
    run_length <- data.frame(lengths=rle(fume_data$sash)$lengths, values=rle(fume_data$sash)$values)
    run_length$cumsum <- cumsum(run_length$lengths)
    run_length$exceeds_5  <- run_length$values=="red" & run_length$lengths>10
    run_length$exceeds_8  <- run_length$values=="red" & run_length$lengths>16
    run_length$exceeds_24 <- run_length$values=="red" & run_length$lengths>48
    
    fume_data$exceeds_5 <- NA
    fume_data$exceeds_8 <- NA
    fume_data$exceeds_24 <- NA
    
    # if there are open/closed sash periods...
    if(nrow(run_length)>2){
        if(sum(run_length$exceeds_5)>0){        #mark those where the sash is open more than 5 hours
            for(i in 1:sum(run_length$exceeds_5)){
                rle_row <- which(run_length$exceeds_5)[i]
                if(rle_row==1){ fume_data$exceeds_5[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_5[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }
            }
        }
        if(sum(run_length$exceeds_8)>0){        #mark those where the sash is open more than 8 hours
            for(i in 1:sum(run_length$exceeds_8)){
                rle_row <- which(run_length$exceeds_8)[i]
                if(rle_row==1){ fume_data$exceeds_8[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_8[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
        if(sum(run_length$exceeds_24)>0){       #mark those where the sash is open more than 24 hours
            for(i in 1:sum(run_length$exceeds_24)){
                rle_row <- which(run_length$exceeds_24)[i]
                if(rle_row==1){ fume_data$exceeds_24[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_24[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
    }
    
    trend <- ggplot(fume_data)+
        geom_line(aes(x=dttm, y=sash_pos)) + 
        geom_point(aes(x=dttm, y=sash_pos), color=fume_data$sash) + 
        geom_point(aes(x=as.POSIXct(exceeds_5, origin="1970-01-01"), y=-0.1), color="yellow", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_8, origin="1970-01-01"), y=-0.1), color="orange", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_24, origin="1970-01-01"), y=-0.1), color="red", lty="-")+
        theme_minimal() + ggtitle(names(all_data$data)[c])
    suppressWarnings(print(trend))
}
graphics.off()
closeAllConnections()

######### read files in format 2, with ventilation rates provided
data_ventrates <- readLines(paste0(wd_data, data_files[2]))
data_ventrates <- gsub("\"", "",data_ventrates)                                      #remove unnecessary quotes within lines
start_chunks <- c(which(grepl("Point",data_ventrates)),(length(data_ventrates)+1))   #get line at which chunk starts
num_chunks   <- sum(grepl("Point",data_ventrates))                          #count number of chunks

#reformat data: break file containing many chunks into single list 
all_data <- list()
for(c in 1:num_chunks){
    chunk_data <- data_ventrates[start_chunks[c]:(start_chunks[c+1]-1)]    #isolate data within a chunk
    
    meta <- lapply(chunk_data[1:4], FUN=function(r){              #keep metadata together
        new_line <- strsplit(r,",")
        data.frame(field=(new_line[[1]][1]), value=new_line[[1]][2])
    })
    meta <- dplyr::rbind_all(meta)
    
    data <- lapply(chunk_data[5:length(chunk_data)], FUN=function(r){    #keep interval data together
        new_line <- strsplit(r,",")
        data.frame(date=new_line[[1]][1], 
                   time=new_line[[1]][2],
                   cfm=new_line[[1]][3],
                   value=new_line[[1]][4])
    })
    data <- dplyr::rbind_all(data)
    
    data <- data[!is.na(data$cfm),]                                                  #remove filler rows
    data$dttm <- as.POSIXct(paste(data$date, data$time), format="%m/%d/%Y %H:%M")    #date formatting
    data$cfm <- as.numeric(data$cfm)                                                 #number formatting
    
    all_data[[c]] <- list(meta=meta, data=data)
    print(paste(all_data[[c]]$meta$value[1], "     Number Intervals:",nrow(all_data[[c]]$data)))
}

#plot cfm data
pdf("ventilation_rate_data.pdf", width=12, height=4)
for(c in 1:length(all_data)){
    if(grepl("STPT$",all_data[[c]]$meta$value[1])) next         #only plot exhaust, not STPT
    
    fume_data <- all_data[[c]]$data
    
    baseline <- 125 + quantile(fume_data$cfm, 0.05, na.rm=TRUE)   #this needs to be tweaked
    fume_data$sash <- "black"
    fume_data$sash[fume_data$cfm>baseline] <-"red"
    
    run_length <- data.frame(lengths=rle(fume_data$sash)$lengths, values=rle(fume_data$sash)$values)
    run_length$cumsum <- cumsum(run_length$lengths)
    run_length$exceeds_5  <- run_length$values=="red" & run_length$lengths>10
    run_length$exceeds_8  <- run_length$values=="red" & run_length$lengths>16
    run_length$exceeds_24 <- run_length$values=="red" & run_length$lengths>48
    
    fume_data$exceeds_5 <- NA
    fume_data$exceeds_8 <- NA
    fume_data$exceeds_24 <- NA
    
    # if there are open/closed sash periods...
    if(nrow(run_length)>2){
        if(sum(run_length$exceeds_5)>0){        #mark those where the sash is open more than 5 hours
            for(i in 1:sum(run_length$exceeds_5)){
                rle_row <- which(run_length$exceeds_5)[i]
                if(rle_row==1){ fume_data$exceeds_5[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_5[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }
            }
        }
        if(sum(run_length$exceeds_8)>0){        #mark those where the sash is open more than 8 hours
            for(i in 1:sum(run_length$exceeds_8)){
                rle_row <- which(run_length$exceeds_8)[i]
                if(rle_row==1){ fume_data$exceeds_8[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_8[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
        if(sum(run_length$exceeds_24)>0){       #mark those where the sash is open more than 24 hours
            for(i in 1:sum(run_length$exceeds_24)){
                rle_row <- which(run_length$exceeds_24)[i]
                if(rle_row==1){ fume_data$exceeds_24[1:run_length$cumsum[rle_row]] <- fume_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          fume_data$exceeds_24[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- fume_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
    }

    trend <- ggplot(fume_data)+
        geom_line(aes(x=dttm, y=cfm)) + 
        geom_point(aes(x=dttm, y=cfm), color=fume_data$sash) + 
        geom_point(aes(x=as.POSIXct(exceeds_5, origin="1970-01-01"), y=min(cfm)-10), color="yellow", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_8, origin="1970-01-01"), y=min(cfm)-10), color="orange", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_24, origin="1970-01-01"), y=min(cfm)-10), color="red", lty="-")+
        theme_minimal() + ggtitle(all_data[[c]]$meta$value[1])
    suppressWarnings(print(trend))
}
graphics.off()
closeAllConnections()

# determine and calc metrics
# create graphs with sash position data