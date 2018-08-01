library(readxl)

Data <- read_xlsx("Masters Research Data MERGED 2018 (highlighted).xlsx", sheet = "All Experiments",
                   col_names = TRUE)
colnames(Data) <- Data[1,]
Data <- Data[-1,]

highliIdx <- c(2,3,5,6,10,11,12,13,16,17,20,22,28,30,31,32,33,37,38,39,43,44,47,48,49,51,52,53,54,55,59,60,63,
               64,72,73,83,84,86,87,88,89)
Data <- Data[highliIdx,]

#------ column cleaning -------#
#Data$ID <- as.numeric(Data$ID)
#Data$Gender <- as.factor(Data$Gender)
#Data$Age <- as.numeric(Data$Age)
#Data$Alcohol <- as.factor(Data$Alcohol)
#Data$Tobacco <- as.factor(Data$Tobacco)

Data$AvgCellSize[Data$AvgCellSize == "Bad section"] <- NA
Data$AvgCellSize <- as.integer(Data$AvgCellSize)

Data$Date[Data$Date == 41604] <- "26-Nov-2013"
Data$Date <- as.Date(Data$Date, format = "%d-%b-%Y")
#--------------------------------#

Data2 <- Data[c(-90, -91, -92),c("ID", "Date", "Gender", "Age", "Alcohol", "Tobacco", "AvgCellSize", "Dx")]
Data2$Dx <- tolower(Data2$Dx)

#------ re-formatting ------#
Dx_new <- rep(NA, nrow(Data2))
types <- c("mild", "moderate", "severe", "cis", "scc", "other")
for (i in seq_len(nrow(Data2))) {
  
  cur.val <- Data2$Dx[i]
  search_result <- sapply(types, function(chr) grepl(chr, cur.val))
  
  if (sum(search_result) == 1) {
    Dx_new[i] <- types[search_result == TRUE]
  } else if (sum(search_result) > 1 & sum(search_result[c(4,5)]) == 1) {
    tmp <- types[search_result == TRUE]
    Dx_new[i] <- tmp[length(tmp)]
  } else {
    tmp <- types[search_result == TRUE]
    Dx_new[i] <- paste0(tmp, collapse = "/")
  }
  
}
#---------------------------#

Data$Dx_new <- Dx_new
Data2$Dx_new <- Dx_new

#-------------survival data creation---------------#
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

all_events <- c("mild", "mild/moderate", "moderate", "moderate/severe", "severe", "cis", "scc")
id <- unique(Data2$ID)
create_surv <- function(df) {
  
  result <- rep(NA, ncol(df) + 3)
  for (j in seq_along(id)) {
    
    print(j)
    cur.df <- subset(df, ID == id[j])
    time.pt <- elapsed_months(cur.df$Date, cur.df$Date[1])
    
    for (k in seq_len(nrow(cur.df) - 1)) {
      
      print(k)
      cur.start <- cur.df$Dx_new[k]
      cur.end <- cur.df$Dx_new[k + 1]
      
      if (cur.start == "other") {
        next
      } else if (which(all_events == cur.start) >= which(all_events == cur.end)) {
        next
      } else {
        possible <- all_events[(which(all_events == cur.start) + 1):which(all_events == cur.end)]
        cur.result <- cbind(rep(time.pt[k], length(possible)), rep(time.pt[k + 1], length(possible)), possible)
        
        exist <- matrix(rep(unlist(cur.df[k + 1,]), nrow(cur.result)), nrow = nrow(cur.result),
                        byrow = TRUE)
        cur.result <- cbind(exist, cur.result)
        result <- rbind(result, cur.result)
      }
    }
  }
  
  result <- result[-1,]
  colnames(result) <- c(colnames(df), "left", "right", "possible")
  result <- as.data.frame(result)
  
  result$ID <- as.numeric(as.character(result$ID))
  result$Date <- NULL
  result$Age <- as.numeric(as.character(result$Age))
  result$AvgCellSize <- as.numeric(as.character(result$AvgCellSize))
  result$Dx <- NULL
  result$Dx_new <- NULL
  result$left <- as.integer(as.character(result$left))
  result$right <- as.integer(as.character(result$right))
  result$possible <- as.character(result$possible)
  
  return(result)
}

Data2_new <- create_surv(Data2)

library(interval)
fit <- icfit(Surv(left,right, type="interval2") ~ Gender, data = Data2_new)

library(icenReg)
data(miceData)
