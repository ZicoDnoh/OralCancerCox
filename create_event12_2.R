library(readxl)

Data <- read_xlsx("Masters Research Data MERGED _new2 2018.xlsx", sheet = "All Experiments",
                  col_names = TRUE)
colnames(Data) <- Data[1,]
Data <- Data[-1,]
Data <- Data[-(nrow(Data)-c(0,1,2)),]

Data$Date[Data$Date == 41604] <- "26-Nov-2013"
Data$Date <- as.Date(Data$Date, format = "%d-%b-%Y")

Data$`Dx Category` <- as.numeric(Data$`Dx Category`)

# function to create event
create_event <- function(df) {
  
  times <- df$Date
  process <- df$`Dx Category`
  
  event <- c()
  final_event <- c()
  time_duration <- c()
  for (i in 1:(nrow(df) - 1)) {
    time_diff <- mondf(times[i], times[i + 1])
    time_duration <- c(time_duration, time_diff)
    
    if (process[i] == 1) {
      event <- c(event, NA)
      final_event <- c(final_event, NA)
    } else if (process[i + 1] > process[i]) {
      event <- c(event, 1)
      if (time_diff <= 12) {
        final_event <- c(final_event, 1)
      } else {
        final_event <- c(final_event, 0)
      }
    } else {
      event <- c(event, 0)
      if (time_diff > 12) {
        final_event <- c(final_event, 0)
      } else {
        final_event <- c(final_event, NA)
      }
    }
  }
  
  return(cbind(event, final_event, time_duration))
}

id <- unique(Data$ID)
result <- c(NA, NA, NA)
for (i in seq_along(id)) {
  
  cur.df <- subset(Data, Data$ID == id[i])
  cur.result <- rbind(c(NA, NA, NA), create_event(cur.df))
  result <- rbind(result, cur.result)
}
result <- result[-1,]
colnames(result) <- c("event", "final_event", "duration")

Data2 <- cbind(Data, result)
write.csv(Data2, "Data_with_events2.csv")
