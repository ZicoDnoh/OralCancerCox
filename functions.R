create_Dx <- function(df) {
  
  df$Dx <- tolower(df$Dx)
  Dx_new <- rep(NA, nrow(df))
  types <- c("mild", "moderate", "severe", "cis", "scc", "other")
  for (i in seq_len(nrow(df))) {
    
    cur.val <- df$Dx[i]
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
  
  Dx_new[Dx_new == ""] <- "other"
  return(Dx_new)
}

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

create_surv <- function(df) {
  
  all_events <- c("mild", "mild/moderate", "moderate", "moderate/severe", "severe", "other",
                  "cis", "cis/scc", "scc")
  id <- unique(df$ID)
  
  result <- rep(NA, ncol(df) + 2)
  for (i in seq_along(id)) {
    
    cur.df <- subset(df, ID == id[i])
    time.pt <- elapsed_months(cur.df$Date, cur.df$Date[1])
    time.pt <- time.pt + as.numeric(cur.df$Age[1])
    
    cur.l <- 0
    pre.dx <- "mild"
    for (j in seq_len(nrow(cur.df))) {
      
      print(j)
      cur.dx <- as.character(cur.df[j,"Dx_new"])
      if (which(all_events == pre.dx) >= which(all_events == cur.dx)) {
        cur.l <- time.pt[j]
        pre.dx <- cur.dx
        next
      } else {
        cur.result <- c(cur.l, time.pt[j])
        exist <- unlist(cur.df[j,])
        cur.result <- c(exist, cur.result)
        result <- rbind(result, cur.result)
        
        cur.l <-  time.pt[j]
        pre.dx <- cur.dx
      }
    }
  }
  
  result <- result[-1,]
  colnames(result) <- c(colnames(df), "l", "u")
  result <- as.data.frame(result)
}

monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon 
} 

mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# take it for a spin
