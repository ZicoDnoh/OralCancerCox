library(readxl)
library(icenReg)

#Data <- read_xlsx("Masters Research Data MERGED _new2 2018.xlsx", sheet = "All Experiments",
#                  col_names = TRUE)
Data <- read.csv("Data_with_events.csv")
#colnames(Data) <- Data[1,]
#Data <- Data[-1,]
#Data$AvgCellSize[Data$AvgCellSize == "Bad section"] <- NA
#Data$AvgCellSize <- as.integer(Data$AvgCellSize)
#Data$Date[Data$Date == 41604] <- "26-Nov-2013"
#Data$Date <- as.Date(Data$Date, format = "%d-%b-%Y")

Data$Dx_new <- create_Dx(Data)

Data_new <- create_surv(Data)
Data_new$l <- as.integer(as.character(Data_new$l))
Data_new$u <- as.integer(as.character(Data_new$u))

idx <- c()
for (i in seq_len(nrow(Data_new))) {
  if (Data_new$l[i] == Data_new$u[i]) {
    idx <- c(idx,i)
  }
}


fit_ph <- ic_sp(cbind(l, u) ~ Gender, model = 'ph', bs_samples = 100, 
                data = Data_new)

newdata <- data.frame(Gender = c('1', '2'))
rownames(newdata) <- c('1', '2')
plot(fit_ph, newdata)

alcohol_na <- c(2,7,10,11,15,16,17,20,22,23)
tobacco_na <- c(2,4,7,9,10,11,15,16,17,20,23)
all_na <- unique(c(alcohol_na, tobacco_na))

Data_new$ID <- as.numeric(as.character(Data_new$ID))
Data_new2 <- Data_new[-which(Data_new$ID %in% all_na),]
Data_new2 <- Data_new2[-which(is.na(Data_new2$AvgCellSize)),]

Data_new2$AvgCellSize <- as.integer(as.character(Data_new2$AvgCellSize))
Data_new2$LocalDiff <- Data_new2[,"Localized/Diffuse"]
fit_ph <- ic_sp(cbind(l, u) ~ Gender + Tobacco + Alcohol + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                data = Data_new2)
fit_ph1_1 <- ic_sp(cbind(l, u) ~ Tobacco + Alcohol + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                data = Data_new2) ##
fit_ph1_2 <- ic_sp(cbind(l, u) ~ Gender + Alcohol + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph1_3 <- ic_sp(cbind(l, u) ~ Gender + Tobacco + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph1_4 <- ic_sp(cbind(l, u) ~ Gender + Tobacco + Alcohol + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph1_5 <- ic_sp(cbind(l, u) ~ Gender + Tobacco + Alcohol + LocalDiff, model = 'ph', bs_samples = 500, 
                   data = Data_new2)

fit_ph2_1 <- ic_sp(cbind(l, u) ~ Alcohol + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph2_2 <- ic_sp(cbind(l, u) ~ Tobacco + LocalDiff + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph2_3 <- ic_sp(cbind(l, u) ~ Tobacco + Alcohol + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2) ##
fit_ph2_4 <- ic_sp(cbind(l, u) ~ Tobacco + Alcohol + LocalDiff, model = 'ph', bs_samples = 500, 
                   data = Data_new2)

fit_ph3_1 <- ic_sp(cbind(l, u) ~ Alcohol + AvgCellSize, model = 'ph', bs_samples = 500, 
                  data = Data_new2)
fit_ph3_2 <- ic_sp(cbind(l, u) ~ Tobacco + AvgCellSize, model = 'ph', bs_samples = 500, 
                   data = Data_new2)
fit_ph3_3 <- ic_sp(cbind(l, u) ~ Tobacco + Alcohol, model = 'ph', bs_samples = 500, 
                   data = Data_new2)

