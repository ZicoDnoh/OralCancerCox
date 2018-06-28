library(survival)
library(KMsurv)

# read data
leuk = read.table("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/leuk.dat", 
                  sep=" ", head=T, skip=7) # read data
head(leuk)

# Cox PH Model
leuk.ph = coxph(Surv(weeks,remiss) ~ trtmt, data=leuk)
# Note: default = Efron method for handling ties
summary(leuk.ph)

# Breslow handling of ties
leuk.phb = coxph(Surv(weeks, remiss) ~ trtmt, data=leuk, method="breslow")
summary(leuk.phb)

# Exact handling of ties
leuk.phe = coxph(Surv(weeks, remiss) ~ trtmt, data=leuk, method="exact")
summary(leuk.phe)
