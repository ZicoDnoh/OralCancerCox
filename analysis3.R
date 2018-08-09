data <- read.csv("Data_with_events.csv")

data_c <- subset(data, select = c("Percent", "event", "final_event", "duration"))
rm_idx <- which(is.na(data_c$event))
data_c <- data_c[-rm_idx,]

data_c$censor <- rep(0, nrow(data_c))
data_c$censor[is.na(data_c$final_event)] <- 1

data_c$Percent <- data_c$Percent * 0.01

library(survcomp)

result <- concordance.index(x = data_c$Percent, 
                            surv.time = data_c$duration, 
                            surv.event = data_c$censor, method="noether")

surv <- Surv(data_c$duration, data_c$censor)
result2 <- survConcordance(surv ~ data_c$Percent)

###########################################################

data <- data[-rm_idx,]
data_a <- subset(data, select = c("ID", "completely.excised", "Bx.Type", "Gender",
                                  "Age", "Alcohol", "Tobacco", "Location.Category",
                                  "Localized.Diffuse", "AvgCellSize", "DAB", "event", "final_event",
                                  "duration"))
data_a$censor <- rep(0, nrow(data_a))
data_a$censor[is.na(data_a$final_event)] <- 1

data_a$Alcohol <- as.factor(data_a$Alcohol)
data_a$Tobacco <- as.factor(data_a$Tobacco)
data_a$Location.Category <- as.factor(data_a$Location.Category)

library("survival")
library("survminer")

covariates <- c("completely.excised", "Bx.Type", "Gender", "Age", "Alcohol",
                "Tobacco", "Location.Category", "Localized.Diffuse", "AvgCellSize",
                "DAB")

cox1 <- coxph(Surv(duration, censor) ~ completely.excised, data = data_a)
cox2 <- coxph(Surv(duration, censor) ~ Bx.Type, data = data_a)
cox3 <- coxph(Surv(duration, censor) ~ Gender, data = data_a)
cox4 <- coxph(Surv(duration, censor) ~ Age, data = data_a)
cox5 <- coxph(Surv(duration, censor) ~ Alcohol, data = data_a)
cox6 <- coxph(Surv(duration, censor) ~ Tobacco, data = data_a)
cox7 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)
cox8 <- coxph(Surv(duration, censor) ~ Localized.Diffuse, data = data_a)
cox9 <- coxph(Surv(duration, censor) ~ AvgCellSize, data = data_a)
cox10 <- coxph(Surv(duration, censor) ~ DAB, data = data_a)

## Caution:
# it is impossible to run stepwise selection because of huge NAs
# each of variable are not significant except for Location Category

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "2")
cox7.2 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "3")
cox7.3 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "4")
cox7.4 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "5") ## meaningless
cox7.5<- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "6") ## meaningless 
cox7.6 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

data_a$Location.Category <- relevel(data_a$Location.Category, ref = "7") ## meaningless
cox7.7 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)

# significant cateogries: 1-2, 1-4, 3-4
# category 5,6,7 are actually meaningless (only one data point)
# the model itself is not significant.. (p-value = 0.152)

data_a2 <- data_a
data_a2 <- data_a2[-which(data_a2$Location.Category == c(5,6,7)),]
data_a2$Location.Category <- factor(data_a2$Location.Category, 
                                       levels = c("1", "2", "3", "4"))

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "1")
cox7.8 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "2")
cox7.9 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "3")
cox7.10 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "4")
cox7.11 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

# significant cateogries: 1-2, 1-4, 3-4
# the model is now significant (p-value = 0.0476)
# 40 observations should be removed if we want to fit a whole cox model....

###################################################################

mean(data$duration)
hist(data$duration)
unique(data$duration)

data <- read.csv("Data_with_events2.csv")

data_c <- subset(data, select = c("Percent", "event", "final_event", "duration"))
rm_idx <- which(is.na(data_c$event))
data_c <- data_c[-rm_idx,]

data_c$censor <- rep(0, nrow(data_c))
data_c$censor[is.na(data_c$final_event)] <- 1

data_c$Percent <- data_c$Percent * 0.01

data <- data[-rm_idx,]
data_a <- subset(data, select = c("ID", "completely.excised", "Bx.Type", "Gender",
                                  "Age", "Alcohol", "Tobacco", "Location.Category",
                                  "Localized.Diffuse", "AvgCellSize", "DAB", "event", "final_event",
                                  "duration"))
data_a$censor <- rep(0, nrow(data_a))
data_a$censor[is.na(data_a$final_event)] <- 1

data_a$Alcohol <- as.factor(data_a$Alcohol)
data_a$Tobacco <- as.factor(data_a$Tobacco)
data_a$Location.Category <- as.factor(data_a$Location.Category)

library("survival")
library("survminer")

covariates <- c("completely.excised", "Bx.Type", "Gender", "Age", "Alcohol",
                "Tobacco", "Location.Category", "Localized.Diffuse", "AvgCellSize",
                "DAB")

cox1 <- coxph(Surv(duration, censor) ~ completely.excised, data = data_a)
cox2 <- coxph(Surv(duration, censor) ~ Bx.Type, data = data_a)
cox3 <- coxph(Surv(duration, censor) ~ Gender, data = data_a)
cox4 <- coxph(Surv(duration, censor) ~ Age, data = data_a)
cox5 <- coxph(Surv(duration, censor) ~ Alcohol, data = data_a)
cox6 <- coxph(Surv(duration, censor) ~ Tobacco, data = data_a)
cox7 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a)
cox8 <- coxph(Surv(duration, censor) ~ Localized.Diffuse, data = data_a)
cox9 <- coxph(Surv(duration, censor) ~ AvgCellSize, data = data_a)
cox10 <- coxph(Surv(duration, censor) ~ DAB, data = data_a)

## change of event definition did not change the result.. (60months -> 12 months)
## not change even though changing to 7months (median of duration)

data_a2 <- data_a
data_a2 <- data_a2[-which(data_a2$Location.Category == c(5,6,7)),]
data_a2$Location.Category <- factor(data_a2$Location.Category, 
                                    levels = c("1", "2", "3", "4"))

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "1")
cox7.8 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "2")
cox7.9 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "3")
cox7.10 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

data_a2$Location.Category <- relevel(data_a2$Location.Category, ref = "4")
cox7.11 <- coxph(Surv(duration, censor) ~ Location.Category, data = data_a2)

# significant categories: 1-2, 1-4, 2-3, 3-4

new_df1 <- with(data_a2,
               data.frame(Location.Category = c("1", "2")
               )
)
new_df3 <- with(data_a2,
                data.frame(Location.Category = "3"
                )
)

fit1 <- survfit(cox7.9, newdata = new_df1)
fit3 <- survfit(cox7.11, newdata = new_df3)

ggsurvplot(fit1, conf.int = TRUE,
           ggtheme = theme_minimal(), 
           legend.labs= c("Category = 2", "Category = 3"), data = new_df1)
ggsurvplot(fit3, conf.int = TRUE, palette = "#2E9FDF",
           ggtheme = theme_minimal(), 
           legend.labs= "Category = 3", data = new_df3)

## plotting indicates the dataset is too small to analyze mayb..
## 1-3 and 3-4 location category is significant in general..

##########################################################