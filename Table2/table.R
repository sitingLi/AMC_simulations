# Read data and data processing
NEBCS <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/qualify/New England data (Scot Zens)/req_sitingli_deid_27jul2021.csv",
                  header = TRUE)
toenail <- NEBCS[,c('finstat', 'refage', 'sex', 'smkstat', 'educ', 'hrocc', 'muscinv', 
                    'toears', 'toese', 'toezn', 'toeAl', 'toeV', 'toeCr', 
                    'toeMn', 'toeFe', 'toeNi', 'toeCu','toeCd', 'toePb')]
toenail$muscinv[is.na(toenail$muscinv)] = 2
# table(toenail$muscinv,toenail$finstat)
toenail_log <- na.omit(toenail)
toenail_log <- toenail_log[which(toenail_log$muscinv!=33 & toenail_log$muscinv!=1),]
colnames(toenail_log)[1] <- "Outcome"
colnames(toenail_log)[2] <- "Age"
colnames(toenail_log)[3] <- "Sex"
colnames(toenail_log)[4] <- "Smoking"
colnames(toenail_log)[5] <- "Education"
colnames(toenail_log)[6] <- "Occupation"
colnames(toenail_log)[7] <- "Invasiveness"
colnames(toenail_log)[8] <- "Arsenic"
colnames(toenail_log)[9] <- "Selenium"
colnames(toenail_log)[10] <- "Zinc"
colnames(toenail_log)[11] <- "Aluminum"
colnames(toenail_log)[12] <- "Vanadium"
colnames(toenail_log)[13] <- "Chromium"
colnames(toenail_log)[14] <- "Manganese"
colnames(toenail_log)[15] <- "Iron"
colnames(toenail_log)[16] <- "Nickel"
colnames(toenail_log)[17] <- "Copper"
colnames(toenail_log)[18] <- "Cadmium"
colnames(toenail_log)[19] <- "Lead"

toenail_log$Outcome <- 
  factor(toenail_log$Outcome, levels=c(1,0),
         labels=c("Cases", "Controls"))


toenail_log$Sex <- 
  factor(toenail_log$Sex, levels=c(1,2),
         labels=c("Male", "Female"))

toenail_log$Smoking <- 
  factor(toenail_log$Smoking, levels=c(1,2,3),
         labels=c("Non-Smoker","Former Smoker","Current Smoker"))

toenail_log$Education <- 
  factor(toenail_log$Education, levels=c(1,2,3),
         labels=c("High School","College","Postgraduate"))

toenail_log$Occupation <- 
  factor(toenail_log$Occupation, levels=c(1,0),
         labels=c("High Risk","Low Risk"))


toenail_log <- subset (toenail_log, select = -Invasiveness)

toenail_log <- toenail_log[which(toenail_log$Arsenic!=0),]
toenail_log <- toenail_log[which(toenail_log$Selenium!=0),]
toenail_log <- toenail_log[which(toenail_log$Zinc!=0),]
toenail_log <- toenail_log[which(toenail_log$Aluminum!=0),]
toenail_log <- toenail_log[which(toenail_log$Vanadium!=0),]
toenail_log <- toenail_log[which(toenail_log$Chromium!=0),]
toenail_log <- toenail_log[which(toenail_log$Manganese!=0),]
toenail_log <- toenail_log[which(toenail_log$Iron!=0),]
toenail_log <- toenail_log[which(toenail_log$Nickel!=0),]
toenail_log <- toenail_log[which(toenail_log$Copper!=0),]
toenail_log <- toenail_log[which(toenail_log$Cadmium!=0),]
toenail_log <- toenail_log[which(toenail_log$Lead!=0),]




library (table1)
table1(~.| Outcome, data=toenail_log, overall=NULL)