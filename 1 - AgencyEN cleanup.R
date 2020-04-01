setwd("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics")
AgencyEN <- read_delim("Agency ALL ENGISH.CSV",";", escape_double = FALSE, trim_ws = TRUE)

#[1] "StartDate"             "EndDate"               "Status"               
#[4] "IPAddress"             "Progress"              "Duration (in seconds)"
#[7] "Finished"              "RecordedDate"          "ResponseId"           
#[10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
#[13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
#[16] "DistributionChannel"   "UserLanguage"          "Q25"                  
#[19] "Q37_1"                 "Q18"                   "Q34_1"                
#[22] "D2"                    "Q35_1"                 "Q32"                  
#[25] "Q1"                    "Q2"                    "Q6"                   
#[28] "Q33"                   "Q26"                   "Q29"                  
#[31] "Q19"                   "Q15_1"                 "Q21"                  
#[34] "Q3"                    "Q10"                   "Q12"                  
#[37] "SSID"                  "PID"                   "SMS"                  
#[40] "DAY"                   "SIG"                   "RDate"                
#[43] "TIME"                  "RT"                    "TimeZone"             
#[46] "addcode"               "RID"  

names(AgencyEN)[48] <- "MindRaw"
names(AgencyEN)[49] <- "MindCode"
names(AgencyEN)[50] <- "MindValence"
names(AgencyEN)[51] <- "Congruence"
names(AgencyEN)[52] <- "ActionRaw"
names(AgencyEN)[53] <- "ActionCode"

# Throw out variables
AgencyEN <- AgencyEN[ -c(1:2), -c(2:17, 20, 22, 35, 37, 39, 42, 45:47) ]

# Remove empty rows
library(dplyr)
library(janitor)
AgencyEN <- AgencyEN %>% remove_empty("rows")

Participants <- read_delim("a--Participants.csv", ";", escape_double = FALSE, trim_ws = TRUE)

AgencyEN <- AgencyEN[which(as.numeric(AgencyEN$PID) %in% Participants$PID),]

### VARIABLE SETUP
colnames(AgencyEN)[colnames(AgencyEN)=="Q25"] <- "MoodValence"
AgencyEN$MoodValence <- as.numeric(AgencyEN$MoodValence)
AgencyEN$MoodValence[which(AgencyEN$MoodValence==1 | AgencyEN$MoodValence==2)] <- 1
AgencyEN$MoodValence[which(AgencyEN$MoodValence==3 | AgencyEN$MoodValence==4)] <- 2
AgencyEN$MoodValence[which(AgencyEN$MoodValence==5)] <- 3
AgencyEN$MoodValence[which(AgencyEN$MoodValence==6 | AgencyEN$MoodValence==7)] <- 4
AgencyEN$MoodValence[which(AgencyEN$MoodValence==8 | AgencyEN$MoodValence==9)] <- 5

colnames(AgencyEN)[colnames(AgencyEN)=="Q37_1"] <- "Arousal"
unique(AgencyEN$Arousal)
AgencyEN$Arousal <- as.numeric(AgencyEN$Arousal)

colnames(AgencyEN)[colnames(AgencyEN)=="Q34_1"] <- "ClosenessMindTracking"
unique(AgencyEN$ClosenessMindTracking)

colnames(AgencyEN)[colnames(AgencyEN)=="Q35_1"] <- "ClosenessActionTracking"
unique(AgencyEN$ClosenessActionTracking)

colnames(AgencyEN)[colnames(AgencyEN)=="Q32"] <- "Tracking"
AgencyEN$Tracking <- factor(AgencyEN$Tracking, levels = c("1", "2"), labels=c("Yes","No"))
unique(AgencyEN$Tracking)

# Turning characters to numeric
cols.num <- c(3:5, 7:12)
AgencyEN[cols.num] <- sapply(AgencyEN[cols.num],as.numeric)

colnames(AgencyEN)[colnames(AgencyEN)=="Q3"] <- "Location"
colnames(AgencyEN)[colnames(AgencyEN)=="Q12"] <- "Sociability"

AgencyEN$Location <- factor(AgencyEN$Location, 
                            levels = c("1", "2", "3","4","5","6"),
                            labels=c("home","work/school","outdoor-public","indoor-public","public-transport","walking"))
AgencyEN$Sociability <- factor(AgencyEN$Sociability, 
                               levels = c("1", "4","5","6","7","8","9","10","11","12"),
                               labels=c("alone","not together","stranger","colleague","friend","family member","2+ strangers","2+ colleagues","2+ friends","2+ family members"))

# Agency Questions
AgencyEN$Q1 <- 8-AgencyEN$Q1
AgencyEN$Q2 <- 8-AgencyEN$Q2
colnames(AgencyEN)[colnames(AgencyEN)=="Q6"] <- "Q3"
colnames(AgencyEN)[colnames(AgencyEN)=="Q33"] <- "Q4"
AgencyEN$Q4 <- AgencyEN$Q4-4
colnames(AgencyEN)[colnames(AgencyEN)=="Q26"] <- "Others"
AgencyEN$Others <- factor(AgencyEN$Others, levels = c("1", "2"), labels=c("Yes","No"))
colnames(AgencyEN)[colnames(AgencyEN)=="Q29"] <- "Q5"
AgencyEN$Q5 <- AgencyEN$Q5-3

colnames(AgencyEN)[colnames(AgencyEN)=="Q19"] <- "Learnings"

colnames(AgencyEN)[colnames(AgencyEN)=="Q15_1"] <- "TimeLag"

colnames(AgencyEN)[colnames(AgencyEN)=="Q21"] <- "Goal"
AgencyEN$Goal <- factor(AgencyEN$Goal, levels = c("1", "2", "3", "4"), labels=c("Indifferent","Overperform","OnTrack", "Behind"))

# Turning characters to numeric
cols.num <- c(2:5, 7:10, 12, 14, 18:20, 24:26, 28 )
AgencyEN[cols.num] <- sapply(AgencyEN[cols.num],as.numeric)

AgencyEN$Source <- 2
AgencyEN$Source <- factor(AgencyEN$Source, levels = c("1", "2"), labels=c("German","English"))

# Tests
sapply(AgencyEN, class)

save(AgencyEN, file="AgencyEN.Rda")
