setwd("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics")
DeAgency <- read_delim("Agency ALL DEUTSCH.CSV",";", escape_double = FALSE, trim_ws = TRUE)

#[1] "StartDate"             "EndDate"               "Status"               
#[4] "IPAddress"             "Progress"              "Duration (in seconds)"
#[7] "Finished"              "RecordedDate"          "ResponseId"           
#[10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
#[13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
#[16] "DistributionChannel"   "UserLanguage"          "Q51"                  
#[19] "Q25"                   "Q37_1"                 "Q18"                  
#[22] "Q34_1"                 "D2"                    "Q35_1"                
#[25] "Q32"                   "Q1"                    "Q38"                  
#[28] "Q39"                   "Q40"                   "Q26"                  
#[31] "Q41"                   "Q19"                   "Q15_1"                
#[34] "Q21"                   "Q3"                    "Q10"                  
#[37] "Q12"                   "SSID"                  "PID"                  
#[40] "SMS"                   "DAY"                   "SIG"                  
#[43] "RDate"                 "TIME"                  "RT"                   
#[46] "TimeZone"              "addcode"               "RID"                  
#[49] "MindRaw"               "MindCode"              "MindValence"          
#[52] "Congruence"            "ActionRaw"             "ActionCode"  

# Throw out variables
DeAgency <- DeAgency[ -c(1:2), -c(2:17, 20, 22, 35, 37, 39, 42, 45:47) ]

# Remove empty rows
library(dplyr)
library(janitor)
DeAgency <- DeAgency %>% remove_empty("rows")

Participants <- read_delim("a--Participants.csv", ";", escape_double = FALSE, trim_ws = TRUE)

DeAgency <- DeAgency[which(as.numeric(DeAgency$PID) %in% Participants$PID),]

### VARIABLE SETUP
colnames(DeAgency)[colnames(DeAgency)=="Q25"] <- "MoodValence"
DeAgency$MoodValence <- as.numeric(DeAgency$MoodValence)
DeAgency$MoodValence[which(DeAgency$MoodValence==1 | DeAgency$MoodValence==2)] <- 1
DeAgency$MoodValence[which(DeAgency$MoodValence==3 | DeAgency$MoodValence==4)] <- 2
DeAgency$MoodValence[which(DeAgency$MoodValence==5)] <- 3
DeAgency$MoodValence[which(DeAgency$MoodValence==6 | DeAgency$MoodValence==7)] <- 4
DeAgency$MoodValence[which(DeAgency$MoodValence==8 | DeAgency$MoodValence==9)] <- 5

colnames(DeAgency)[colnames(DeAgency)=="Q37_1"] <- "Arousal"
unique(DeAgency$Arousal)
DeAgency$Arousal <- as.numeric(DeAgency$Arousal)

colnames(DeAgency)[colnames(DeAgency)=="Q34_1"] <- "ClosenessMindTracking"
unique(DeAgency$ClosenessMindTracking)

colnames(DeAgency)[colnames(DeAgency)=="Q35_1"] <- "ClosenessActionTracking"
unique(DeAgency$ClosenessActionTracking)

colnames(DeAgency)[colnames(DeAgency)=="Q32"] <- "Tracking"
DeAgency$Tracking <- factor(DeAgency$Tracking, levels = c("1", "2"), labels=c("Yes","No"))
unique(DeAgency$Tracking)

colnames(DeAgency)[colnames(DeAgency)=="Q3"] <- "Location"
colnames(DeAgency)[colnames(DeAgency)=="Q12"] <- "Sociability"

DeAgency$Location <- factor(DeAgency$Location, 
                            levels = c("1", "2", "3","4","5","6"),
                            labels=c("home","work/school","outdoor-public","indoor-public","public-transport","walking"))
DeAgency$Sociability <- factor(DeAgency$Sociability, 
                               levels = c("1", "4","5","6","7","8","9","10","11","12"),
                               labels=c("alone","not together","stranger","colleague","friend","family member","2+ strangers","2+ colleagues","2+ friends","2+ family members"))

# Turning characters to numeric
cols.num <- c(2:5, 7:12, 14, 18:20, 24:26, 28 )
DeAgency[cols.num] <- sapply(DeAgency[cols.num],as.numeric)
# 1, 21, 22 are time formats which get destroyed by being turned numeric

# Agency Questions
DeAgency$Q1 <- 8-(DeAgency$Q1-1)

colnames(DeAgency)[colnames(DeAgency)=="Q38"] <- "Q2"
DeAgency$Q2 <- 8-(DeAgency$Q2-1)

colnames(DeAgency)[colnames(DeAgency)=="Q39"] <- "Q3"
DeAgency$Q3 <- (DeAgency$Q3-1)

colnames(DeAgency)[colnames(DeAgency)=="Q40"] <- "Q4"
DeAgency$Q4 <- DeAgency$Q4-1

colnames(DeAgency)[colnames(DeAgency)=="Q26"] <- "Others"
DeAgency$Others <- factor(DeAgency$Others, levels = c("1", "2"), labels=c("Yes","No"))

colnames(DeAgency)[colnames(DeAgency)=="Q41"] <- "Q5"
DeAgency$Q5 <- DeAgency$Q5-1

colnames(DeAgency)[colnames(DeAgency)=="Q19"] <- "Learnings"

colnames(DeAgency)[colnames(DeAgency)=="Q15_1"] <- "TimeLag"

colnames(DeAgency)[colnames(DeAgency)=="Q21"] <- "Goal"
DeAgency$Goal <- factor(DeAgency$Goal, levels = c("5", "1", "3", "4"), labels=c("Indifferent","Overperform","OnTrack", "Behind"))

DeAgency$Source <- 1
DeAgency$Source <- factor(DeAgency$Source, levels = c("1", "2"), labels=c("German","English"))

# Tests
sapply(DeAgency, class)
save(DeAgency, file="DeAgency.Rda")
