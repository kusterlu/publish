setwd("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics")
load("DeAgency.Rda")
load("AgencyEN.Rda")
Agency <- rbind(AgencyEN, DeAgency)
rm(DeAgency, AgencyEN)

#a <- abc %>% group_by(PID) %>% summarise(App_IndivMean=mean(App,  na.rm = TRUE))

# reshuffle so time variables are in front
Agency <- Agency[c(18:20,22, 1,21,6,14,15,2:5,7:12,16:17,24:26,28,23,27,13,29)]
Agency <- Agency[with(Agency, order(PID, DAY, SIG)),]
#
#
Agency <- Agency[-which(is.na(Agency$Q1)&is.na(Agency$Q2)&is.na(Agency$Q3)&is.na(Agency$Q4)),]
Agency$Agency <- rowMeans(Agency[14:17])-1 #Mean, and set minium to zero.
boxplot(Agency$Agency)

library(ggplot2)
ggplot(data = Agency, aes(x = "", y = Agency)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE, fill = "white", size = 1) + coord_flip() + xlab("") + labs(title = "SoA: All session average")

names(Agency)[30] <- "SoA"

### Create State (or: Person independent) SoA, 
# by correcting for the difference to the global mean
#
Mittelwerte <- Agency %>% group_by(PID) %>% summarise(MeanSoA=mean(SoA,  na.rm = FALSE))
Mittelwerte$diff <- Mittelwerte$MeanSoA-(mean(Agency$SoA))
Agency <- merge(Agency, Mittelwerte, by="PID")
Agency$StateSoA <- Agency$SoA - Agency$diff
#
#                   
library(psych) 
library(questionr) 
library(sjmisc) 
library(sjPlot) 

### INTENTION VARIABLE
#
frq(Agency, Q5, out="v") 
Agency$Intention <- as.numeric(1) 
Agency$Intention[which(Agency$Q5<3.9)] <- 3
Agency$Intention[which(Agency$Q5>3.9)] <- 2
Agency$Intention <- factor(Agency$Intention, levels = c("1", "2", "3"), labels=c("Alone","NoShared","Shared"))
frq(Agency, Intention, out="v") 
#a <- Agency %>% group_by(PID) %>% summarise(App_IndivMean=mean(App,  na.rm = TRUE))

#### TIME VARIABLE
#
# so lange die Person ID gleich bleibt, nummeriere TIME gleichbleibend weiter. für alle Personsn. 
id <- Agency$PID[1]
j <- 1
for (i in 1:384) {
  if(Agency$PID[i]==id) {
    Agency$TIME[i] <- j
    j <- j+1
  }
  if(Agency$PID[i]!=id) {
    j=1
    Agency$TIME[i] <- j
    id <- Agency$PID[i]
    j <- j+1
  }
}

#drop unused time variables
Agency <- Agency[,-c(4)]
#names(Agency)[4] <- "Time"
Agency$TIME <- unlist(as.numeric(Agency$TIME))

Agency$Weekday <- as.double(substr(Agency$StartDate, 9, 10))
Wochentage <- Agency %>% group_by(Weekday) %>% summarise(WeekdayAVG=mean(StateSoA,  na.rm = TRUE))

Agency$Weekday <- as.factor(Agency$Weekday)
Dag <- lm(StateSoA ~ Weekday, data = Agency)
tab_model(Dag, show.se = TRUE, show.stat = TRUE, digits = 2)

### GOAL
#
frq(Agency, Goal, out="v")
#Agency$Goal[which(Agency$Goal=="OnTrack" | Agency$Goal=="Overperform")] <- "OnTrack"
#Agency$Goal <- droplevels(Agency$Goal)

### MIND VALENCE
#
Agency$MindValence[which(Agency$MindValence==0)] <- -1
Agency$MindValence[which(is.na(Agency$MindValence==0))] <- 0

### CLOSENESS ACTION
#
frq(Agency, ClosenessActionTracking, out="v")
Agency$CAT <- as.numeric(1)
Agency$CAT[which(Agency$ClosenessActionTracking>3.9)] <- 2
frq(Agency, CAT, out="v") 
Agency$CAT <- factor(Agency$CAT, levels = c("1", "2"), labels=c("aDistant","aClose"))
#
### CLOSENESS MIND
#
frq(Agency, ClosenessMindTracking, out="v")
Agency$TMD <- as.numeric(1)
Agency$TMD[which(Agency$ClosenessMindTracking>3.9)] <- 2
frq(Agency, TMD, out="v") 
Agency$TMD <- factor(Agency$TMD, levels = c("1", "2"), labels=c("mDistant","mClose"))


### Look for overarching static variables
#
#
library(readr)
load("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics/a--PARTICIPANTS.Rda")
Participants$TrackingIntensity <- as.numeric(unlist(Participants$TrackingIntensity))
frq(Participants, TrackingIntensity, out="v")
Participants$PID <- as.numeric(Participants$PID)
Agency <- merge(Agency, Participants[,c(1,8)], by="PID")

Agency$TrackingIntensity[which(Agency$TrackingIntensity<5.9)] <- 1
Agency$TrackingIntensity[which(Agency$TrackingIntensity>5.9)] <- 2
Agency$TrackingIntensity <- factor(Agency$TrackingIntensity, levels = c("1", "2"), labels=c("Low", "High"))
Agency$TrackingIntensity[which(is.na(Agency$TrackingIntensity))] <- "Low"
frq(Agency, TrackingIntensity, out="v")


### TIME-OF-DAY
#
# SIG
Agency$SIG <- factor(Agency$SIG, 
                     levels = c("1",    "2",         "3"),
                     labels=c("Morning","Afternoon", "Evening"))
sig <- lm(StateSoA ~ SIG, data = Agency)
summary(sig)
tab_model(sig, show.se = TRUE, show.stat = TRUE, digits = 2)
# (RT) Time
Agency$StartDate <- as.double(substr(Agency$StartDate, 12, 13))
names(Agency)[which(names(Agency)=="StartDate")] <- "TimeOfDay"

### Time-of-Day and SoA
#
daytime <- lm(StateSoA ~ TimeOfDay, data = Agency)
summary(daytime)
tab_model(daytime, show.se = TRUE, show.stat = TRUE, digits = 2)
ggplot(Agency, aes(x = TimeOfDay, y = StateSoA)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")
#
## Time-of-Day and Arousal
#
Wallung <- lm(StateSoA ~ TimeOfDay, data = Agency)
summary(Wallung)
tab_model(Wallung, show.se = TRUE, show.stat = TRUE, digits = 2)
ggplot(Agency, aes(x = TimeOfDay, y = Arousal)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")
#
# SIG and Arousal
#
Wal <- lm(Arousal ~ SIG, data = Agency)
summary(Wal)
tab_model(Wal, show.se = TRUE, show.stat = TRUE, digits = 2)


##### SETUP SEARCH MODEL
#
#
AgencyPredictors <- Agency[,c(3,4,9,10,17,22,23,32,33,36)]
# [,c(4,5,7,8,9,10,11,16,17,21, 22, 28, 29)]
library(caret)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(StateSoA ~., data = AgencyPredictors,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:10),
                    trControl = train.control,
                    na.action = na.omit
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)


library(psych)
library(snakecase)
library(questionr)
library(sjmisc)
library(sjPlot)
library(dplyr)
library(ggplot2)






### Explorative Linear Modelling with Variable Candidates. 
#
#
# Caret hatte Model mit Arousal und Time als am prädiktivsten vorher gesagt.
model1 <- lm(StateSoA ~ TIME + Arousal + MoodValence + MindValence + 
               Intention + Congruence, data = Agency)
model11 <- lm(StateSoA ~ TIME + Arousal + Goal, data = Agency)
modelTIME <- lm(StateSoA ~ TIME, data = Agency)
modelAROUS <- lm(StateSoA ~ Arousal, data = Agency)
summary(model1)
summary(model11)
tab_model(model1, show.se = TRUE, show.stat = TRUE, digits = 2)

lagging <- lm(StateSoA ~ TIME + Arousal + Goal + TMD + CAT +
                Goal*TMD + Goal*CAT, data = Agency)
summary(lagging)
tab_model(lagging, show.se = TRUE, show.stat = TRUE, digits = 2)


### LOCATION
#
plot(Agency$Location)
frq(Agency, Location, out="v")
Agency %>%group_by(Location) %>% summarise(no_rows = length(Location))
location <- lm(StateSoA ~ Location, data = Agency)
summary(location)
tab_model(location, show.se = TRUE, show.stat = TRUE, digits = 2)


### SOCIABILITY
#
plot(Agency$Sociability)
frq(Agency, Sociability, out="v")
social <- lm(StateSoA ~ Sociability, data = Agency)
summary(social)
tab_model(social, show.se = TRUE, show.stat = TRUE, digits = 2)


#### ACTION_CODE
#
Agency$ActionCode[which(Agency$ActionCode==43)] <- 44
Agency$ActionCode[which(Agency$ActionCode==3)] <- 5
Agency$ActionCode[which(Agency$ActionCode==45)] <- 25
Agency$ActionCode[which(Agency$ActionCode==35)] <- 34
Agency$ActionCode[which(Agency$ActionCode==25)] <- 45
Agency$ActionCode[which(Agency$ActionCode==26)] <- 43
Agency$ActionCode[which(is.na(Agency$ActionCode))] <- 5
ggplot(Agency, aes(x=ActionCode)) + 
  geom_histogram(binwidth=.5, position="dodge") + 
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  labs(title = "Action Categories after SECOND coding")
Agency$ActionCode <- factor(Agency$ActionCode, 
                            levels = c("12",     "13",       "14",       "15",    "21",    "22",         "23",        "24", 
                                       "31",      "32",      "33",    "34",     "36",     "37",  "38",            "39",
                                       "41",      "42",        "43",     "44",          "45","5"), 
                            labels=c("Studying","Employment", "Planning","Future","Eating","PersonalCare","Transport","Chores",
                                     "Socialising","TeleCom","Sports","TheArts","Romance","Walk","SocialConflict","FreetimeOrga",
                                     "MediaCons","ThinkOthers","NapSleep","Reflection","Relax","Mixed"))
frq(Agency, ActionCode, out="v")
action <- lm(StateSoA ~ ActionCode, data = Agency)
summary(action)
tab_model(action, show.se = TRUE, show.stat = TRUE, digits = 2)
ggplot(Agency, aes(x=ActionCode, y=StateSoA))+
  geom_boxplot() + coord_flip()

### Action Plot only with significant categories
#
Auswahl <- Agency[which(Agency$ActionCode=="Studying" 
                        | Agency$ActionCode=="Employment"
                        | Agency$ActionCode=="PersonalCare"
                        | Agency$ActionCode=="Transport"
                        | Agency$ActionCode=="Chores"
                        | Agency$ActionCode=="Socialising"
                        | Agency$ActionCode=="Romance"
                        | Agency$ActionCode=="MediaCons"
                        | Agency$ActionCode=="NapSleep"
                        | Agency$ActionCode=="Relax"),]
ggplot(Auswahl, aes(x=ActionCode, y=StateSoA, fill=ActionCode))+
  geom_boxplot(alpha=0.5)  +  coord_flip() +
  theme(legend.position="none") + geom_hline(yintercept=4.69, linetype="dashed", color = "red") +
  ggtitle("Significant Action Types")  + xlab("Action") + ylab("SoA") +labs(caption= "Significantly different from reference:Studying (dahsed line); Alpha-level 5%")

### MIND_CODE
#
Agency$MindCode[which(Agency$MindCode==43)] <- 44
Agency$MindCode[which(Agency$MindCode==3)] <- 5
Agency$MindCode[which(Agency$MindCode==45)] <- 25
Agency$MindCode[which(Agency$MindCode==35)] <- 34
Agency$MindCode[which(Agency$MindCode==25)] <- 45
Agency$MindCode[which(Agency$MindCode==26)] <- 43
Agency$MindCode[which(is.na(Agency$MindCode))] <- 5
ggplot(Agency, aes(x=MindCode)) + 
  geom_histogram(binwidth=.5, position="dodge") + 
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  labs(title = "Mind Categories after SECOND coding")
Agency$MindCode <- factor(Agency$MindCode, 
                          levels = c("12",     "13",       "14",       "15",    "21",    "22",         "23",        "24", 
                                     "31",      "32",      "33",    "34",     "36",     "37",  "38",            "39",
                                     "41",      "42",        "43",     "44",          "45","5"), 
                          labels=c("Studying","Employment", "Planning","Future","Eating","PersonalCare","Transport","Chores",
                                   "Socialising","TeleCom","Sports","TheArts","Romance","Walk","SocialConflict","FreetimeOrga",
                                   "MediaCons","ThinkOthers","NapSleep","Reflection","Relax","Mixed"))
frq(Agency, MindCode, out="v")
mind <- lm(StateSoA ~ MindCode, data = Agency)
summary(mind)
tab_model(mind, show.se = TRUE, show.stat = TRUE, digits = 2)
plot(StateSoA ~ MindCode, data = Agency)
ggplot(Agency, aes(x=MindCode, y=StateSoA))+
  geom_boxplot() + coord_flip()


Auswahl <- Agency[which(Agency$MindCode=="Studying" 
                        | Agency$MindCode=="Transport"
                        | Agency$MindCode=="Chores"
                        | Agency$MindCode=="Romance"
                        | Agency$MindCode=="ThinkOthers"
                        | Agency$MindCode=="Relax"),]
ggplot(Auswahl, aes(x=MindCode, y=StateSoA, fill=MindCode))+
  geom_boxplot(alpha=0.5)  +  coord_flip() +
  theme(legend.position="none") + geom_hline(yintercept=4.56, linetype="dashed", color = "red") +
  ggtitle("Significant Thought Types")  + xlab("Mind") + ylab("SoA") +labs(caption= "Significantly different from reference:Studying (dahsed line); Alpha-level 5%")




ggplot(Agency, aes(x = TIME, y = StateSoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(Agency, aes(x = Arousal, y = StateSoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(Agency, aes(x = Intention, y = StateSoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")




#sink("a -- Agency Summary.txt", append=TRUE, split=TRUE) 
#summary(Agency)
#dev.off()

#Agency <- merge(Agency, Participants[,c(1,9)], by="PID")


### END
#
#

#save(Agency, file= "Agency_Normed.Rda")

### print low agency moments
niedrig <- Agency[which(Agency$StateSoA<3),]
niedrig <- niedrig[,c(19,20,25,26,27,32)]
#write.csv(niedrig, file="LowAgencyMoments.csv")


### TMD | CAT experiment
#
#
nah <- Agency[which(Agency$TMD=="mClose"|Agency$CAT=="aClose"),]
a <- nah %>% group_by(PID) %>% summarise(number_of_session = n()) 
a <- a[which(a$number_of_session>4),]
nah <- nah[which(as.numeric(nah$PID) %in% a$PID),]

#no convergence
ctrl <- lmeControl(opt='optim')
zeit <- lme(fixed= StateSoA ~ 1 + TIME, 
              random= ~ 1 |PID, 
              data=nah,
              control=ctrl,
              na.action=na.exclude)
#

nana <- lm(StateSoA ~ TIME + Arousal, data = nah)
summary(nana)
tab_model(nana, show.se = TRUE, show.stat = TRUE, digits = 2)

ggplot(data=nah, aes(x=TIME, y=StateSoA, group=PID, color="gray")) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="blue") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="green") +
  xlab("Time of Study") + xlim(3,17) +
  ylab("Sense of Agency")  + ylim(3,6) +
  ggtitle("SoA in self-tracking domain") + theme(legend.position = "none")


# 71419090876 #coef -0.048 #behind: 0,33 #TIME: -0,02 #-2
# 71419091784 #coef -0.075 #behind: 0,29 #TIME: -0,02 #-3
# 71519023287 #coef  0.039 #behind: 0,43 #TIME: -0,02 #plus
# 71519081919 #coef -0.090 #behind: 0,07 #TIME: -0,02 #-4
# 71619061161 #coef -0.139 #behind: 1,00 #TIME: -0,02 #-5
# 71619061726 #coef -0.032 #behind: 0,00 #TIME: -0,02 #-1


a <- lm(StateSoA ~ TIME, data = subset(nah, PID ==71619061726))
a[["coefficients"]][["TIME"]]


