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

ggplot(data = Agency, aes(x = "", y = Agency)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE, fill = "white", size = 1) + coord_flip() + xlab("") + labs(title = "SoA: All session average")


library(psych) 
# describe(Agency) 
# Let's describe some variables 
# install.packages("questionr") 
# install.packages("sjmisc") 
# install.packages("sjPlot") 
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

names(Agency)[29] <- "SoA"


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
sig <- lm(SoA ~ SIG, data = Agency)
summary(sig)
tab_model(sig, show.se = TRUE, show.stat = TRUE, digits = 2)
# (RT) Time
Agency$StartDate <- as.double(substr(Agency$StartDate, 12, 13))
names(Agency)[which(names(Agency)=="StartDate")] <- "TimeOfDay"
daytime <- lm(SoA ~ TimeOfDay, data = Agency)
summary(daytime)
tab_model(daytime, show.se = TRUE, show.stat = TRUE, digits = 2)
ggplot(Agency, aes(x = TimeOfDay, y = SoA)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")



##### SETUP SEARCH MODEL
#
#
AgencyPredictors <- Agency[,c(3,4,9,10,17,22,23,29,30,33)]
# [,c(4,5,7,8,9,10,11,16,17,21, 22, 28, 29)]

library(caret)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(SoA ~., data = AgencyPredictors,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
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
model1 <- lm(SoA ~ TIME + Arousal + MoodValence + MindValence + 
               Intention + Congruence + Goal, data = Agency)
model11 <- lm(SoA ~ TIME + Arousal + Goal, data = Agency)
modelTIME <- lm(SoA ~ TIME, data = Agency)
modelAROUS <- lm(SoA ~ Arousal, data = Agency)
summary(model1)
summary(model11)
tab_model(model1, show.se = TRUE, show.stat = TRUE, digits = 2)

lagging <- lm(SoA ~ TIME + Arousal + Goal + TMD + CAT +
                Goal*TMD + Goal*CAT, data = Agency)
summary(lagging)
tab_model(lagging, show.se = TRUE, show.stat = TRUE, digits = 2)


### LOCATION
#
plot(Agency$Location)
frq(Agency, Location, out="v")
Agency %>%group_by(Location) %>% summarise(no_rows = length(Location))
location <- lm(SoA ~ Location, data = Agency)
summary(location)
tab_model(location, show.se = TRUE, show.stat = TRUE, digits = 2)


### SOCIABILITY
#
plot(Agency$Sociability)
frq(Agency, Sociability, out="v")
social <- lm(SoA ~ Sociability, data = Agency)
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
action <- lm(SoA ~ ActionCode, data = Agency)
summary(action)
tab_model(action, show.se = TRUE, show.stat = TRUE, digits = 2)
ggplot(Agency, aes(x=ActionCode, y=SoA))+
  geom_boxplot() + coord_flip()

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
mind <- lm(SoA ~ MindCode, data = Agency)
summary(mind)
tab_model(mind, show.se = TRUE, show.stat = TRUE, digits = 2)
plot(SoA ~ MindCode, data = Agency)
ggplot(Agency, aes(x=MindCode, y=SoA))+
  geom_boxplot() + coord_flip()




ggplot(Agency, aes(x = TIME, y = SoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(Agency, aes(x = Arousal, y = SoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(Agency, aes(x = Intention, y = SoA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")




sink("a -- Agency Summary.txt", append=TRUE, split=TRUE) 
summary(Agency)
dev.off()

Agency <- merge(Agency, Participants[,c(1,9)], by="PID")

#### PLOT mean(SoA)|PID against SoPa
#

Mittelwerte <- Agency %>% group_by(PID) %>% summarise(meanSoA=mean(SoA,  na.rm = TRUE))
Agency <- merge(Agency, Mittelwerte, by="PID")

GlobalAgency <- lm(meanSoA ~ SoPa, data = Agency)
summary(GlobalAgency)
ggplot(Agency, aes(x = scale(SoPa, center = TRUE, scale = TRUE), y = scale(meanSoA, center = TRUE, scale = TRUE))) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


### END #################################
#
#save(Agency, file= "Agency_Cleaned.Rda")


require(dplyr)
SessionPerPerson <- Agency %>% count(PID)
Mittelwerte$meanSoA[sort(Mittelwerte$meanSoA)]

AvergPerson <- Agency %>% group_by(TIME) %>% summarise(AvergPerson=mean(SoA,  na.rm = TRUE))


# max: Person 71419072989 Wert: 5.285714
# min: Person 71519023849 Wert: 3.303571
b <- Agency[which(Agency$PID==71419072989|Agency$PID==71519023849),]
klein <- as.data.frame(cbind(b$TIME, b$SoA[1:14]))
names(klein)[1] <- "TIME"
names(klein)[2] <- "Min"
klein <- as.data.frame(cbind(klein, b$SoA[15:28], AvergPerson$AvergPerson[1:14]))
names(klein)[3] <- "Max"
names(klein)[4] <- "Avrg"


ggplot(klein, aes(x=TIME), legend=FALSE) +
  #geom_rect(mapping=aes(xmin=day, xmax=day+1, ymin=0, ymax=100, fill=posaff), alpha=0.8) +
  geom_point(aes(x=TIME,y = Min), shape=16, size=3,colour="blue") +
  geom_line(aes(x=TIME,y = Min), lty=1, size=1,colour="blue") +
  geom_point(aes(x=TIME,y = Max), shape=17, size=3,colour="red") +
  geom_line(aes(x=TIME,y = Max), lty=1, size=1,colour="red") +
  geom_point(aes(x=TIME,y = Avrg), shape=17, size=3,colour="green") +
  geom_line(aes(x=TIME,y = Avrg), lty=1, size=1,colour="green") +
  xlab("Time in Sessions") + 
  ylab("Minimum, Average, Maximum") + ylim(1.5,6.5) +
  scale_x_continuous(breaks=seq(0,15,by=1)) +
  labs(title="Minimum, Average and Maximum Participant") +
  geom_hline(yintercept=5.29, linetype="dashed", color = "blue") +
  geom_hline(yintercept=3.30, linetype="dashed", color = "red") +
  geom_hline(yintercept=4.40, linetype="dashed", color = "green")


ggplot(b, aes(x=TIME, y=SoA), legend=FALSE) +
  geom_point()+
  geom_smooth(method=lm, color="black")+
  labs(title="Miles per gallon \n according to the weight",
       x="Weight (lb/1000)", y = "Miles/(US) gallon")+
  theme_classic() 
