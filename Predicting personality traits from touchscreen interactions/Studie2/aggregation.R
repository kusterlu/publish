# #######################################################
# # AGGREGATE FEATURES
# #######################################################

setwd("C:/Users/Asus/tubcloud/Studium/J_SS17/Bachelor Arbeit/Studie_Giorgio")
require('gplots')
library(ggplot2)
library(lattice)
library(GGally)
library(plyr)
library(dplyr)
library(reshape)
library(moments)
library(psych)

source("preprocessData.R")
source("getTouchData.R")
source("visualizeData.R")
# 
VP.numbers <- 611:655
User.ids <- 190:234
421
# 
# Load Data from FB
FB.Values <- read.csv("FB-Werte.csv", sep=",")
FB.Values <- FB.Values[1:4]
FB.Values <- preprocessFBData(FB.Values, VP.numbers, User.ids)
FB.Values <- FB.Values[!FB.Values$UserId==216,]
##############FB.Values$SessionNr <- 1:dim(FB.Values)[1]
# 
FB.Values$Condition <- as.factor(FB.Values$Condition)
# 
# # Load Data from Demographics
FB.Demograph <- read.csv("FB-Demograph.csv", comment.char="#")
FB.Demograph <- preprocessDemographicsData(FB.Demograph, VP.numbers, User.ids)
FB.Demograph <- FB.Demograph[c(1,2,3,4,6)]
FB.Demograph <- FB.Demograph[!FB.Demograph$UserId==216,]


load("InteractionDataStudie01.Rda")
data.experiment.interactions <- data.experiment.interactions[data.experiment.interactions$user_id %in% User.ids,]
data.experiment.features <- subset(data.experiment.interactions, event=="touch.release")
data.experiment.features <- with(data.experiment.features, data.frame(timestamp, user_id, sessionNr, taskNr, interactionNr,
                                                                      App, Cond, button_pressed, correct_answer, button_height, button_origin_x, button_origin_y, button_width, button_center_x, button_center_y, freezing_duration,
                                                                      touch.duration, swipe_length, swipe_speed, time_between_touches, difference.touch_buttonCenter_x, difference.touch_buttonCenter_y,
                                                                      touchAccuracy, touchAccuracy_x, touchAccuracy_y,
                                                                      sessionDuration))
data.experiment.features$SessionTaskInteractionNr <- factor(with(data.experiment.features, interaction(sessionNr, taskNr, interactionNr)))
data.experiment.features$SessionTaskNr <- factor(with(data.experiment.features, interaction(sessionNr, taskNr)))
data.experiment.features$SessionInteractionNr <- factor(with(data.experiment.features, interaction(sessionNr, interactionNr)))
data.experiment.features$TaskInteractionNr <- factor(with(data.experiment.features, interaction(taskNr, interactionNr)))
data.experiment.features$SessionTaskInteractionNr <- factor(with(data.experiment.features, interaction(SessionTaskNr, interactionNr)))

### clean data.experiment.features from flawed sessions
# exclude subject 216
data.experiment.features <- data.experiment.features[!data.experiment.features$user_id==216,]
# subject 234: session: (537, 538) - change session number 538 into 537
user <- 231
from <- 495
to <- 496
data.experiment.features[data.experiment.features$user_id==user & data.experiment.features$sessionNr %in% c(from,to),]$sessionNr <- data.experiment.features[data.experiment.features$user_id==user & data.experiment.features$sessionNr %in% c(from,to),]$sessionNr[1]
###! watch out with session duration here.


#find flaw #screening tool
#number of sessions conducted for VP in set.
einzig <- unique(data.experiment.features$sessionNr, drop=FALSE)
# list the number of sessions for each VP
spur <- data.experiment.features[1:length(User.ids),c(2,3)]
colnames(spur) <- c("User", "Sessions")
k <- as.integer(min(User.ids))
for (i in 190:max(User.ids)) {
  spur$User[i-189] <- i
  spur$Sessions[i-189] <- length(unique(data.experiment.features[with(data.experiment.features, data.experiment.features$user_id==i),]$sessionNr))
}
rm(k)
# --> pro VP schwankt die Anzahl der Sessions zwischen 11 und 14, eventuell kürzere die nachher rausgefiltert werden.
plot(spur$Sessions)
text(spur$Sessions, row.names(spur$User), cex=0.6, pos=4, col="blue")
problem <- spur[spur$Sessions!=12,]
problem <- times[times$user_id%in%problem$User,]

# save(data.experiment.features, file="Features-190-to-234-cleaned.Rda")



load("Features-190-to-234-cleaned.Rda")
# #######################################################
# # Showing all features
# #######################################################
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$touch.duration)
# hist(data.experiment.features$touch.duration)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$time_between_touches)
# hist(data.experiment.features$time_between_touches)
# # data.experiment.features <- data.experiment.features[data.experiment.features$time_between_touches < 15,]#300,]
# 
# # test <- subset(data.experiment.features, time_between_touches < 15)
# # rownames(test) <- NULL
# # plot(test$time_between_touches)
# # hist(test$time_between_touches)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$swipe_length)
# hist(data.experiment.features$swipe_length)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$swipe_speed)
# hist(data.experiment.features$swipe_speed)
# 
# par(mfrow=c(2,2))
# plot(data.experiment.features$difference.touch_buttonCenter_x)
# hist(data.experiment.features$difference.touch_buttonCenter_x)
# plot(data.experiment.features$difference.touch_buttonCenter_y)
# hist(data.experiment.features$difference.touch_buttonCenter_y)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$sessionDuration)
# hist(data.experiment.features$sessionDuration)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$taskDuration)
# hist(data.experiment.features$taskDuration)
# 
# par(mfrow=c(1,2))
# plot(data.experiment.features$interactionDuration)
# hist(data.experiment.features$interactionDuration)
# 
# ##########################################
# # aggregating task-dependent features
# ##########################################

FB.Values$SessionNr <- unique(data.experiment.features$sessionNr)
 
data.features.aggregated <- with(FB.Values, data.frame(SessionNr, VP, VP.Touches, Condition))



# data.ratings <- with(FB.Values, data.frame(SessionNr, VP, VP.Touches, App, Cond, Cond.App, SEA, SEA.std, PQ, HQ, ATT, PQ.std, HQ.std, ATT.std))
# 
# ##########################################
# # TODO: REMOVE!!!!!
# ##########################################
# 
# # Load Data from FB
# FB.Values <- read.csv("FB-Werte.csv", sep=";")
# FB.Values <- preprocessFBData(FB.Values, VP.numbers, User.ids)
# FB.Values$SessionNr <- 1:dim(FB.Values)[1]
# 
# FB.Values$Cond.App <- factor(with(FB.Values, interaction(Cond, App)))
# 
# ##########################################
# 
# aggregate durations of interaction, tasks, session
agg <- aggregate(data=data.experiment.features, sessionDuration ~ sessionNr, mean)
agg <- rename(agg, c("sessionDuration"="SessionDuration"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")
# 
# # count touches, interaction, tasks per interaction, task, session
# # number of tasks per session
agg <- aggregate(data=data.experiment.features, taskNr ~ sessionNr, function(x) length(unique(x)))
agg <- rename(agg, c("taskNr"="Count.TasksPerSession"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")
# 
# # number interactions per session
agg <- aggregate(data=data.experiment.features, interactionNr ~ sessionNr, function(x) length(unique(x)))
agg <- rename(agg, c("interactionNr"="Count.InteractionsPerSession"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")
# 
# # number of touches per session
agg <- data.frame(table(data.experiment.features$sessionNr))
agg <- rename(agg, c("Var1"="sessionNr", "Freq"="Count.TouchesPerSession"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")
# 
# 
# 
agg2 <- aggregate(data=data.features.aggregated, Count.InteractionsPerSession ~ Condition, mean)

agg2 <- t(agg2)
names <- agg2[1,]
agg2 <- agg2[-1,]
agg2 <- as.integer(round(as.numeric(agg2))) / 2
names(agg2) <- names
barchart(agg2, xlab="Average Interaction Count", main="Average Interaction Count per Condition")

agg2 <- aggregate(data=data.features.aggregated, Count.TasksPerSession ~ Condition, mean)

agg2 <- t(agg2)
names <- agg2[1,]
agg2 <- agg2[-1,]
agg2 <- as.integer(round(as.numeric(agg2))) / 2
names(agg2) <- names
barchart(agg2, xlab="Average Task Count", main="Average Task Count per Condition")

# number of touches per interaction per session
data.experiment.features.touch.down <- data.experiment.features
touch.down.per.interaction <- as.data.frame(table(data.experiment.features.touch.down$SessionTaskInteractionNr))
touch.down.per.task <- as.data.frame(table(data.experiment.features.touch.down$SessionTaskNr))
touch.down.per.session <- as.data.frame(table(data.experiment.features.touch.down$sessionNr))

# number of button touches per interaction per session
data.experiment.features.button.touched <- data.experiment.features[data.experiment.features$button_pressed,]
button.touch.per.interaction <- as.data.frame(table(data.experiment.features.button.touched$SessionTaskInteractionNr))
button.touch.per.task <- as.data.frame(table(data.experiment.features.button.touched$SessionTaskNr))
button.touch.per.session <- as.data.frame(table(data.experiment.features.button.touched$sessionNr))
correct.button.touch.per.interaction <- as.data.frame(table(data.experiment.features.button.touched$SessionTaskInteractionNr[data.experiment.features.button.touched$correct_answer]))
correct.button.touch.per.task <- as.data.frame(table(data.experiment.features.button.touched$SessionTaskNr[data.experiment.features.button.touched$correct_answer]))
correct.button.touch.per.session <- as.data.frame(table(data.experiment.features.button.touched$sessionNr[data.experiment.features.button.touched$correct_answer]))

data.experiment.features.touch.down.missed <- data.experiment.features[!data.experiment.features$button_pressed,]
missed.button.per.interaction <- as.data.frame(table(data.experiment.features.touch.down.missed$SessionTaskInteractionNr))
missed.button.per.task <- as.data.frame(table(data.experiment.features.touch.down.missed$SessionTaskNr))
missed.button.per.session <- as.data.frame(table(data.experiment.features.touch.down.missed$sessionNr))


touch.down.per.interaction <- rename(touch.down.per.interaction, c("Freq"="Count.TouchesPerInteraction"))
touch.down.per.task <- rename(touch.down.per.task, c("Freq"="Count.TouchesPerTask"))
touch.down.per.session <- rename(touch.down.per.session, c("Freq"="Count.TouchesPerSession2"))

button.touch.per.interaction <- rename(button.touch.per.interaction, c("Freq"="Count.ButtonTouchedPerInteraction"))
button.touch.per.task <- rename(button.touch.per.task, c("Freq"="Count.ButtonTouchedPerTask"))
button.touch.per.session <- rename(button.touch.per.session, c("Freq"="Count.ButtonTouchedPerSession"))
correct.button.touch.per.interaction <- rename(correct.button.touch.per.interaction, c("Freq"="Count.CorrectButtonTouchedPerInteraction"))
correct.button.touch.per.task <- rename(correct.button.touch.per.task, c("Freq"="Count.CorrectButtonTouchedPerTask"))
correct.button.touch.per.session <- rename(correct.button.touch.per.session, c("Freq"="Count.CorrectButtonTouchedPerSession"))

missed.button.per.interaction <- rename(missed.button.per.interaction, c("Freq"="Count.MissedButtonPerInteraction"))
missed.button.per.task <- rename(missed.button.per.task, c("Freq"="Count.MissedButtonPerTask"))
missed.button.per.session <- rename(missed.button.per.session, c("Freq"="Count.MissedButtonPerSession"))


data.experiment.features <- merge(data.experiment.features, touch.down.per.interaction, by.x = "SessionTaskInteractionNr", by.y = "Var1", all.x=T)
data.experiment.features <- merge(data.experiment.features, touch.down.per.task, by.x = "SessionTaskNr", by.y = "Var1", all.x=T)

data.experiment.features <- merge(data.experiment.features, button.touch.per.interaction, by.x = "SessionTaskInteractionNr", by.y = "Var1", all.x=T)
data.experiment.features <- merge(data.experiment.features, button.touch.per.task, by.x = "SessionTaskNr", by.y = "Var1", all.x=T)

data.experiment.features <- merge(data.experiment.features, correct.button.touch.per.interaction, by.x = "SessionTaskInteractionNr", by.y = "Var1", all.x=T)
data.experiment.features <- merge(data.experiment.features, correct.button.touch.per.task, by.x = "SessionTaskNr", by.y = "Var1", all.x=T)

data.experiment.features <- merge(data.experiment.features, missed.button.per.interaction, by.x = "SessionTaskInteractionNr", by.y = "Var1", all.x=T)
data.experiment.features <- merge(data.experiment.features, missed.button.per.task, by.x = "SessionTaskNr", by.y = "Var1", all.x=T)

data.experiment.features <- data.frame(data.experiment.features[order(data.experiment.features$timestamp),])
rownames(data.experiment.features) <- NULL

# data.features.aggregated <- merge(data.features.aggregated, touch.down.per.session, by.x = "SessionNr", by.y = "Var1", all.x=T)
data.features.aggregated <- merge(data.features.aggregated, button.touch.per.session, by.x = "SessionNr", by.y = "Var1", all.x=T)
data.features.aggregated <- merge(data.features.aggregated, correct.button.touch.per.session, by.x = "SessionNr", by.y = "Var1", all.x=T)
data.features.aggregated <- merge(data.features.aggregated, missed.button.per.session, by.x = "SessionNr", by.y = "Var1", all.x=T)
data.features.aggregated$Count.MissedButtonPerSession[is.na(data.features.aggregated$Count.MissedButtonPerSession)] <- 0
rownames(data.features.aggregated) <- NULL

# aggregate Counts per session

agg <- aggregate(data=data.experiment.features, Count.TouchesPerInteraction ~ sessionNr, mean)
agg <- rename(agg, c("Count.TouchesPerInteraction"="MeanPerSession.Count.TouchesPerInteraction"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.TouchesPerTask ~ sessionNr, mean)
agg <- rename(agg, c("Count.TouchesPerTask"="MeanPerSession.Count.TouchesPerTask"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.ButtonTouchedPerInteraction ~ sessionNr, mean)
agg <- rename(agg, c("Count.ButtonTouchedPerInteraction"="MeanPerSession.Count.ButtonTouchedPerInteraction"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.ButtonTouchedPerTask ~ sessionNr, mean)
agg <- rename(agg, c("Count.ButtonTouchedPerTask"="MeanPerSession.Count.ButtonTouchedPerTask"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.CorrectButtonTouchedPerInteraction ~ sessionNr, mean)
agg <- rename(agg, c("Count.CorrectButtonTouchedPerInteraction"="MeanPerSession.Count.CorrectButtonTouchedPerInteraction"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.CorrectButtonTouchedPerTask ~ sessionNr, mean)
agg <- rename(agg, c("Count.CorrectButtonTouchedPerTask"="MeanPerSession.Count.CorrectButtonTouchedPerTask"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.MissedButtonPerInteraction ~ sessionNr, mean)
agg <- rename(agg, c("Count.MissedButtonPerInteraction"="MeanPerSession.Count.MissedButtonPerInteraction"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

agg <- aggregate(data=data.experiment.features, Count.MissedButtonPerTask ~ sessionNr, mean)
agg <- rename(agg, c("Count.MissedButtonPerTask"="MeanPerSession.Count.MissedButtonPerTask"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")

# 
# #######################################################
# # Kicking out some values
# #######################################################
par(mfrow=c(1,2))
plot(data.experiment.features$touch.duration)
hist(data.experiment.features$touch.duration)
touches.experiment <- touches.experiment[touches.experiment$duration < 4,]
# 
par(mfrow=c(1,2))
plot(data.experiment.features$time_between_touches)
hist(data.experiment.features$time_between_touches)
data.experiment.features <- data.experiment.features[data.experiment.features$time_between_touches < 30,]

par(mfrow=c(1,2))
plot(data.experiment.features$swipe_length)
hist(data.experiment.features$swipe_length)
data.experiment.features <- data.experiment.features[!(is.na(data.experiment.features$swipe_length)),]

par(mfrow=c(1,2))
plot(data.experiment.features$swipe_speed)
hist(data.experiment.features$swipe_speed)
data.experiment.features <- data.experiment.features[!(is.na(data.experiment.features$swipe_speed)),]
data.experiment.features$swipe_speed[is.infinite(data.experiment.features$swipe_speed) | data.experiment.features$swipe_speed  > 3000] <- 0


# #######################################################
# # Aggregating task-independent features
# #######################################################
# 
# # touch.duration, swipe_length, swipe_speed, time_between_touches, difference.touch_buttonCenter_x, difference.touch_buttonCenter_y,
# # touchAccuracy, touchAccuracy_x, touchAccuracy_y,
# 
agg <- aggregate(data=data.experiment.features, touch.duration ~ sessionNr, mean)
agg <- rename(agg, c("touch.duration"="MeanPerSession.touch.duration"))
data.features.aggregated <- merge(data.features.aggregated, agg, by.x = "SessionNr", by.y="sessionNr")



 

touches.experiment.touchBehaviour <- with(data.experiment.features, data.frame(user_id, sessionNr,
                                                                               touch.duration, swipe_length, swipe_speed, time_between_touches,
                                                                               difference.touch_buttonCenter_x, difference.touch_buttonCenter_y,
                                                                               touchAccuracy, touchAccuracy_x, touchAccuracy_y))
touches.experiment.touchBehaviour$user_id <- as.character(touches.experiment.touchBehaviour$user_id )
aggData <- describeBy(touches.experiment.touchBehaviour, group=list(touches.experiment.touchBehaviour$user_id,touches.experiment.touchBehaviour$sessionNr))

aggData.rownames <- rownames(aggData)
aggData.colnames <- colnames(aggData)

### Der Code geht für jede VP jede einzelne der 500 Sessions durch; warum ist das nötig; jede VP hat ja nur eine begrenzte 
# Zahl an Session, an der die überhaupt teilnimmt ??

save(data.experiment.features, file="Features-190-to-234-before-loop.Rda")
FB.Values$VP.Touches <- FB.Values$UserId

for (VP in aggData.rownames){
  for(Nr in aggData.colnames){
    entry <- data.frame(aggData[VP, Nr])
    for(statValue in c("mean", "sd", "median", "mad", "min", "max", "range", "skew", "kurtosis", "se")){
      for(touchBehaviour in c("touch.duration", "swipe_length", "swipe_speed", "time_between_touches",
                              "difference.touch_buttonCenter_x", "difference.touch_buttonCenter_y",
                              "touchAccuracy", "touchAccuracy_x", "touchAccuracy_y")){
        data.features.aggregated[data.features.aggregated$VP.Touches == VP & data.features.aggregated$SessionNr == Nr, paste0(touchBehaviour,".", statValue)] <- entry[touchBehaviour, statValue]
        print(VP)
        print(Nr)
        print(touchBehaviour)
        flush.console()
        
      }
    }
  }
}

# 
# rm(touches.experiment.touchBehaviour, aggData, aggData.colnames, aggData.rownames, VP, Nr, entry, statValue, touchBehaviour)
# 
save(data.experiment.features, file="FeaturesStudie_cleaned.Rda")
save(data.features.aggregated, file="AggregatedFeaturesPerSession_cleaned.Rda")
# save(data.ratings, file="RatingsStudie01.Rda")
# 
# 
rm(list = ls())
# 

