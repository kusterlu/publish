#### Setup
# set workign directory to Data Analysis
# manuall input: AggregatedFeaturesPerSession_cleaned.Rda (from Studie2)
Kus <- data.features.aggregated
AG <- read.csv("AG_Carola_Daten.csv")

# the temp variable is used as R cuts of the table-display after a certain number of variables
# a table with 120 variables is not fully displayed.
temp <- AG[,90:120]
# 
VP.numbers <- 611:655
User.ids <- 190:234
# 
library(plyr)
#
# # Load Data from Demographics
FB.Demograph <- read.csv("FB-Demograph.csv", comment.char="#")
FB.Demograph <- FB.Demograph[FB.Demograph$VP %in% VP.numbers,c(1,2,3,4,6)]

Kus <- merge(Kus, FB.Demograph, by = "VP")
Kus <- rename(Kus, c("Genre"="Gender"))
Kus <- merge(Kus, big5, by = "VP")

#
Kus <- transform(Kus, Count.TasksPerSession = Count.TasksPerSession / sessionDuration)
Kus <- transform(Kus, Count.InteractionsPerSession = Count.InteractionsPerSession / sessionDuration)
Kus <- transform(Kus, Count.TouchesPerSession = Count.TouchesPerSession / sessionDuration)
Kus <- transform(Kus, Count.MissedButtonPerSession = Count.MissedButtonPerSession / sessionDuration)
Kus <- transform(Kus, Count.ButtonTouchedPerSession = Count.ButtonTouchedPerSession / sessionDuration)
Kus <- transform(Kus, Count.CorrectButtonTouchedPerSession   = Count.CorrectButtonTouchedPerSession   / sessionDuration)

AG <- transform(AG, Count.TasksPerSession = Count.TasksPerSession / SessionDuration)
AG <- transform(AG, Count.InteractionsPerSession = Count.InteractionsPerSession / SessionDuration)
AG <- transform(AG, Count.TouchesPerSession = Count.TouchesPerSession / SessionDuration)
AG <- transform(AG, Count.MissedButtonPerSession = Count.MissedButtonPerSession / SessionDuration)
AG <- transform(AG, Count.ButtonTouchedPerSession = Count.ButtonTouchedPerSession / SessionDuration)
AG <- transform(AG, Count.CorrectButtonTouchedPerSession   = Count.CorrectButtonTouchedPerSession / SessionDuration)

Kus$source <- 'new'
Kus$source <- factor(Kus$source, levels = c("old","new"))
AG$Gender <- factor(AG$Gender, levels =c("m", "w"), labels=c("M","F"))
Kus$Gender <- factor(Kus$Gender, levels =c("M", "F"))
colnames(Kus)[109] <- "SessionDuration"
AG <- AG[-1]
Kus <- Kus[-c(110,111)]
colnames(Kus)[113] <- "neuro"
colnames(Kus)[114] <- "extra"
colnames(Kus)[115] <- "open"
colnames(Kus)[116] <- "agree"
colnames(Kus)[117] <- "con"


AG$Handedness <- factor(AG$Handedness, levels =c("LEFT", "RIGHT"))
Kus$Handedness <- factor(Kus$Handedness, levels =c("LEFT", "RIGHT"))
names(AG)[names(AG)=="Alter"] <- "Age"
AG$MeanPerSession.InteractionDuration <- NULL
AG$MeanPerSession.TaskDuration <- NULL
Kus$SessionNr <- NULL    
a <- names(AG)[!(names(Kus) %in% names(AG))]
a <- names(AG)[!(names(AG) %in% names(Kus))]

data <- rbind(AG,AG)
data <- rbind(data,data)
data <- rbind(data,data)
data <- data[1:652,]

#find indice of the matching column in Kus
for (i in 1:117) {
temp <- which(names(Kus)==names(AG)[i])
data[[i]] <- append(AG[[i]], Kus[[temp]], after = 124)
}

temp <- data[,50:117]
data$Condition <- as.factor(data$Condition)
write.csv(data, file="data_aggregated.csv")
