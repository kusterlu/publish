#extract duration:
# load manually: InteractionDataStudie01.Rda; AggregatedFeaturesPerSession_cleaned;  

#create table to write in:
times <- data.experiment.interactions[1:543,c("sessionNr", "sessionDuration","user_id")]

a <- NULL
session.start <- 0
session.stop <- 0
for (i in 1:543) {
a <- data.experiment.interactions[data.experiment.interactions$sessionNr==i,]
session.start <- a$timestamp[1]
session.stop <- a$timestamp[length(a$timestamp)]
times$sessionDuration[i] <- difftime(session.stop, session.start, units="secs")
times$sessionNr[i] <- i
times$user_id[i] <- a$user_id[1]
}
rm(i, session.start, session.stop, a)

# inspect outliers
# times[times$sessionDuration<40,]
# times <- times[times$sessionDuration>40,]
# boxplot(times$sessionDuration)

# manual session alignment
times <- times[!times$user_id==216,]
times$sessionDuration[times$sessionNr==360] <- times$sessionDuration[times$sessionNr==360] + times$sessionDuration[times$sessionNr==361]
times$sessionDuration[times$sessionNr==363] <- times$sessionDuration[times$sessionNr==363] + times$sessionDuration[times$sessionNr==364]
times$sessionDuration[times$sessionNr==537] <- times$sessionDuration[times$sessionNr==537] + times$sessionDuration[times$sessionNr==538]
times$sessionDuration[times$sessionNr==495] <- times$sessionDuration[times$sessionNr==495] + times$sessionDuration[times$sessionNr==496]
times <- times[!(times$sessionNr %in% c(361,364,538,496)),]

#add do aggregation
colnames(times)[1] <- "SessionNr"
data.features.aggregated <- merge(data.features.aggregated, times, by="SessionNr")
# manually save it.