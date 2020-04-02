
sdf <- read.csv("data_aggregated.csv")
sdf <- sdf[,c(-1)] # remove wo useless variables.



# Conditionen, in normaler Icon Größe und bei Benutzung mit Haupt Hand:

# $Handedness==RIGHT und Condition == 1(table), 3(stand), 5(hold)
# $Handedness==LEFT und Condition==7(table), 9(stand), 11(hold)

sdf <- sdf[sdf$Handedness==2&sdf$Condition %in% c(1,3,5) | sdf$Handedness==1&sdf$Condition %in% c(7,9,11),]

# remove outliers, as describe in Data Analysis part. 
sdf <- sdf[-4,] 
sdf <- sdf[-58,] 
sdf <- sdf[-122,] 

temp <- sdf[,70:117]

library(ggplot2)
library(mlbench)
library(caret)

### dimensionality reduction
features<-sdf[,3:107] 
# calculate correlation matrix
correlationMatrix <- cor(features)
# find attributes that are highly corrected (ideally >0.75)
# Using exact = TRUE will cause the function to re-evaluate the average correlations at each step
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8, exact = TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# 64 features shall be removed.
features <- features[,-highlyCorrelated]
sdf <- cbind(sdf[,1:2],features, sdf[,108:117])
# remove dublicated rows - (why do they exist in the first place?)

# handle missing values in big5
sdf$extra[which(is.na(sdf$extra))] <- as.integer(mean(sdf[which(sdf$source==1),53], na.rm=TRUE))
sdf$open[which(is.na(sdf$open))] <- as.integer(mean(sdf[which(sdf$source==1),54], na.rm=TRUE))
sdf$agree[which(is.na(sdf$agree))] <- as.integer(mean(sdf[which(sdf$source==1),55], na.rm=TRUE))
sdf$con[which(is.na(sdf$con))] <- as.integer(mean(sdf[which(sdf$source==1),56], na.rm=TRUE))

sdf$Condition <- as.factor(sdf$Condition)
sdf$Handedness <- factor(sdf$Handedness, levels =c(1, 2), labels=c("LEFT","RIGHT"))
sdf$Gender <- factor(sdf$Gender, levels =c(1,2), labels=c("M","F"))
sdf$source <- factor(sdf$source, levels =c(1,2), labels=c("old","new"))


# inspect big-5 dimensions
persons <- sdf[,c(1,51:59)]
persons <- persons[!duplicated(persons),]
# loop that does simple plots of raw data, to inspect outliers.
for (i in 2:10) {
  plot(persons[[i]], xlab="VP", ylab=names(persons)[i], main=i)
  text(persons[[i]], row.names(persons[[i]]), cex=0.6, pos=4, col="blue")
  abline(h=mean(persons[which(persons$source==1),i]))
  abline(h=mean(persons[which(persons$source==2),i]))
  print(mean(persons[which(persons$source==1),i]))
  print(mean(persons[which(persons$source==2),i]))
}
# abline(v=3, col="purple") adds a vertical line top a graph

# loop that does simple plots of raw data, to inspect outliers.
for (i in 8:8) {
plot(sdf[[i]], xlab="VP", ylab=names(sdf)[i], main=i)
text(sdf[[i]], row.names(sdf[[i]]), cex=0.6, pos=4, col="blue")
abline(h=mean(sdf[which(sdf$source==1),i]))
abline(h=mean(sdf[which(sdf$source==2),i]))
}

# loop that creates boxplots for all variables, dependent on data-source
# calculates anova and return p-value on plot.
nm <- names(sdf)
k <- NULL
pe <- NULL
for(i in 3:60) {
fml <- as.formula(paste(names(sdf)[i], "~Condition"))
p <- as.character(summary(aov(fml, data = sdf))[[1]][["Pr(>F)"]])
  if (p<0.05) {
  print(ggplot(sdf, aes_string("Condition", names(sdf)[i])) + geom_boxplot() + 
  ggtitle(as.character(i)) +
  xlab(p))
  k <- append(k, names(sdf)[i])
  pe <- append(pe, p[1])
  }
}
# signifikant <- cbind(k,pe)
# write.csv(signifikant, file="anova-signifikanzen-bzgl-condition.csv")
# save(sdf, file="data_aggregated_ready.Rda")
