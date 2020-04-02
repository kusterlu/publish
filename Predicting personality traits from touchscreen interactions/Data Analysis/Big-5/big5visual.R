# Normalverteilung testen
library(nortest)
library(ggplot2)
ad.test(work$agree)


######### neuro

# create data frame so summarise values:
mien <- as.data.frame(matrix(NA,  nrow=2,  ncol=2))
colnames(mien) <- c("mean","Gender")
# factor(zusammen$Gender, levels = c("F","M"))
mien[1,1] <- 19.885 #men
mien[2,1] <- 23.2 #female
mien[1,2] <- "M"
mien[2,2] <- "F"

# Dichte darstellen mit Vertikaler Mittelwert Linie
# a
ggplot(work, aes(x=neuro)) + geom_density()  + ggtitle("Neuroticism Total") + xlab("neuroticism")
         
# b
# Density plots with means
ggplot(work, aes(x=neuro, colour=Gender)) + ggtitle("Neuroticism") + xlab("neuroticism") +
  geom_density() +
  geom_vline(data=mien, aes(xintercept=mean,  colour=Gender),
             linetype="dashed", size=1) 

######### extra

# create data frame so summarise values:
mien <- as.data.frame(matrix(NA,  nrow=2,  ncol=2))
colnames(mien) <- c("mean","Gender")
# factor(zusammen$Gender, levels = c("F","M"))
mien[1,1] <- 28.255 #men
mien[2,1] <- 29.13 #female
mien[1,2] <- "M"
mien[2,2] <- "F"

# Dichte darstellen mit Vertikaler Mittelwert Linie
# a
ggplot(work, aes(x=extra)) + geom_density()  + ggtitle("Extraversion Total") + xlab("extraversion")

# b
# Density plots with means
ggplot(work, aes(x=extra, colour=Gender)) + ggtitle("Extraversion") + xlab("extraversion") +
  geom_density() +
  geom_vline(data=mien, aes(xintercept=mean,  colour=Gender),
             linetype="dashed", size=1) 

######### open

# create data frame so summarise values:
mien <- as.data.frame(matrix(NA,  nrow=2,  ncol=2))
colnames(mien) <- c("mean","Gender")
# factor(zusammen$Gender, levels = c("F","M"))
mien[1,1] <- 32.37 #men
mien[2,1] <- 33.19 #female
mien[1,2] <- "M"
mien[2,2] <- "F"

# Dichte darstellen mit Vertikaler Mittelwert Linie
# a
ggplot(work, aes(x=open)) + geom_density()  + ggtitle("Openess Total") + xlab("openess")

# b
# Density plots with means
ggplot(work, aes(x=open, colour=Gender)) + ggtitle("Openess") + xlab("openess") +
  geom_density() +
  geom_vline(data=mien, aes(xintercept=mean,  colour=Gender),
             linetype="dashed", size=1) 

######### agree

# create data frame so summarise values:
mien <- as.data.frame(matrix(NA,  nrow=2,  ncol=2))
colnames(mien) <- c("mean","Gender")
# factor(zusammen$Gender, levels = c("F","M"))
mien[1,1] <- 28.98 #men
mien[2,1] <- 30.97 #female
mien[1,2] <- "M"
mien[2,2] <- "F"

# Dichte darstellen mit Vertikaler Mittelwert Linie
# a
ggplot(work, aes(x=agree)) + geom_density()  + ggtitle("Agreeableness Total") + xlab("agreeableness")

# b
# Density plots with means
ggplot(work, aes(x=agree, colour=Gender)) + ggtitle("Agreeableness") + xlab("agreeableness") +
  geom_density() +
  geom_vline(data=mien, aes(xintercept=mean,  colour=Gender),
             linetype="dashed", size=1) 

################# Conscientiousness

# create data frame so summarise values:
mien <- as.data.frame(matrix(NA,  nrow=2,  ncol=2))
colnames(mien) <- c("mean","Gender")
# factor(zusammen$Gender, levels = c("F","M"))
mien[1,1] <- 28.98 #men
mien[2,1] <- 30.97 #female
mien[1,2] <- "M"
mien[2,2] <- "F"

# Dichte darstellen mit Vertikaler Mittelwert Linie
# a
ggplot(work, aes(x=con)) + geom_density()  + ggtitle("Conscientiousness Total") + xlab("conscientiousness")

# b
# Density plots with means
ggplot(work, aes(x=con, colour=Gender)) + ggtitle("Conscientiousness") + xlab("conscientiousness") +
  geom_density() +
  geom_vline(data=mien, aes(xintercept=mean,  colour=Gender),
             linetype="dashed", size=1) 

#######################

#age 
ggplot(work, aes(x=Age)) + geom_histogram(binwidth=.5)
p <- ggplot(work, aes(class, hwy))

#gender
ggplot(data=work, aes(x=Gender)) +
  geom_bar(stat="count", fill="#266072") + ggtitle("Gender") 

#hand
ggplot(data=work, aes(x=Handedness)) +
  geom_bar(stat="count", fill="#18adde") + ggtitle("Handedness") 

######################
# calculate means

men <- work[which(work$Gender=="M"),]
men <- men[c(2,3,4,5,6)]
fem <- work[which(work$Gender=="F"),]
fem <- fem[c(2,3,4,5,6)]

colMeans(men)
colMeans(work[,c(2,3,4,5,6)])
