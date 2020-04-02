setwd("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics")
load("Agency_Cleaned.Rda")

library(psych) 
library(questionr) 
library(sjmisc) 
library(sjPlot)
library(snakecase)
library(plyr)
library(dplyr)
library(ggplot2)
library(nlme)
library(data.table)

# praktisch: tab_model(cpmodel, show.se = TRUE, show.stat = TRUE, digits = 2)
um.fit <- lme(fixed= SoA ~ 1, 
              random= ~ 1|PID, 
              data=Agency,
              na.action=na.exclude)

summary(um.fit)
VarCorr(um.fit)
RandomEffects <- as.numeric(VarCorr(um.fit)[,1])
#We can then compute the intra-class correlation (ICC) as the ratio of the random intercept
# variance (between-person) to the total variance (between + within).
RandomEffects
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_between

# MLM EXPERIMENT 0: SoA modelled by arouasl and time
#
# Easy MEANS per person.
# source: https://www.guru99.com/r-aggregate-function.html
#
a <- Agency
a$centered_arousal <- Agency$Arousal-(mean(Agency$Arousal))
Mittelwerte <- a %>% group_by(PID) %>% summarise(trait_arousal=mean(centered_arousal,  na.rm = TRUE),
                                            mean_mood=mean(MoodValence,  na.rm = TRUE))
a <- merge(a, Mittelwerte, by="PID")
#calculate "state" scores
a$state_arousal <- a$centered_arousal-a$trait_arousal
#
# FIT MODEL WITH FIXED AND RANDOM ELEMENTS
#
model2.fit <- lme(fixed= SoA ~ 1 + TIME + state_arousal + trait_arousal, 
                  random= ~ 1 + state_arousal|PID, 
                  data=a,
                  na.action=na.exclude)

VarCorr(model2.fit)
RandomEffects <- as.numeric(VarCorr(model2.fit)[,1])
RandomEffects
ICC_between <- RandomEffects[3]/(RandomEffects[1]+RandomEffects[2]+RandomEffects[3]) 
ICC_between

#sink("MLM V2 - SoA and Arousal.txt", append=TRUE, split=TRUE) 
#summary(model2.fit)
#dev.off()

# Depict the Effect of Arousal
#
ggplot(data=a, aes(x=Arousal, y=SoA, group=PID, color="gray")) + 
  #geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="hotpink") +
  xlab("Arousal State") + xlim(2,4) +
  ylab("Sense of Agency")  + ylim(2,6) +
  ggtitle("Within-person Associations")

VarCorr(model2.fit)
RandomEffects <- as.numeric(VarCorr(model2.fit)[,1])
#We can then compute the intra-class correlation (ICC) as the ratio of the random intercept
# variance (between-person) to the total variance (between + within).
RandomEffects
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_between
#### Es ist mir noch nicht ganz klar, was dann mit dem "Random-Effekt" anzufangen.
#### Wie viel Varianz wird wovon erklärt?
cfs <- ranef(model2.fit)

#intercept: average person in an average state, the SoA is ... coefficient
#state coefficient: average within person association. In situations with higher arousal ...
#... the prototypical person will feel a ... higher SoA.
#mean: (trait): individuals with one unit higher arousal have "mean_coefficient" higher SoA




### MLM Experiment 1
#
#
### SETUP OF "App Benutzung, wenn man on-track ist, oder nicht "on-track ist."
## subset a data set. 
#
# include only those cases, where "Goal orientation" was questioned, and only those that where
# 'on-track|overperform' or 'behind' (exlude the 170 NAs)
abc <- Agency
frq(abc, Goal, out="v")
abc$Goal[which(abc$Goal=="Overperform")] <- "OnTrack"
abc <- abc[which(abc$Goal=="OnTrack"|abc$Goal=="Behind"),] #we could also include "indifferent" here

abc$App <- 0
abc$App[which(abc$TimeLag<0.5)] <- 1 #das Abgrenzungsmaß war eine halbe Stunde. Super eng.
abc$App <- factor(abc$App, levels = c("0", "1"), labels=c("pocket","used"))
frq(abc, App, out="v")

# exclude people with less than 4 sessinos from the abc subsample
exclude <- c(71519150730,
             71519053509,
             71519061858,
             71519073952,
             71619053425,
             71619055344,
             71619060527)
exclude <- which(abc$PID %in% exclude)
abc <- abc[-exclude,]


abc$Goal <- droplevels(abc$Goal)
#62 Observations fulfill the criterion; Which includes 20 participants
describe(abc$Goal)
describe(abc$App)
# to summarise: Goal is a situational variable, where people had engaged with their tracking
# app less than 2h ago, and the "parameter they were tracking was EXTREME in the sense
# that it was not indifferent, but either GOOD or BAD. 

ggplot(data = abc, aes(x=Goal, y=SoA, fill=App)) + geom_boxplot()
# From the plot it does not look like there is any meaningful difference
# its even counterintuitive: When I am behind on my domain and I take the app,
# my SoA goes up.

describe(abc$Goal)
describe(abc$TrackingIntensity)
ggplot(data = abc, aes(x=Goal, y=SoA, fill=TrackingIntensity)) + geom_boxplot()
ggplot(data = abc, aes(x=TrackingIntensity, y=SoA, fill=Goal)) + geom_boxplot()
mimi <- lm(SoA ~ Goal + TrackingIntensity + Goal*TrackingIntensity, data = abc)
tab_model(mimi, show.se = TRUE, show.stat = TRUE, digits = 2)

###  TEST normal LM für abc subset bzgl CAT
#
frq(abc, CAT, out="v")
ggplot(data = abc, aes(x=Goal, y=SoA, fill=CAT)) + geom_boxplot()
ggplot(data = abc, aes(x=CAT, y=SoA, fill=Goal)) + geom_boxplot()
catcat <- lm(SoA ~ Goal + CAT + Goal*CAT, data = abc)
tab_model(catcat, show.se = TRUE, show.stat = TRUE, digits = 2)

###  TEST normal LM für abc subset bzgl TMD
#
frq(abc, TMD, out="v")
ggplot(data = abc, aes(x=Goal, y=SoA, fill=TMD)) + geom_boxplot()
tomtom <- lm(SoA ~ Goal + TMD + Goal*TMD, data = abc)
tab_model(tomtom, show.se = TRUE, show.stat = TRUE, digits = 2)

### SPLIT UP GOAL
#
#
# the most complex rescaling is required for the situational binary variable. (Here:Goal)
# we create separate within and between subject versions of the variable (Here Goal)
# 1: We substract the grand-mean accross all subjects and time-points from the raw score (of Goal)
abc$Goal <- as.numeric(abc$Goal)-1
# now meanins are: 0-onTrack 1-behind
abc$Goal_centered <- abc$Goal-(mean(abc$Goal)) #mean is 0.5 because by chance both groups are exactly equally large
a <- abc %>% group_by(PID) %>% summarise(GoalTrait=mean(Goal,  na.rm = TRUE))
abc <- merge(abc, a, by="PID")
rm(a)
abc$GoalState <- abc$Goal_centered-abc$GoalTrait

abc$Arousal_centered <- abc$Arousal-(mean(abc$Arousal))


### Test Lm on split up goal (the [centred] value minus the personal mean)
# Do a regression only with the "state" bit. 
#
#
mimi2 <- lm(SoA ~ GoalState + TrackingIntensity + GoalState*TrackingIntensity, data = abc)
tab_model(mimi2, show.se = TRUE, show.stat = TRUE, digits = 2)
#
catcat2 <- lm(SoA ~ GoalState + CAT + GoalState*CAT, data = abc)
tab_model(catcat2, show.se = TRUE, show.stat = TRUE, digits = 2)
#
tomtom2 <- lm(SoA ~ GoalState + TMD + GoalState*TMD, data = abc)
tab_model(tomtom2, show.se = TRUE, show.stat = TRUE, digits = 2)


### SPLIT UP APP
#
#
abc$App <- as.numeric(abc$App)-1
# now meanins are: 0-pocket 1-use
abc$App_centered <- abc$App-(mean(abc$App)) #mean is 0.5 because by chance both groups are exactly equally large
a <- abc %>% group_by(PID) %>% summarise(App_IndivMean=mean(App,  na.rm = TRUE))
abc <- merge(abc, a, by="PID")
rm(a)
abc$App_Random <- abc$App_centered-abc$App_IndivMean



### NOT SO MEANINGFUL
#
#Fit a nice lme with Goal(-0.5:OnTrack, 0.5:Behind) and Use
# incredibly complicated to analyse.
ctrl <- lmeControl(opt='optim')
FirstModel <- lme(fixed=SoA ~ Arousal_centered 
                  + App_IndivMean*GoalState
                  + App_IndivMean*GoalTrait
                  + App_Random*GoalTrait
                  + App_Random*GoalState,
                  control=ctrl,
                  data=abc, random=~App_Random + GoalState | PID, correlation = corAR1())

###RELEVANT
#
#Fit a nice lme with Goal(-0.5:OnTrack, 0.5:Behind) and Use
ctrl <- lmeControl(opt='optim')
SecondModel <- lme(fixed=SoA ~ Arousal_centered 
                  + App_IndivMean*Goal
                  + App_Random*Goal,
                  control=ctrl,
                  data=abc, random=~App_Random | PID, correlation = corAR1())
#sink("Summary V2 MLM SoA - Goal AppUsage.txt", append=TRUE, split=TRUE) 
#summary(SecondModel)
#dev.off()

#
VarCorr(SecondModel)
RandomEffects <- as.numeric(VarCorr(FirstModel)[,1])
#We can then compute the intra-class correlation (ICC) as the ratio of the random intercept
# variance (between-person) to the total variance (between + within).
RandomEffects
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_between


### FIT MODEL 3, DIFFERENCES IN HOW TRACKERS PERCEIVE THE WORLD
#
#
describe(abc$Goal)
describe(abc$TrackingIntensity)
#
ctrl <- lmeControl(opt='optim')
ThirdModel <- lme(fixed=SoA ~ Arousal_centered 
                   + GoalState*TrackingIntensity
                   + GoalTrait*TrackingIntensity,
                   control=ctrl,
                   data=abc, random=~GoalTrait | PID, correlation = corAR1())
summary(ThirdModel)


### FIT MODEL 4, How does being involved in the action, or in some other action,
# interact with goal orientation
#
describe(abc$Goal)
describe(abc$CAT)
#
# Goal, das hier aufgesplittet wurde, heißt dass sich jemand als "trait" öfter 
# "behind" oder "on-top" fühlt (in welcher % bist du "behind" on your goal?)
# state heißt dann: Die Auwirkung öfter mal Behind zu sein auf dein SoA
#
# CAT: Der oben erwähnte Effekt je nachdem, ob man an die Aktion gedacht hat oder nicht.
#
ctrl <- lmeControl(opt='optim')
FourthModel <- lme(fixed=SoA ~ GoalState*CAT
                  + GoalTrait*CAT,
                  control=ctrl,
                  data=abc, random=~GoalTrait | PID, correlation = corAR1())
summary(FourthModel)
tab_model(FourthModel, show.se = TRUE, show.stat = TRUE, digits = 2)

VarCorr(FourthModel)
Fourth_cfs<-ranef(FourthModel)
