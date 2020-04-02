#### Setup
# A: set workign directory to Studie1
# B: manually: load("AggregatedFeaturesPerSessionStudie01_alt.Rda")

### Prepare Aggregated Features
AG <- data.features.aggregated
# Subset nach Bedingungen: #damit habe ich ca. 2x2 Minuten
# Von 372 obs. bleiben noch 62 über.
AG <- AG[which(AG$App=="Spell"),]    # $App=="Spell"
AG <- AG[which(AG$Cond=="Normal"|AG$Cond=="TinyIcons"),]



###Prepare Demographics
#
demo <- read.csv("FB-Demograph.csv", header = TRUE, sep=";")
# Rename a column in R
colnames(demo)[42] <- "neuro"
colnames(demo)[43] <- "extra"
colnames(demo)[44] <- "open"
colnames(demo)[45] <- "agree"
colnames(demo)[46] <- "con"
# convert to numerical
demo$neuro <- as.integer(demo$neuro)
demo$extra <- as.integer(demo$extra)
demo$open <- as.integer(demo$open)
demo$agree <- as.integer(demo$agree)
demo$con <- as.integer(demo$con)
ge <- demo[,c(1,6)]
demo <- demo[,c(1,5,42,43,44,45,46)]


# merge demographics and feature values, 
# and prepare variables so it can later be merged with Study2.
AG <- merge(AG, demo, by="VP")
# create source variable to later distinguish the two data sets.
AG$source <- 'old' 
AG$source <- factor(AG$source, levels = c("old","new"))
AG <- merge(AG, ge, by="VP")
# manually annotate handedness and conditions.
AG$Handedness <- "RIGHT" # nur VP20 ist links
AG$Handedness[69:72] <- "LEFT"
AG$Condition <- 1
AG$Condition[which(AG$Cond.App=="TinyIcons.Spell")] <- 2
AG$Condition[which(AG$VP=="20"&AG$Cond=="Normal")] <- 7
AG$Condition[which(AG$VP=="20"&AG$Cond=="TinyIcons")] <- 8
AG <- AG[,-(2:6)]         # $entferne nicht gebrauchte Spalten
colnames(AG)[117] <- "Gender"


write.csv(AG, file="AG.csv")


# #####Examine Ouliers
# # --------> wie kann man das automatisieren?
# # scewedness
# for (i in 81:108) {
# plot(AG[[i]], xlab="VP", ylab=names(AG)[i], main=i)
# text(AG[[i]], row.names(AG[[i]]), cex=0.6, pos=4, col="blue")
# }
# 
# 
# # Auffäligkeiten nach VP geordnet
# # VP   Variablen mit Auffäligkeiten
# # 18   31, 23,22,21,19,17,16,9, 
# # 30   109, 97, 82, 73, 37, 28
# # 2    108, 99, 90, 81, 72, 36
# # 50 hat schöne, nicht lineare Beziehung
# 
# boxplot(AG[[i]])
# # List of values which look too much like extreme outliers.
# AG[18,8] <- NA
# # wieder VP 18, AUffälligkeit mit Variablen AG[[i]] | i={8,9}


# #function which takes mean value for specifiv VP over all variables
# #
# mvalues <- function(i){
#   #subset von AG ziehen, für VP==i
#   temp <- AG[which(AG$VP==i),]
#   return(colMeans(temp))
# }
# # apply the function for all VP
# for(i in 3:33) {
#   AG[i-2,] <- mvalues(i)
# }
# AG <- AG[1:31,]


# #####Analysis
# 
# #ist diese Korrelation nun ein- oder beidseitig?
# 
# library(Hmisc) # --- https://rdrr.io/cran/Hmisc/man/rcorr.html
# # Pearson=1: alle Werte liegen auf einer Gerade (Linear)
# # je näher Wert r bei 0, so kleiner ist Zusammenhang. 
# #
# correlation_matrix <- rcorr(as.matrix(AG[1:114]), type="pearson")
# P_Values <- as.data.frame(correlation_matrix$P)
# Corr_Values <- as.data.frame(correlation_matrix$r)
# #
# # --- focus on neurotizismus => Spalte 3
# #
# Analysis <- cbind(P_Values[8:114,5,drop=FALSE],Corr_Values[8:114,5,drop=FALSE])
# colnames(Analysis) <- c("pWert", "Person Koeffizient r")
# min(Analysis$pWert)
# sig <- Analysis[Analysis$pWert<0.1,]
# reihenfolge <- order(sig$pWert)
# sig <- sig[reihenfolge,]



# ### examine linear Modelling with detected significant values.
# 
# for (a in 1:length(sig$pWert)) {
#   
# i <- which( colnames(AG)==rownames(sig)[a] )
# #
# # hier die Abhängige ändern!
# # 
# Modell <- lm(open ~ AG[[i]], data=AG)
# koeff.Modell <- coef(Modell)
# plot(open ~ AG[[i]], data=AG)
# abline(a=koeff.Modell[[1]], b=koeff.Modell[2])
# Modell.Ueberblick <- summary(Modell)
# Modell.Ueberblick
# sig[names(AG)[i],"lm.coef"] <- koeff.Modell[2]
# sig[names(AG)[i],"lm.intercept"] <- koeff.Modell[1]
# }
# 
# # write.csv(sig, file="Signifikanztabelle_Con") #data-frame als Datei speichern.
# rm(sig)
# 
# # First Correlogram Example 
# #library(corrgram)
# #corrgram(AG, order=TRUE, lower.panel=panel.shade,
# #         upper.panel=panel.pie, text.panel=panel.txt,
# #         main="Alle Daten auf einmal")
