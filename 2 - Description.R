setwd("C:/Users/Ludwig/OneDrive - London School of Economics/LSE/Dissertation/Data Analysis/Raw Data Qualtrics")
load("DeAgency.Rda")
load("AgencyEN.Rda")
Agency <- rbind(AgencyEN, DeAgency)
rm(DeAgency, AgencyEN)

# reshuffle so time variables are in front
Agency <- Agency[c(18:20,22, 1,21,6,14,15,2:5,7:12,16:17,24:26,28,23,27,13,29)]
Agency <- Agency[with(Agency, order(PID, DAY, SIG)),]


## Item analysis
# Correlation Matrix
#
# HEATMAP FOR AGENCY
#
corS1 <- round(cor(Agency[c(14:17,19)], use="pairwise"),2)
# Die Namen stimmen 

library(reshape2)
melted_corS1 <- melt(corS1)
library(ggplot2)
ggplot(data = melted_corS1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get upper triangle of the correlation matrix
get_upper_tri <- function(corS1){
  corS1[lower.tri(corS1)]<- NA
  return(corS1)
}
upper_tri <- get_upper_tri(corS1)
upper_tri

ggplot(data = melted_corS1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_corS1 <- function(corS1){
  # Use correlation between variables as distance
  dd <- as.dist((1-corS1)/2)
  hc <- hclust(dd)
  corS1 <-corS1[hc$order, hc$order]
}

corS1 <- reorder_corS1(corS1)
upper_tri <- get_upper_tri(corS1)
# Melt the correlation matrix
melted_corS1 <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_corS1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
# Print the heatmap
print(ggheatmap)
rm(corS1, ggheatmap, melted_corS1, upper_tri)
#
#
#
# Description
#
#
library(psych)
a <- alpha(Agency[14:17], check.keys=TRUE)
sink("Crombachs-Alpha.txt", append=TRUE, split=TRUE) 
#pdf(file="Crombachs-alpha.pdf", width=14, height=3)
a
dev.off()


library(ggplot2)

ksv <- ks.test(Agency$Q1[which(Agency$Source=="German")],
               Agency$Q1[which(Agency$Source=="English")])
jpeg("Q1.jpg", width = 400, height = 300)
ggplot(Agency, aes(x=Q1, fill=Source)) + 
       geom_histogram(binwidth=.5, position="dodge") + labs(title = "Q1", 
            caption = paste("Two-sample KS-test p-value:", 
                            round(ksv$p.value, 4) ))
dev.off()

ksv <- ks.test(Agency$Q2[which(Agency$Source=="German")],
               Agency$Q2[which(Agency$Source=="English")])
jpeg("Q2.jpg", width = 400, height = 300)
plot(ggplot(Agency, aes(x=Q2, fill=Source)) + 
       geom_histogram(binwidth=.5, position="dodge")
     + labs(title = "Q2", 
            caption = paste("Two-sample KS-test p-value:", 
                            round(ksv$p.value, 4) )))
dev.off()


ksv <- ks.test(Agency$Q3[which(Agency$Source=="German")],
               Agency$Q3[which(Agency$Source=="English")])
jpeg("Q3.jpg", width = 400, height = 300)
plot(ggplot(Agency, aes(x=Q3, fill=Source)) + 
       geom_histogram(binwidth=.5, position="dodge")
     + labs(title = "Q3", 
            caption = paste("Two-sample KS-test p-value:", 
                            round(ksv$p.value, 4) )))
dev.off()

ksv <- ks.test(Agency$Q4[which(Agency$Source=="German")],
               Agency$Q4[which(Agency$Source=="English")])
jpeg("Q4.jpg", width = 400, height = 300)
plot(ggplot(Agency, aes(x=Q4, fill=Source)) + 
       geom_histogram(binwidth=.5, position="dodge")
     + labs(title = "Q4", 
            caption = paste("Two-sample KS-test p-value:", 
                            round(ksv$p.value, 4) )))
dev.off()

jpeg("Q5.jpg", width = 400, height = 300)
ksv <- ks.test(Agency$Q5[which(Agency$Source=="German")],
               Agency$Q5[which(Agency$Source=="English")])
plot(ggplot(Agency, aes(x=Q5, fill=Source)) + 
       geom_histogram(binwidth=.5, position="dodge")
     + labs(title = "Q5", 
            caption = paste("Two-sample KS-test p-value:", 
                            round(ksv$p.value, 4) )))
dev.off()
rm(ksv)

#
#
###