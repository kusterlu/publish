# all for the example of  extraversion

# load output of DataAnalysis.R
# set sowrking-directory to some empty folder (script produces lots of output)


library(mlbench)
library(caret)

################################ A: extraversion

# create data frame so summarise values:
zusammen <- as.data.frame(matrix(NA,  nrow=5,  ncol=6))
rownames(zusammen) <- c("SVM","KNN","RF","NB","GLM")
colnames(zusammen) <- c("median","pvalue","Sig","conf.start","conf.end","testset")

# prepare target class
target <- sdf$extra
a <- median(target)
print(paste("# unter Median: ",length(target[target<a]),
            "| # gleich Median: ",length(target[target==a]),
            "| # über Median: ",length(target[!target<a])))
target[target>a] <- 1
target[!target==1] <- 0
print(paste("# 0=",length(target[target==0]),"# 1=",length(target[target==1])))

sdf$Condition <- as.numeric(sdf$Condition)
sdf$Handedness <- as.numeric(sdf$Handedness)
sdf$Gender <- as.numeric(sdf$Gender)
sdf$source <- as.numeric(sdf$source)

features<-sdf[,3:50]
features <- cbind(features, target)

set.seed(3033)
# split data into test and training set. test-set should not be standardized. 
# P=percentage of split.
intrain <- createDataPartition(y = target, p= 0.9, list = FALSE)
training <- features[intrain,]
testing <- features[-intrain,]
# intrain <- createDataPartition(y = target, p= 0.8, list = FALSE)
training$target <- factor(training$target, levels=c(0,1), labels=c("niedrig","hoch"))
testing$target <- factor(testing$target, levels=c(0,1), labels=c("niedrig","hoch"))
# check if any missing values exist
anyNA(features)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

############## A-1 extra-svm

# create Cluster, usign repeated cross validation.
# use two times the number of cores in the computer. Intel i5 has 2 physical cores. 
library(doSNOW)
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
# RBF Kernel and tuning of gamma and C.
set.seed(3033)
grid_radial <- expand.grid(sigma = c(0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2,5))
svm_Radial_Grid <- train(target ~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)
#Shutdown cluster
stopCluster(cl)

png(filename="extra-svm.png")
plot(svm_Radial_Grid)
dev.off()
test_pred_grid <- predict(svm_Radial_Grid, newdata = testing)
importance <- varImp(svm_Radial_Grid, scale=FALSE)

zusammen["SVM","testset"] <- confusionMatrix(test_pred_grid, testing$target)$overall[1]
sink("extra-svm.txt", append = TRUE)
svm_Radial_Grid
print(importance)
sink()




################ A-2 extra knn

library(doSNOW)
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
set.seed(3033)
knn_fit <- train(target ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
stopCluster(cl)

png(filename="extra-knn.png")
plot(knn_fit)
dev.off()
test_pred_grid <- predict(knn_fit, newdata = testing)
importance <- varImp(knn_fit, scale=FALSE)

zusammen["KNN","testset"] <- confusionMatrix(test_pred_grid, testing$target)$overall[1]
sink("extra-knn.txt", append = TRUE)
knn_fit
print(importance)
sink()



###################  A-3 rf

library(doSNOW)
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
set.seed(3033)

# run a simple random-forest without rfe:
sink("extra-rf.txt", append = TRUE)
print("- Simple rf start - ")
t <- Sys.time()
sink()
rf_fit <- train(target ~., data = training, method = "rf",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)

png(filename="extra-rf_simple.png")
plot(rf_fit)
dev.off()
test_pred_grid <- predict(rf_fit, newdata = testing)
importance <- varImp(rf_fit, scale=FALSE)

zusammen["RF","testset"] <- confusionMatrix(test_pred_grid, testing$target)$overall[1]
sink("extra-rf.txt", append = TRUE)
print("- Simple rf end - ")
Sys.time()-t
rf_fit
print(importance)
sink()



###################  A-4 extra nb native-bayes
sink("extra-nb.txt", append = TRUE)
print("- start rf2 - ")
t <- Sys.time()
sink()
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
set.seed(3033)

nb <- train(target ~., data = training, method = "nb",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)
stopCluster(cl)

png(filename="extra-nb.png")
plot(nb)
dev.off()
test_pred_grid <- predict(nb, newdata = testing)
importance <- varImp(nb, scale=FALSE)

zusammen["NB","testset"] <- confusionMatrix(test_pred_grid, testing$target)$overall[1]
sink("extra-nb.txt", append = TRUE)
print("- rf2 end - ")
Sys.time() - t
nb
print(importance)
sink()

#################### A-5 extra glm


cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
set.seed(3033)
glmModel <- train(target ~., data = training, method = "glm",
                                   trControl=trctrl,
                                   preProcess = c("center", "scale"),
                                   tuneLength = 10)
stopCluster(cl)

test_pred_grid <- predict(glmModel, newdata = testing)
importance <- varImp(glmModel, scale=FALSE)

zusammen["GLM","testset"] <- confusionMatrix(test_pred_grid, testing$target)$overall[1]
sink("extra-glmModel.txt", append = TRUE)
print("- glmModel end - ")
Sys.time()-t
glmModel
print(importance)
sink()


############### A6 comparing variable-importance

results <- resamples(list(SVM=svm_Radial_Grid, KNN=knn_fit, RF=rf_fit, NB=nb, GLM=glmModel))

png(filename="extra-Results-Boxplot.png")
bwplot(results)
dev.off()

png(filename="extra-Results-Dotplot.png")
dotplot(results, metric = "Accuracy")
dev.off()

sink("extra-Results.txt", append = TRUE)
summary(results)
sink()

for (i in 1:5) {
a <- as.data.frame(results$values)
# pWert beweist, dass mit alpha=0,05 mean verschieden von 0,5 (NIR).
t <- t.test(a[i*2] -0.5)
# ziehe dir mean und confidenz Intervall
zusammen[i,"pvalue"] <- t$p.value
if(t$p.value<0.05) {zusammen[i,"Sig"]<-TRUE}
t <- t.test(a[i*2])
zusammen[i,"median"] <- t$estimate
zusammen[i,"conf.start"] <- t$conf.int[1]
zusammen[i,"conf.end"] <- t$conf.int[2]
}
write.csv(zusammen, file="extra.csv")

