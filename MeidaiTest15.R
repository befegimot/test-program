setwd("~/R/data")
library(e1071)
library(data.table)
old.op <- options(max.print=999999)

meidai <- read.csv("Driving_data_list_20150418_editHSMT.csv", header=FALSE)
feature <- as.matrix(meidai[1:500, 5:12])
answer <- as.factor(meidai$V14[1:500]) #transfor boolian
MdataMT <- cbind(feature, answer)
Mdata <- as.data.frame(MdataMT)
result <- svm(answer~., Mdata, type="C-classification", cost=1,
              kernel="radial", scale=TRUE, cross=12) #cross = "number of feature data?"
summary(result)

#�O���b�h�T�[�`
gammaRange = 10^(-5:2)
costRange = 10^(-1:4)
ti <- proc.time() #�v�Z�O�̎���
t <- tune.svm(feature, answer, gamma=gammaRange, cost=costRange,
              tunecontrol = tune.control(sampling="cross", cross=12))
proc.time()-ti#�v�Z��̎���-�v�Z�O�̎���=�v�Z����
cat("- best parameters:\n")
cat("gamma =", t$best.parameters$gamma, "; cost =", t$best.parameters$cost, ";\n")
cat("accuracy:", 100 - t$best.performance * 100, "%\n\n")
plot(t, transform.x=log10, transform.y=log10)
#�O���b�h�T�[�`�����܂�



pred <- predict(result,feature)
pred <- fitted(result)
table(pred, answer)
pred <- predict(result, feature, decision.values = TRUE)
attr(pred, "decision.values")[1:2,] #?[1:2,]�ł悢�H

plot(result, Mdata)