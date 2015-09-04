library(e1071)
library(data.table)
setwd("~/R/data")
old.op <- options(max.print=999999)
meidai <- read.csv("Driving_data_list_20150418_editHashimoto.csv", header=FALSE)
feature <- as.matrix(meidai[1:70, 5:7])
answer <- as.logical(meidai$V14[1:70])
MdataMT <- cbind(answer, feature)
Mdata <- as.data.frame(MdataMT)
result <- svm(feature, answer, type="C-classification", cost=10, kernel="linear", scale=TRUE, trelance=1e-5)

#meidai.svm <- svm(meidai[1:70,14]~., data = meidai)
#—ñ‚ð‘I‘ð‚Å‚«‚È‚¢

#svm <- svm(~., data=meidai[feature,])


pred <- predict(result,feature)
pred <- fitted(result)
table(pred, answer)
pred <- predict(result, feature, decision.values = TRUE)
attr(pred, "decision.values")[1:2,] #?[1:2,]‚Å‚æ‚¢H

plot(result, Mdata, pred~.)
