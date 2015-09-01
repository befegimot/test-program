library(e1071)
library(data.table)
setwd("~/R/data")
old.op <- options(max.print=999999)
meidai <- read.csv("Driving_data_list_20150418_editHashimoto.csv", header=FALSE)
feature <- as.matrix(meidai[1:70, 5:13])
answer <- as.numeric(meidai$V14[1:70])
result <- svm(feature, answer, type="C-classification", cost=10, kernel="linear", scale=TRUE, trelance=1e-5)

#meidai.svm <- svm(meidai[1:70,14]~., data = meidai)
#—ñ‚ð‘I‘ð‚Å‚«‚È‚¢

#svm <- svm(~., data=meidai[feature,])

x <- subset(answer, select = -V1)
y <- meidai$V14[1:70]
model <- svm(x, y)
pred <- predict(result,feature)
pred <- fitted(result)
table(pred, answer)
