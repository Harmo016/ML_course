library(readxl)
data <- read_excel("spambase.xlsx")

n = dim(data)[1]
set.seed(12345)
id=sample(1:n,floor(n*0.5))
train=data[id,]
test=data[-id,]


logistic_r = glm(formula=Spam ~ ., data=train, family ="gaussian")
#logsitc_R > maps between 0-1 (good for probs. (given that u have 2 distinct classes)) 
#summary(logistic_r)


### classification rates on logistic regression model
pred_train = predict(logistic_r,train)
pred_test  = predict(logistic_r,test)


y_hat_train_0.5 = ifelse(pred_train>0.5,1,0)
y_hat_test_0.5  = ifelse(pred_test >0.5,1,0)

table(y_hat_train_0.5, train$Spam)
Misclassificationrate_traindata_0.5 = 1-sum(diag(table(y_hat_train_0.5, train$Spam)))/sum(table(y_hat_train_0.5, train$Spam))
table(y_hat_test_0.5, test$Spam)
Misclassificationrate_testdata_0.5 = 1-sum(diag(table(y_hat_test_0.5, test$Spam)))/sum(table(y_hat_test_0.5, test$Spam))

y_hat_train_0.8 = ifelse(pred_train>0.8,1,0)
Misclassificationrate_traindata_0.8 = 1-sum(diag(table(y_hat_train_0.8, train$Spam)))/sum(table(y_hat_train_0.8, train$Spam))
y_hat_test_0.8  = ifelse(pred_test >0.8,1,0)
Misclassificationrate_testdata_0.8 = 1-sum(diag(table(y_hat_test_0.8, test$Spam)))/sum(table(y_hat_test_0.8, test$Spam))

table(y_hat_train_0.8, train$Spam)
table(y_hat_test_0.8, test$Spam)


#K-nearest neighbour

library(kknn)
knn_r_k30_testdata = kknn(formula = Spam~.,train = train, k=30, test = test)
knn_r_k30_traindata = kknn(formula = Spam~.,train = train, k=30, test = train)
#fitted values = modellen som körts på test-data. Detta innebär test = test, kör på testdata. Test = train = kör på traindata.

pred_train_knn=ifelse(knn_r_k30_traindata$fitted.values>0.5,1,0)
table(pred_train_knn,train$Spam)
Misclassifcationrate_kkn_k30=1-sum(diag(table(pred_train_knn,train$Spam)))/sum(table(table(pred_train_knn,train$Spam)))

pred_test_knn=ifelse(knn_r_k30_testdata$fitted.values>0.5,1,0)
table(pred_test_knn,train$Spam)