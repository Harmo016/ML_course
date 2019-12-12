data <- read_excel("tecator.xlsx")

plot(data$Moisture,data$Protein)
# seems to be a linear relation where Protein(moisture)
# N(p(protein),sigma²)
# minimse MSE => maximising the likelihood function given normal dist.

n = dim(data)[1]
set.seed(12345)
id=sample(1:n,floor(n*0.5))
train=data[id,]
test=data[-id,]

M = list()
MSE_testset = numeric(6)
MSE_trainset = numeric(6)
M[[1]] = lm(Moisture ~ Protein, data = train)
M[[2]] = lm(Moisture ~ Protein + I((Protein)^2), data = train)

#Generating model M1 through M6 for training
for (i in 3:6){
  M[[i]] = lm(Moisture ~ poly(Protein,i),data=train)
}

#Calculating the Mean Square Error between model predictions and actual values.
#Done both on the test set and train set.

for (i in 1:6){
  predictions = predict(M[[i]],test)
  MSE_testset[i] = mean((test$Moisture-predictions)^2)

  
  predictions = predict(M[[i]],train)
  MSE_trainset[i] = mean((train$Moisture-predictions)^2)
}


plot(MSE_trainset,typ="l",col="red")
par(new = TRUE)
plot(MSE_testset, type = "l", col = "green", axes = "FALSE", ylab = "")

# Model 3.

data <- read_excel("tecator.xlsx")

library(MASS)

data_subset = as.data.frame(data[2:102])
#Extracting the subset where the colnames are Channels.


full_model = glm(formula=data_subset$Fat ~ ., data=data_subset, family ="gaussian")

Model_after_stepAIC = stepAIC(full_model)


predictors = Model_after_stepAIC$model[,grep("Channel", colnames(Model_after_stepAIC$model))]
cat("Number of variables selected: ", length(predictors))


predictors = cbind(data$Fat, predictors)
#Extract all columns where Channel occurs in column name.






lambda_vec = seq(0,0.015,0.001)

ridge_model = lm.ridge(model = predictors$`data$Fat`~ ., data = predictors, lambda = lambda_vec)
#Creating the ridge regression model.


plot(ridge_model, xlim = c(-2, -0.5), ylim = c(-5, 5))

library(glmnet)
#Package to run lasso.

x_values = matrix(as.numeric(unlist(predictors[-1])),nrow = 215,byrow=FALSE)

#x_values_test = data.frame(matrix(unlist(x_values),nrow=215,byrow=FALSE),stringsAsFactors = FALSE)

#lasso_model = glm(formula = predictors2$`data$Fat`~ ., data = predictors2, lambda = lambda_vec)


lambda_vec = seq(0,1,0.05)
lambda_vec = rev(lambda_vec)

lasso_model = glm(y= predictors$`data$Fat`, x = x_values, lambda = lambda_vec)
# alfa = 1 in deafult, resulting on a lasso penalty.


plot(lasso_model)
#lasso_model = glmnet(y=y_values, x =x_values, lambda = lambda_vec)
#We notice that as the penalty increases, the coefficent increases in order match the increasing penalty.



