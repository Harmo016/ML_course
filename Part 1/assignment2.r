#Assignment 2
# Calculate ML(θ) where θ varies.
# harmo016

data <- read_excel("machines.xlsx")

#Part 1, calculate ML(x|θ). Plot ML(x|θ) for different values on θ. 
likelihood_calculator <- function(theta, xvector) {
  
  
  likelihoodvec = log(theta)-theta*xvector
  log_liklihood = sum(likelihoodvec)
  
  
  return(log_liklihood)
}


theta_vec = seq(from = 0,to = 10, by = 0.1)
# theta = 0 => l value -inf, but this is ignored so np.

likelihood_vec = numeric(length(theta_vec))

for (i in 1:length(likelihood_vec)) {
  likelihood_vec[i] =  likelihood_calculator(theta_vec[i],data)
}

best_theta = theta_vec[which(likelihood_vec==max(likelihood_vec))]
l_value_betst_theta = likelihood_vec[which(likelihood_vec==max(likelihood_vec))]
plot(x = theta_vec,y = likelihood_vec, col = "red", type = "l")
points(x = l_value_betst_theta, y = best_theta, col = "green", cex = 2)
text(x = l_value_betst_theta, y = best_theta, labels = round(l_value_betst_theta))

#Part 3, compare full observation set (48 obs) to a limited subset (6 obs).
data_limited <- data[c(1:6),]

likelihood_vec_limited = numeric(length(theta_vec))

for (i in 1:length(likelihood_vec_limited)) {
  likelihood_vec_limited[i] =  likelihood_calculator(theta_vec[i],data_limited)
}
plot(x = theta_vec,  y = likelihood_vec, col = "red", type = "l",ylim=c(-115,-2))
lines(x = theta_vec, y = likelihood_vec_limited, col ="green")
#plot(x = theta_vec, y = likelihood_vec_limited, col ="green", axes = FALSE, xlab = "", ylab = "", type = "l")
axis(4, col="red",col.axis="red",las=1)

#Part 3, calculate the ML-function assuming a bayesian model.


likelihood_calculator_bayesian <- function(theta, xvector) {
  
  lambda = 10
  likelihoodvec = log(theta)-theta*xvector + log(lambda) - lambda*xvector
  log_liklihood = sum(likelihoodvec)
  
  
  return(log_liklihood)
}


likelihood_vec_bayesian = numeric(length(theta_vec))

for (i in 1:length(likelihood_vec)) {
  likelihood_vec_bayesian[i] =  likelihood_calculator_bayesian(theta_vec[i],data)
}

plot(x = theta_vec, y = likelihood_vec, col = "red", ylim=c(-600,-2), type="l")
par(new = TRUE)
lines(x = theta_vec, y = likelihood_vec_bayesian, col ="green")
axis(4, col="red",col.axis="red",las=1)


#Part 5
plot(theta_vec,likelihood_vec)
#Reading the plot, the theta that maximize l seems to be around 1.3.
best_theta = theta_vec[which(likelihood_vec_bayesian==max(likelihood_vec_bayesian))]
#The actual best theta was 1.1

data_size = 50
set.seed(12345)
x = rexp(data_size,best_theta)
breaks = 5;
#generate scenarios using the exponential distribution.

genereatedLengths = x
dataGenerated <- data.frame(genereatedLengths)

hist(data$Length, breaks = breaks, col = "red")
hist(dataGenerated$genereatedLengths, breaks = breaks, col = "blue")
