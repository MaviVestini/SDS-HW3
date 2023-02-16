
# reproducibility
set.seed(262)
# Simulation size


# 3 -----------------------------------------------------------------------

n0 <- 35
n1 <- 40
k <- 3
alpha <- 0.01
N <- 1000


# first attempt, same distribution: normal with mean 0 and sd 1
c(1,3,0.2)
?diag
diag(c(0.4,2,1))

x <- mvrnorm(n0, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
z <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1)) 



# First thing to do is train the model 

# TODO add names: dimnames = c('Kolmogorov-Smirnov', 'Mann-Whitney')
stats <- rep(NA, N)

for(i in (1:N)){
  
  z_t <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
    
  # First thing to do is train the model
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u))
  
  stats[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
}


hist(stats)
abline(v = quantile(stats, 1 - alpha), col = 'blue')

# check


result <- rep(NA, N)

for(i in (1:N)){
  x <- mvrnorm(n0, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  z <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1))
  
  model <- glm(Label ~ ., data = as.data.frame(u))
  score <- predict(model, as.data.frame(u))
  result[i] <- 1*(ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic<quantile(stats, 1 - alpha))
}


barplot(prop.table(table(result)))


# TODO Funzione della distanza Wasserstein


# Get the results for the true data
coefs <- glm(Label ~ ., data = data)$coefficients 
ps <- apply(p, 1, sigmoid, theta = coefs) 
qs <- apply(q, 1, sigmoid, theta = coefs) 

ks_true <- ks.test(ps, qs, alternative = "two.sided")$statistic
mann_true <- wilcox.test(ps, qs, alternative = "two.sided")$statistic


hist(stats[,1])
abline(v = ks_true, col = 'orchid')
abline(v = quantile(stats[,1], 1 - alpha), col = 'blue')

hist(stats[,2])
abline(v = mann_true, col = 'orchid')
abline(v = quantile(stats[,2], 1 - alpha), col = 'blue')


# Now let's check the size and power

# For the size we will have to check how many times the hypothesis is rejected 
# under H0, so using the same distribution

results <- matrix(NA, nrow = N, ncol = 2)

for(i in (1:N)){
  p <- data.frame(cbind(matrix(rnorm(n0*k), nrow = n0), 0))
  q <- data.frame(cbind(matrix(rnorm(n1*k), nrow = n1), 1))
  
  colnames(p) <- labels
  colnames(q) <- labels
  
  data <- rbind(p, q)
  
  # Get the results for the true data
  coefs <- glm(Label ~ ., data = data)$coefficients 
  ps <- apply(p, 1, sigmoid, theta = coefs) 
  qs <- apply(q, 1, sigmoid, theta = coefs) 
  
  
  ks_true <- ks.test(ps, qs, alternative = "two.sided")$statistic
  mann_true <- wilcox.test(ps, qs, alternative = "two.sided")$statistic
  
  results[i,1] <- 1*(ks_true < quantile(stats[,1], 1-alpha))
  results[i,2] <- 1*(mann_true < quantile(stats[,2], 1-alpha))
}

barplot(table(results[,1]))
barplot(table(results[,2]))


# For the size we will have to check how many times the hypothesis is rejected 
# under H1, so using two different distributions. 
# And that is going to be 1 - the power

results <- matrix(NA, nrow = N, ncol = 2)

for(i in (1:N)){
  p <- data.frame(cbind(matrix(rnorm(n0*k), nrow = n0), 0))
  q <- data.frame(cbind(matrix(rnorm(n1*k, 10, 2), nrow = n1), 1))
  
  colnames(p) <- labels
  colnames(q) <- labels
  
  data <- rbind(p, q)
  
  # Get the results for the true data
  coefs <- glm(Label ~ ., data = data)$coefficients 
  ps <- apply(p, 1, sigmoid, theta = coefs) 
  qs <- apply(q, 1, sigmoid, theta = coefs) 
  
  
  ks_true <- ks.test(ps, qs, alternative = "two.sided")$statistic
  mann_true <- wilcox.test(ps, qs, alternative = "two.sided")$statistic
  
  results[i,1] <- 1*(ks_true < quantile(stats[,1], 1-alpha))
  results[i,2] <- 1*(mann_true < quantile(stats[,2], 1-alpha))
}

barplot(table(results[,1]))
barplot(table(results[,2]))






# Second attempt, different distributions: 
# Normal with mean 0 and sd 1
# Gamma 
?rgamma
p <- data.frame(cbind(matrix(rnorm(n0*k), nrow = n0), 0))
q <- data.frame(cbind(matrix(rgamma(n1*k, 5, 8), nrow = n1), 1))

colnames(p) <- labels
colnames(q) <- labels

data <- rbind(p, q)

# Friedman's procedure

# We are going to use the logistic regression classifier
# So we will need the sigmoid function

sigmoid <- function(x, theta){
  l <-  length(theta)
  aux <- exp(theta[1] + sum(x*theta[2:l]))
  
  return(aux/(aux+1))
}



?ks.test
?chisq.test # Non usato abbiamo bisogno che i sample siano della stessa dimensione
?wilcox.test

?colnames
?data.frame
?seq
?matrix
# 4 -----------------------------------------------------------------------





# test

model <- glm(Label ~ ., data = as.data.frame(u))

results <- matrix(NA, nrow = P, ncol = 2)

for(i in (1:P)){
  p <- mvrnorm(n0, mu = rep(0, k), Sigma = diag(k))
  q <- mvrnorm(n1, mu = rep(0, k), Sigma = diag(k))
  
  data <- rbind(cbind(p, Label = 0), cbind(q, Label = 1))  
  
  # Get the results for the true data
  #coefs <- glm(Label ~ ., data = as.data.frame(data))
  score <- predict(model, as.data.frame(data))
  
  
  ks_true <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
  #mann_true <- wilcox.test(ps, qs, alternative = "two.sided")$statistic
  
  #results[i,1] <- 1*(ks_true > quantile(t_l, alpha/2) & ks_true < quantile(t_l, 1 - (alpha/2)) )
  results[i,1] <- 1*(ks_true < quantile(t_l, 1 - (alpha))) #& ks_true < quantile(t_l, 1 - (alpha/2)) )
  results[i, 2] <- ks_true
  abline(v = ks_true, col = 'red')
  #results[i,2] <- 1*(mann_true < quantile(stats[,2], 1-alpha))
}

barplot(table(results[,1]))


# H1



hist(t_l, prob = T)  
abline(v = quantile(t_l, 1 - (alpha/2))) 
abline(v = quantile(t_l, alpha/2)) 


model <- glm(Label ~ ., data = as.data.frame(u))

for(i in (1:P)){
  p <- mvrnorm(n0, mu = rep(0, k), Sigma = diag(k))
  q <- mvrnorm(n1, mu = rep(0.5, k), Sigma = diag(k))
  
  data <- rbind(cbind(p, Label = 0), cbind(q, Label = 1))  
  
  # Get the results for the true data
  #coefs <- glm(Label ~ ., data = as.data.frame(data))
  score <- predict(model, as.data.frame(data))
  
  
  ks_true <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
  #mann_true <- wilcox.test(ps, qs, alternative = "two.sided")$statistic
  
  results[i,1] <- 1*(ks_true > quantile(t_l, alpha/2) & ks_true < quantile(t_l, 1 - (alpha/2)) )
  results[i, 2] <- ks_true
  abline(v = ks_true, col = 'red')
  #results[i,2] <- 1*(mann_true < quantile(stats[,2], 1-alpha))
}

barplot(table(results[,1]))




load('hw3_data.RData')

