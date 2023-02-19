
# reproducibility
library(MASS)
set.seed(123)
# Simulation size


# 3 -----------------------------------------------------------------------

?acf

boh <- acf(asd_data[[1]][[1]], lag = length(asd_data[[1]][[1]])  ,type = "covariance")

n0 <- 43
n1 <- 34
k <- 3
alpha <- 0.05
N <- 100


# first attempt, same distribution: normal with mean 0 and sd 1
c(1,3,0.2)
diag(c(0.4,2,1))

# alpha 

result <- rep(NA, N)


for(j in (1:N)){
  x <- mvrnorm(n0, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  z <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1)) 
  
  
  
  # First thing to do is train the model 
  
  stats <- rep(NA, N)
  
  for(i in (1:N)){
    
    z_t <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
    u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
      
    # First thing to do is train the model
    model <- glm(Label ~ ., data = as.data.frame(u_t))
    score <- predict(model, as.data.frame(u_t))
    
    stats[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
  }
  
  
  #hist(stats, prob = T)
  #abline(v = quantile(stats, 1 - alpha), col = 'blue')
  
  
  model <- glm(Label ~ ., data = as.data.frame(u))
  score <- predict(model, as.data.frame(u))
  result[j] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic < quantile(stats, 1 - alpha)
}

sum(result==F)/100

# distanza

wasserstein_sameS <- function(mu1, mu2){
  return(sum((mu1-mu2)^2))
}
?dist

# power

?barplot
result <- rep(NA, N)
POWER <- matrix(NA, 11, 2)
grid <- seq(0, 1, 0.1)

for(l in (1:11)){
  m <- c(1,3,0.2)
  
  for(j in (1:N)){
    
    x <- mvrnorm(n0, mu = m, Sigma = diag(c(0.4,2,1)))
    z <- mvrnorm(n1, mu = m+rep(grid[l],3), Sigma = diag(c(0.4,2,1)))
    u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1)) 
    
    
    
    # First thing to do is train the model 
    
    stats <- rep(NA, N)
    
    for(i in (1:N)){
      
      z_t <- mvrnorm(n1, mu = m, Sigma = diag(c(0.4,2,1)))
      u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
      
      # First thing to do is train the model
      model <- glm(Label ~ ., data = as.data.frame(u_t))
      score <- predict(model, as.data.frame(u_t))
      
      stats[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
    }
    
    
    #hist(stats, prob = T)
    #abline(v = quantile(stats, 1 - alpha), col = 'blue')
    
    
    model <- glm(Label ~ ., data = as.data.frame(u))
    score <- predict(model, as.data.frame(u))
    result[j] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic < quantile(stats, 1 - alpha)
  }
  POWER[l,1] <- wasserstein_sameS(m,m+rep(grid[l],3))
  POWER[l,2] <- sum(result==F)/100
}


plot(POWER[(1:11),2], type = 'l', lwd = 4)
?curve



x <- mvrnorm(n0, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
z <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1)) 

# First thing to do is train the model 

stats <- rep(NA, N)

for(i in (1:N)){
  
  z_t <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
  
  # First thing to do is train the model
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u))
  
  stats[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic
}


hist(stats, prob = T)
abline(v = quantile(stats, 1 - alpha), col = 'blue')



# check

q <- quantile(stats, 1 - alpha)
sum(stats>q)/N

result <- matrix(NA, N, 2)

for(i in (1:N)){
  z_t <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u_t <- rbind(cbind(x, Label = 0), cbind(z, Label = 1))
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u))
  result[i,1] <- 1*(ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$statistic<q)
  result[i,2] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$p.value
}


barplot(prop.table(table(result[,1])))
hist(result[result[,2]<alpha,2])



sum(result[,2]<=alpha)/N

PV <- rep(NA, N)
x <- mvrnorm(n0, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
z <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1))

for(i in (1:N)){
  z_t <- mvrnorm(n1, mu = c(1,3,0.2), Sigma = diag(c(0.4,2,1)))
  u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1))
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u))
  PV[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], alternative = "two.sided")$p.value
}


hist(PV, prob = T)



?ks.test
?chisq.test # Non usato abbiamo bisogno che i sample siano della stessa dimensione
?wilcox.test

?colnames
?data.frame
?seq
?matrix
# 4 -----------------------------------------------------------------------




load('hw3_data.RData')

