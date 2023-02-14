?ks.test
?chisq.test # Non usato abbiamo bisogno che i sample siano della stessa dimensione
?wilcox.test

?colnames
?data.frame
?seq
?matrix
?mvrnorm
?glm
?predict



# 3 -----------------------------------------------------------------------



library(MASS)
set.seed(123)


n0 <- 80
n1 <- 90
k <- 3
P <- 10000
alpha <- 0.05


x <- mvrnorm(n0, mu = rep(0, k), Sigma = diag(k))
t_l <- rep(NA, P)

for(i in (1:P)){
  z_t <- mvrnorm(n1, mu = rep(0, k), Sigma = diag(k))
  u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u_t))
  
  t_l[i] <- 1*((ks.test(score[1:n0], score[(n0+1):(n0+n1)])$p.value) > alpha)
}

barplot(table(t_l))
table(t_l)



#t_l <- matrix(NA, P, 11)
P <- 10000
t_l <- rep(NA, P)
m <- 0.5

for(i in (1:P)){
    z_t <- mvrnorm(n1, mu = rep(m, k), Sigma = diag(k))
    u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
    
    model <- glm(Label ~ ., data = as.data.frame(u_t))
    score <- predict(model, as.data.frame(u_t))
    
    t_l[i] <- 1*((ks.test(score[1:n0], score[(n0+1):(n0+n1)])$p.value) > alpha)
}



barplot(table(t_l))
table(t_l)



#t_l <- matrix(NA, P, 11)
P <- 10000
t_l <- rep(NA, P)
m <- 0.7

for(i in (1:P)){
  z_t <- mvrnorm(n1, mu = rep(m, k), Sigma = diag(k))
  u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u_t))
  
  t_l[i] <- 1*((ks.test(score[1:n0], score[(n0+1):(n0+n1)])$p.value) > alpha)
}



barplot(table(t_l))
table(t_l)



  


# 4 -----------------------------------------------------------------------







x <- mvrnorm(n0, mu = rep(0, k), Sigma = diag(k))
z <- mvrnorm(n1, mu = rep(0, k), Sigma = diag(k))

u <- rbind(cbind(x, Label = 0), cbind(z, Label = 1))  



for(m in seq(0, 10, by = 1)){
  barplot(table(t_l[,m]))
}

hist(t_l[,1], prob = T, breaks = 10)  
abline(v = alpha) 
abline(v = quantile(t_l, 1 - (alpha/2))) 
abline(v = quantile(t_l, alpha/2))  
abline(v = quantile(t_l, 1-alpha), col = 'blue')  


#aux <- ks.test(score[1:n0], score[(n0+1):(n0+n1)], 
        #alternative = "two.sided")







