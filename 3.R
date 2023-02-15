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
?prob.table
?list


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

barplot(prop.table(table(t_l)))
table(t_l)



#t_l <- matrix(NA, P, 11)
P <- 10000
t_l <- rep(NA, P)
m <- 0.3

for(i in (1:P)){
    z_t <- mvrnorm(n1, mu = rep(m, k), Sigma = diag(k))
    u_t <- rbind(cbind(x, Label = 0), cbind(z_t, Label = 1)) 
    
    model <- glm(Label ~ ., data = as.data.frame(u_t))
    score <- predict(model, as.data.frame(u_t))
    
    t_l[i] <- 1*((ks.test(score[1:n0], score[(n0+1):(n0+n1)])$p.value) > alpha)
}



barplot(prop.table(table(t_l)))
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

set.seed(123)

load('hw3_data.Rdata')

# Normalize across time 

for(i in (1:length(asd_data))){
  paziente <- asd_data[[i]]
  for(j in (1:116)){
    m <- mean(paziente[[j]])
    s <- sd(paziente[[j]])
    paziente[[j]] <- (paziente[[j]]-m)/s  
  }
  asd_data[[i]] <- paziente
}

for(i in (1:length(td_data))){
  paziente <- td_data[[i]]
  for(j in (1:116)){
    m <- mean(paziente[[j]])
    s <- sd(paziente[[j]])
    paziente[[j]] <- (paziente[[j]]-m)/s  
  }
  td_data[[i]] <- paziente
}

names <- names(asd_data[[1]])


# select the data

k <- 5 # just median


asd <- c()

for(i in (1:length(asd_data))){
  paziente <- asd_data[[i]]
  for(j in (1:116)){
    asd <- c(asd, median(paziente[[j]]))
    asd <- c(asd, sd(paziente[[j]]))
    asd <- c(asd, mean(paziente[[j]]))
    asd <- c(asd, quantile(paziente[[j]], 0.25))
    asd <- c(asd, quantile(paziente[[j]], 0.75))
  }
}
asd <- t(matrix(asd, nrow = 116*5))
#colnames(asd) <- names
asd <- cbind(asd, Label = 1)


td <- c()

for(i in (1:length(td_data))){
  paziente <- td_data[[i]]
  for(j in (1:116)){
    td <- c(td, median(paziente[[j]]))
    td <- c(td, sd(paziente[[j]]))
    td <- c(td, mean(paziente[[j]]))
    td <- c(td, quantile(paziente[[j]], 0.25))
    td <- c(td, quantile(paziente[[j]], 0.75))
  }
}
td <- t(matrix(td, nrow = 116*5))
#colnames(asd) <- names
td <- cbind(td, Label = 1)



u <- rbind(asd, td)

aux <- prcomp(t(u))
PC <- aux$rotation[,1] 

for(i in (2:10)){
  PC <- cbind(PC, aux$rotation[,i])
}

PC <- cbind(PC, Label = c(rep(1, 85), rep(0, 93)))

n0 <- 85
n1<- 93
alpha <- 0.01

X_asd <- PC[(1:85),]

P <- 1000
t_l <- rep(NA, P)


for(i in (1:P)){
  idx <- sample((n0+1):(n1+n0), n1, replace = T)
  td_t <- PC[idx,]
  
  u_t <- rbind(X_asd, td_t)
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u_t))
  
  t_l[i] <- 1*(ks.test(score[1:n0], score[(n0+1):(n0+n1)])$p.value > alpha)
  
}


barplot(prop.table(table(t_l)))


