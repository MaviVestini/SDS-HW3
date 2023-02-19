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
?sum
?rowSums
?diag
?cor


# 3 -----------------------------------------------------------------------



library(MASS)
set.seed(153)


n0 <- 80
n1 <- 90
k <- 3
P <- 100
alpha <- 0.05


scores <- rep(NA, P)

for(j in (1:P)){
  x <- mvrnorm(n0, mu = rep(0, k), Sigma = diag(k))
  z <- mvrnorm(n1, mu = rep(0, k), Sigma = diag(k))
  u <- rbind(x, z)
  t_l <- rep(NA, P)
  label <- c(rep(0, n0), rep(1, n1))
  
  for(i in (1:P)){
    l <- sample(label, n0 + n1)
    u_t <- rbind(cbind(x, Label = l[1:n0]), cbind(z, Label = l[(n0+1):(n0+n1)])) 
    
    model <- glm(Label ~ ., data = as.data.frame(u_t))
    score <- predict(model, as.data.frame(u_t))
    
    t_l[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
  }
  
  #hist(t_l)
  #abline(v = quantile(t_l, 1 - alpha))
  scores[j] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic < quantile(t_l, 1 - alpha)
}


barplot(prop.table(table(scores)))
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

?curve

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



  
l <- list()
append(l,1)

# 4 -----------------------------------------------------------------------

load('hw3_data.Rdata')



# Put all data together.
super_data <- c()
for(el in 1:length(asd_data))  super_data <- c(super_data, asd_data[el])
for(el in 1:length(td_data))  super_data <- c(super_data, td_data[el])

?substr
# Take unique names of labs.
lab <- names(super_data)
init_lab <- c()
for(el in lab) init_lab <- c(init_lab, substr(el,1,2))
u_init_lab <- unique(init_lab)


# Scale every data frame by lab.
for(l in u_init_lab){
  pos <- which(grepl(l, init_lab))
  d <- matrix(NA, 0, 116)
  for(i in pos) d <- rbind(d, super_data[[i]])
  d <- as.matrix(d)
  m <- mean(d)
  s <- sd(d)
  for(i in pos) super_data[[i]] <- (super_data[[i]] - m) / s
}

# Get new scaled data frames.
asd_data_lab_scale <- super_data[1:85]
td_data_lab_scale <- super_data[86:178]


# ##
# for(i in (1:length(asd_data))){
#   paziente <- asd_data[[i]]
#   for(j in (1:116)){
#     s <- sd(paziente[[j]])
#       if(s){
#       m <- mean(paziente[[j]])
#       paziente[[j]] <- (paziente[[j]]-m)/s
#     }
#   }
#   asd_data[[i]] <- paziente
# }
# 
# for(i in (1:length(td_data))){
#   paziente <- td_data[[i]]
#   for(j in (1:116)){
#     s <- sd(paziente[[j]])
#     if(s){
#       m <- mean(paziente[[j]])
#       paziente[[j]] <- (paziente[[j]]-m)/s
#     }
#   }
#   td_data[[i]] <- paziente
# }
# 
# names <- names(asd_data[[1]])
###

# select the data

k <- 3

asd <- c()

for(i in (1:length(asd_data_lab_scale))){
  paziente <- asd_data_lab_scale[[i]]
  for(j in (1:116)){
    asd <- c(asd, mean(paziente[[j]]))
    asd <- c(asd, sd(paziente[[j]]))
    asd <- c(asd, median(paziente[[j]]))
  }
}
asd <- t(matrix(asd, nrow = 116*k))
#colnames(asd) <- names


td <- c()

for(i in (1:length(td_data_lab_scale))){
  paziente <- td_data_lab_scale[[i]]
  for(j in (1:116)){
    td <- c(td, mean(paziente[[j]]))
    td <- c(td, sd(paziente[[j]]))
    td <- c(td, median(paziente[[j]]))
  }
}
td <- t(matrix(td, nrow = 116*k))
#colnames(asd) <- names



u <- rbind(asd, td)


?glm
?sample

Label <- c(rep(1, length(asd_data)), rep(0, length(td_data)))


u <- cbind(u, Label)

N <- 500
n0 <- 85
n1<- 93
alpha <- 0.05

X <- cbind(asd, Label = 1)
Z <- cbind(td, Label = 0)
scores <- rep(NA, N)

for(i in (1:N)){
  l <- sample(Label, n0 + n1)
  # Put the samples together with the permutated labels
  u_t <- rbind(cbind(asd, Label = l[1:n0]), cbind(td, Label = l[(n0+1):(n0+n1)])) 
  
  # Train the model on the permutated data set
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  # Get the model's scores
  score <- predict(model, as.data.frame(u_t))   
  
  # Find and save the test statistic computed on the scores
  scores[i] <- ks.test(score[l == 0], score[l == 1])$statistic
}

hist(scores)
abline(v=quantile(scores, 1-alpha))

model <- glm(Label ~., data = as.data.frame(u))
score <- predict(model, as.data.frame(u))
ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic


?mtext

aux <- prcomp(t(u))
PC <- aux$rotation[,1] 

for(i in (2:40)){
  PC <- cbind(PC, aux$rotation[,i])
}

PC <- cbind(PC, Label = c(rep(1, 85), rep(0, 93)))

n0 <- 85
n1<- 93
alpha <- 0.01

X_asd <- PC[(1:85),]
Z_td <- PC[(86:178),]

P <- 1000
t_l <- rep(NA, P)
Label <- c(rep(1, length(asd_data)), rep(0, length(td_data)))

for(i in (1:P)){
  l <- sample(Label, n0 + n1)
  # Put the samples together with the permutated labels
  u_t <- rbind(cbind(X_asd, Label = l[1:n0]), cbind(Z_td, Label = l[(n0+1):(n0+n1)])) 
  
  # Train the model on the permutated data set
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  # Get the model's scores
  score <- predict(model, as.data.frame(u_t))   
  
  # Find and save the test statistic computed on the scores
  t_l[i] <- ks.test(score[l == 0], score[l == 1])$statistic
  
}

hist(t_l, xlim = c(0,0.3))
abline(v=quantile(t_l, 1-alpha))

model <- glm(Label ~., data = as.data.frame(PC))
score <- predict(model, as.data.frame(PC))
ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
abline(v=ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic)


# test 2


aux <- prcomp(t(u))
PC <- aux$rotation[,1] 

for(i in (2:50)){
  PC <- cbind(PC, aux$rotation[,i])
}

PC <- cbind(PC, Label = c(rep(1, 85), rep(0, 93)))

n0 <- 85
n1<- 93
alpha <- 0.01

X_asd <- PC[(1:85),]

P <- 1000
t_l <- rep(NA, P)
dim(PC)

for(i in (1:P)){
  idx <- sample((1):(n0), n1, replace = T)
  td_t <- cbind(PC[idx,(1:50)], Label = 0)
  
  u_t <- rbind(X_asd, td_t)
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u_t))
  
  t_l[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
  
}

hist(t_l, xlim = c(0,1))
abline(v=quantile(t_l, 1-alpha))

model <- glm(Label ~., data = as.data.frame(PC))
score <- predict(model, as.data.frame(PC))
ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
abline(v=ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic)



# test 3


aux <- prcomp(t(u))
PC <- aux$rotation[,1] 

for(i in (2:3)){
  PC <- cbind(PC, aux$rotation[,i])
}

PC <- cbind(PC, Label = c(rep(1, 85), rep(0, 93)))

n0 <- 85
n1<- 93
alpha <- 0.01

X_asd <- PC[(1:85),]

P <- 1000
t_l <- rep(NA, P)
dim(PC)

for(i in (1:P)){
  idx <- sample((1):(n0), n1, replace = T)
  td_t <- cbind(PC[idx,(1:1)], Label = 0)
  
  u_t <- rbind(X_asd, td_t)
  
  model <- glm(Label ~ ., data = as.data.frame(u_t))
  score <- predict(model, as.data.frame(u_t))
  
  t_l[i] <- ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
  
}

hist(t_l, xlim = c(0,1))
abline(v=quantile(t_l, 1-alpha))

model <- glm(Label ~., data = as.data.frame(PC))
score <- predict(model, as.data.frame(PC))
ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic
abline(v=ks.test(score[1:n0], score[(n0+1):(n0+n1)])$statistic, col = 'red')




barplot(prop.table(table(t_l)))


