# Exercise 1.1 ####
## To import a csv file ####
optdigits <- read.csv("C:/Users/Pontus/Desktop/optdigits.csv", header=FALSE)

# We rename the response variable to a easily identifying name so we can recall it easily
colnames(optdigits)[ncol(optdigits)] <- "number"
optdigits$number <- as.factor(optdigits$number)

## Partition the data ####
# This code has a lot of redundancies, but since this exercise wanted us to do EXACTLY like the powerpoint
n <- nrow(optdigits)
set.seed(5691) # In the original code, set was set for every codeline. It only needs to be in this equation once, since we run all the code together
id <- sample(x = seq_len(n), size = floor(nrow(optdigits) * 0.5))
id1 <- setdiff(seq_len(n), id) # This is the same as doing "seq_len(n)[-id,]" which I perfere
id2 <- sample(id1, floor(n*0.25)) # We could just use sample 1:n[-train] instead, but we want to keep the code consistent with the powerpoint

# Output
train <- optdigits[id1, ]
valid <- optdigits[id2, ]
test <- optdigits[setdiff(id1,id2), ] # in the code, another id was created, since id3 is not used anywhere in the lab. We do not store it and use it immediately

# Exercise 1.2 ####
# For predicting training data
m1_train <- kknn(formula = number ~ ., train = train, test = train, k = 30, kernel = "rectangular")
# For predicting test data
m1_test <- kknn(formula = number ~ ., train = train, test = test, k = 30, kernel = "rectangular")

# For creating confusion matrix for training data
conf_mat1 <- table(m1_train$fitted.values, train$number)
# For creating confusion matrix for testing data
conf_mat2 <- table(m1_test$fitted.values, test$number)

# This could also be done with knitr::kable, but for this lab, we're lazy af. :)
cat("confusion matrix for train data\nrows: predicted values \ncols: true values \n")
table(m1_train$fitted.values, train$number)


cat("\nconfusion matrix for test data\nrows: predicted values \ncols: true values \n")
table(m1_test$fitted.values, test$number)

# For calculating misclassification error for test
# 1/n sum^N5_(i=1)  I(Y_i != Y\hat_i)
# is the same as:
# 1- 1/n sum^N_(i=1)  I(Y_i == Y\hat_i)

# training
misclas1 <- 1 - sum(diag(conf_mat1))/sum(conf_mat1)
# testing
misclas2 <- 1 - sum(diag(conf_mat2))/sum(conf_mat2)

# For training
misclas1
# For testing
misclas2

# Exercise 1.3 ####
m2 <- kknn(formula = number ~ ., train = train, test = train, k = 30, kernel = "rectangular")

index_8 <- which(train$number == "8")
train_data_prob <- m2$prob[index_8, ]

high_low_8_train <- order(train_data_prob[, 9])


# If we did this properly, we would do this output in kable, but for now, this will do.
cat("The observations that the model had a hard time predicting", rownames(train[index_8, ])[high_low_8_train[1:3]])
cat("\nThe probability was \n")
train_data_prob[order(train_data_prob[, 9])[1:3], ]
cat("\nThe observations that the model had an easy time predicting", rownames(train[index_8, ])[high_low_8_train[nrow(train_data_prob):(nrow(train_data_prob)-1)]])
cat("\nThe probability was \n")
train_data_prob[order(train_data_prob[, 9])[nrow(train_data_prob):(nrow(train_data_prob)-1)], ]


# 1st easy observation
ob <- train["3726", ] %>% select(-"number") %>% unlist() %>% matrix(nrow = 8, ncol = 8, byrow = T)
heatmap(ob, Rowv = NA, Colv = NA, revC = TRUE)
# 2nd easy observation
ob <- train["3631", ] %>% select(-"number") %>% unlist() %>% matrix(nrow = 8, ncol = 8, byrow = T)
heatmap(ob, Rowv = NA, Colv = NA, revC = TRUE)

# 1st hard observation
ob <- train["858", ] %>% select(-"number") %>% unlist() %>% matrix(nrow = 8, ncol = 8, byrow = T)
heatmap(ob, Rowv = NA, Colv = NA, revC = TRUE)
# 2nd hard observation
ob <- train["3301", ] %>% select(-"number") %>% unlist() %>% matrix(nrow = 8, ncol = 8, byrow = T)
heatmap(ob, Rowv = NA, Colv = NA, revC = TRUE)
# 3rd hard observation
ob <- train["2360", ] %>% select(-"number") %>% unlist() %>% matrix(nrow = 8, ncol = 8, byrow = T)
heatmap(ob, Rowv = NA, Colv = NA, revC = TRUE)

# Exercise 1.4 ####
misclas1 <- c()
misclas2 <- c()

for(i in 1:30){
  m1_train <- kknn(formula = number ~ ., train = train, test = train, k = i, kernel = "rectangular")
  m1_test <- kknn(formula = number ~ ., train = train, test = valid, k = i, kernel = "rectangular")
  
  conf_mat1 <- table(m1_train$fitted.values, train$number)
  conf_mat2 <- table(m1_test$fitted.values, valid$number)
  
  misclas1[i] <- 1 - sum(diag(conf_mat1))/sum(conf_mat1)
  misclas2[i] <- 1 - sum(diag(conf_mat2))/sum(conf_mat2)
}

misclas <- c(misclas1, misclas2)

ggplot(mapping = aes(x = rep(1:30, times = 2), y = misclas, colour = c(rep("train", times = 30), rep("valid", times = 30)))) + geom_line() + geom_point() + xlim(30, 1) + xlab("k-value") + scale_colour_discrete(name = "Dataset") + theme_minimal() + ylab("Misclassification rate")

# Exercise 1.5 ####
entr1 <- c()
entr2 <- c()

for(i in 1:30){
  #valid
  m1_test <- kknn(formula = number ~ ., train = train, test = valid, k = i, kernel = "rectangular")
  probs <- m1_test$prob
  p_hat <- sapply(X = seq_len(nrow(valid)), FUN = function(x){
    probs[x, match(as.character(valid$number), colnames(probs))[x]]
  })
  
  entr2[i] <- -sum(log(p_hat+1e-15))
  
  #train
  m1_train <- kknn(formula = number ~ ., train = train, test = train, k = i, kernel = "rectangular")
  probs <- m1_train$prob
  p_hat <- sapply(X = seq_len(nrow(train)), FUN = function(x){
    probs[x, match(as.character(train$number), colnames(probs))[x]]
  })
  
  entr1[i] <- -sum(log(p_hat+1e-15))
}

entr <- c(entr1, entr2)

ggplot(mapping = aes(x = rep(1:30, times = 2), y = entr, colour = c(rep("train", times = 30), rep("valid", times = 30)))) + geom_line() + geom_point() + xlim(30, 1) + xlab("k-value") + scale_colour_discrete(name = "Dataset") + theme_minimal() + ylab("Misclassification rate")

m1_valid <- kknn(formula = number ~ ., train = train, test = valid, k = 1, kernel = "rectangular")
# For creating confusion matrix for testing data
conf_mat2 <- table(m1_valid$fitted.values, valid$number)
cat("confusion matrix for validation data\nrows: predicted values \ncols: true values \n")
table(m1_valid$fitted.values, valid$number)

m1_test <- kknn(formula = number ~ ., train = train, test = valid, k = 20, kernel = "rectangular")
probs <- m1_test$prob
p_hat <- sapply(X = seq_len(nrow(valid)), FUN = function(x){
  probs[x, match(as.character(valid$number), colnames(probs))[x]]
})

showcase_20 <- sum(p_hat == 0)

m1_test <- kknn(formula = number ~ ., train = train, test = valid, k = 6, kernel = "rectangular")
probs <- m1_test$prob
p_hat <- sapply(X = seq_len(nrow(valid)), FUN = function(x){
  probs[x, match(as.character(valid$number), colnames(probs))[x]]
})

showcase_6 <- sum(p_hat == 0)

cat("The amount of true answers that are assumed to never happen in K=20:", showcase_20, "\n")
cat("The amount of true answers that are assumed to never happen in K=6:", showcase_6)
