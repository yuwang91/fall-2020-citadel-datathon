library(reshape)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dimRed)
library(uwot)
library(vizier)
library(factoextra)
library(randomForest)
library(lmvar)
library(xgboost)
library(clue)
library(scales)
library(glmnet)


embed_img <- function(X, Y, k = 15, ...) {
  args <- list(...)
  args$coords <- Y
  args$x <- X
  
  do.call(vizier::embed_plot, args)
}


setwd("C:\\Users\\yuwan\\Dropbox\\Stanford\\Year\ 2\\datathon\\movie_lense\\movie_lense")

#data loading
df <- read.csv("genome-scores.csv")
cast.data <- cast(df, movieId~tagId, sum)
rownames(cast.data) <- cast.data$movieId
cast.data <- cast.data[,-1]
cast.data <- scale(cast.data, scale = FALSE)

#cluster analysis
wss = tibble(k = 1:15, value = NA_real_)
wss$value[1] = sum(scale(cast.data, scale = FALSE)^2)
for (i in 2:nrow(wss)){
  km = kmeans(cast.data, centers = wss$k[i])
  wss$value[i] = sum(km$withinss)
}
ggplot(wss, aes(x = k, y = value)) + geom_col()

#pca
pc_princomp <- princomp(cast.data)
pc_data <- pc_princomp$scores[,1:25]
rownames(pc_data) = rownames(cast.data)

fviz_screeplot(pc_princomp, ncp = 20) #9 pcs
eigs <- pc_princomp$sdev^2
sum(eigs[1:9])/sum(eigs)


genres_df <- read.csv("movies_pivot.csv")
rownames(genres_df) <- genres_df$movieId
genres_df <- genres_df[,-1]

pc_combine <- merge(pc_data, genres_df, by =  0, all.x = TRUE)

randind = sample(nrow(pc_combine),500)


#CH for clusters
library("fpc")
library("cluster")
simdatxy = pc_combine[,2:26]
CH = tibble(
  k = 2:8,
  value = sapply(k, function(i) {
    p = pam(simdatxy, i)
    calinhara(simdatxy, p$cluster)
  })
)
ggplot(CH, aes(x = k, y = value)) + geom_line() + geom_point() +
  ylab("CH index") +
  theme_bw()


km = kmeans(pc_combine[,2:10], centers = 2)
pc_umap <- uwot::umap(pc_combine[,2:10], n_neighbors = 50, min_dist = 0.5, spread = 10)
par(mfrow = c(1,1))
embed_img(km$cluster[randind], pc_umap[randind,], cex = 2, alpha = 0.5, color_scheme = c('red','blue'), title = "UMAP on Principal Components") #umap of clusters

par(mfrow = c(1,2))
for (i in 1:2){
    temp = pc_combine[km$cluster == i,][-(1:26)]
    temp_b = sort(colSums(temp)/nrow(temp), decreasing = TRUE)[1:5]
    barplot(temp_b, las = 2)
}


#####################################################
#
# XGBoost
#
#####################################################

#### Get engagement data

ratings_df <- read.csv('ratings.csv')
head(ratings_df)
engagement <- ratings_df %>% count(movieId)
engagement <- engagement %>% column_to_rownames(.,var = 'movieId')

data_df <- data.frame(merge(cast.data,engagement, by = 0,all.x = TRUE)) %>% column_to_rownames(.,var = 'Row.names')
data_df$n <- log(data_df$n)
trainind = sample(nrow(data_df),nrow(data_df)*0.75)
data_train <- data_df[trainind,]
data_test <- data_df[-trainind,]



train_x <- data.matrix(data_train[,1:1128])
train_y <- data_train[,1129]

test_x <- data.matrix(data_test[,1:1128])
test_y <- data_test[,1129]

###ridge

cvfit_ridge <- cv.glmnet(train_x, train_y, alpha = 0)

###xgb

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

m_xg <- xgboost(
  data = xgb_train,
  max.depth = 5, 
  nrounds = 50
)


xgb.cv(
  data = xgb_train,
  nrounds = 50,
  nfold = 5,
  metrics = list('rmse'),
  max_depth = 5,
  objective = 'reg:squarederror'
)
n_clusters = 2

pc_train = pc_combine[trainind,]
km_train = kmeans(pc_train[,2:10], centers = n_clusters)
for (i in 1:n_clusters){
  assign(paste("data_sub_", i, sep = ""),  data_train[km_train$cluster == i,])
}


m_lm <- lm(n ~., data_train,  x= TRUE, y = TRUE)
cv_lm <- cv.lm(m_lm, k = 5, max_cores = 2) #MSE 0.652
m1$mse[which.min(m1$mse)] #MSE 0.701

## cluster LM

for (i in 1:n_clusters){
  print(i)
  temp_data = get(paste('data_sub_',i,sep =''))
  temp_m = lm(n ~., temp_data)
  assign(paste('m_lm_',i, sep = ''), temp_m)
}

## cluster ridge

for (i in 1:n_clusters){
  print(i)
  temp_data = get(paste('data_sub_',i,sep =''))
  train_temp_x <- data.matrix(temp_data[,1:1128])
  train_temp_y <- temp_data[,1129]
  temp_m = cv.glmnet(train_temp_x, train_temp_y, alpha = 0)
  assign(paste('m_rr_',i, sep = ''), temp_m)
}


## cluster XG

for (i in 1:n_clusters){
  print(i)
  temp_data = get(paste('data_sub_',i,sep =''))
  train_temp_x <- data.matrix(temp_data[,1:1128])
  train_temp_y <- temp_data[,1129]
  xgb_temp_train <- xgb.DMatrix(data = train_temp_x, label = train_temp_y)
  temp_m <- xgboost(
    data = xgb_temp_train,
    max.depth = 3, 
    nrounds = 100
  )
  assign(paste('m_XG_',i, sep = ''), temp_m)
}


###################
#
# OOS
#
###################




mse_lm <- mean((data_test$n - predict.lm(m_lm, data_test))^2)
mse_lm

mse_xg <- mean((data_test$n-predict(m_xg, xgb_test))^2)
mse_xg


test_labels <- cl_predict(km_train, pc_combine[-trainind,][,2:26])

mse_xg_c <- 0
for (i in 1:n_clusters){
  temp_m <- get(paste('m_XG_',i,sep = ''))
  temp_data <- data_test[test_labels == i,]
  test_temp_x <- data.matrix(temp_data[,1:1128])
  test_temp_y <- temp_data[,1129]
  xgb_temp_test <- xgb.DMatrix(data = test_temp_x, label = test_temp_y)
  mse_xg_c <- mse_xg_c + sum((test_temp_y-predict(temp_m, xgb_temp_test))^2)
}
mse_xg_c <- mse_xg_c/dim(data_test)[1]
mse_xg_c

mse_lm_c <- 0
for (i in 1:n_clusters){
  temp_m <- get(paste('m_lm_',i,sep = ''))
  temp_data <- data_test[test_labels == i,]
  test_temp_y <- temp_data[,1129]
  mse_lm_c <- mse_lm_c + sum((test_temp_y-predict.lm(temp_m, temp_data))^2)
}
mse_lm_c <- mse_lm_c/dim(data_test)[1]
mse_lm_c


mse_rr_c <- 0
for (i in 1:n_clusters){
  temp_m <- get(paste('m_rr_',i,sep = ''))
  temp_data <- data_test[test_labels == i,]
  test_temp_x <- data.matrix(temp_data[,1:1128])
  test_temp_y <- temp_data[,1129]
  mse_rr_c <- mse_rr_c + sum((test_temp_y - predict(temp_m, newx = test_temp_x, s = "lambda.min"))^2)
}
mse_rr_c <- mse_rr_c/dim(data_test)[1]
mse_rr_c

mse_ridge <- mean((data_test$n - predict(cvfit_ridge, newx = test_x, s = "lambda.min"))^2)
mse_ridge

### importance

gt <- read.csv('genome-tags.csv')
xg1_imp <- sapply(as.vector(xgb.importance(model = m_XG_1)$Feature), function(i) as.numeric(substring(i,2)))
xg2_imp <- sapply(as.vector(xgb.importance(model = m_XG_2)$Feature), function(i) as.numeric(substring(i,2)))
head(gt[xg1_imp,],20)
