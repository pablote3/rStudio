getwd()
data=read.csv("housePrice_train.csv")
head(houses)

str(houses$LandContour)
unique(houses$LandContour)

xgboost
dtrain <- xgb.DMatrix(data.matrix(ntrain[, 2:81]), label = ntrain$SalePrice)
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=5, nthread = 2, nround=2, lambda = 0, lambda_bias = 0, alpha = 0, eval.metric = "error", eval.metric = "rmse", objective = "reg:linear")
dtest <- xgb.DMatrix(data.matrix(ntest[, 2:80]))
label = getinfo(dtest, "label")
prediction <- predict(bst, dtest)


