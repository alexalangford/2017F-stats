## ISYE 6501 - Week 1
## Q2.2 - k Nearest Neighbor

require(kknn)

creditcardData <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

## This code produces 22/inv
(fit.train2 <- train.kknn(R1 ~ ., creditcardData, kmax = 50, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal", "inv", "gaussian"), distance = 2, scaled = T))
table(predict(fit.train2, creditcardData), creditcardData$R1)
fit.train2

fit.kknn <- kknn(R1 ~ .,creditcardData, creditcardData)
table(creditcardData$R1, fit.kknn$fit)
fit.kknn

## Prediction
fit <- fitted(fit.kknn)
table(creditcardData$R1, fit)

prediction.binary <- as.matrix(fit)
fit

prediction.binary[prediction.binary >= 0.5] <- 1 
prediction.binary[prediction.binary < 0.5] <- 0
prediction.binary

CM <- table(creditcardData[, 11], prediction.binary)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

