## ISYE 6501 - Week 1
## Q2.1 - Support Vector Model

require(kernlab)

## Read data into R and format as a matrix
creditcardData <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)
creditcardData <- as.matrix(creditcardData)

## Call ksvm (Support Vector Machines); Vanilladot is a simple linear kernel
## scaled=TRUE; ksvm will auto-scale the input variables
model <- ksvm(creditcardData[ , 1:10], creditcardData[,11], type="C-svc", kernel= "vanilladot", C=100, scaled=TRUE)

## Calculate a1.am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
print("a =")
print(a)

## Calculate a0 (intercept)
a0 <- -model@b
print("a0 =")
print(a0)

## View model prediction
pred <- predict(model, creditcardData[,1:10])
print("pred =")
print(pred)

## See what fraction of the model's predictions match the actual classification
print("Prediction % =")
print(sum(pred == creditcardData[,11]) / nrow(creditcardData))

