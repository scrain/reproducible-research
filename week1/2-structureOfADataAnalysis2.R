## #########################################
## Slide 6

library(kernlab)
data(spam)

# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5) # coinflip

table(trainIndicator)
# trainIndicator 
# 0 1
# 2314 2287

# use trainIndicator to split data into training/test datasets
trainSpam = spam[trainIndicator == 1, ] 
testSpam  = spam[trainIndicator == 0, ]

## #########################################
## Slide 8

names(trainSpam)
# [1] "make"              "address"           "all"               "num3d"             "our"               "over"              "remove"           
# [8] "internet"          "order"             "mail"              "receive"           "will"              "people"            "report"           
# [15] "addresses"         "free"              "business"          "email"             "you"               "credit"            "your"             
# [22] "font"              "num000"            "money"             "hp"                "hpl"               "george"            "num650"           
# [29] "lab"               "labs"              "telnet"            "num857"            "data"              "num415"            "num85"            
# [36] "technology"        "num1999"           "parts"             "pm"                "direct"            "cs"                "meeting"          
# [43] "original"          "project"           "re"                "edu"               "table"             "conference"        "charSemicolon"    
# [50] "charRoundbracket"  "charSquarebracket" "charExclamation"   "charDollar"        "charHash"          "capitalAve"        "capitalLong"      
# [57] "capitalTotal"      "type"             

## #########################################
## Slide 9

head(trainSpam)
# make address  all num3d  our over remove internet order mail receive will people report addresses free business email  you credit your font num000
# 1  0.00    0.64 0.64     0 0.32 0.00   0.00        0  0.00 0.00    0.00 0.64   0.00      0         0 0.32        0  1.29 1.93   0.00 0.96    0      0
# 7  0.00    0.00 0.00     0 1.92 0.00   0.00        0  0.00 0.64    0.96 1.28   0.00      0         0 0.96        0  0.32 3.85   0.00 0.64    0      0
# 9  0.15    0.00 0.46     0 0.61 0.00   0.30        0  0.92 0.76    0.76 0.92   0.00      0         0 0.00        0  0.15 1.23   3.53 2.00    0      0
# 12 0.00    0.00 0.25     0 0.38 0.25   0.25        0  0.00 0.00    0.12 0.12   0.12      0         0 0.00        0  0.00 1.16   0.00 0.77    0      0
# 14 0.00    0.00 0.00     0 0.90 0.00   0.90        0  0.00 0.90    0.90 0.00   0.90      0         0 0.00        0  0.00 2.72   0.00 0.90    0      0
# 16 0.00    0.42 0.42     0 1.27 0.00   0.42        0  0.00 1.27    0.00 0.00   0.00      0         0 1.27        0  0.00 1.70   0.42 1.27    0      0
# money hp hpl george num650 lab labs telnet num857 data num415 num85 technology num1999 parts pm direct cs meeting original project re edu table
# 1   0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0       0  0   0     0
# 7   0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0       0  0   0     0
# 9   0.15  0   0      0      0   0    0      0      0 0.15      0     0          0    0.00     0  0   0.00  0       0      0.3       0  0   0     0
# 12  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0       0  0   0     0
# 14  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0       0  0   0     0
# 16  0.42  0   0      0      0   0    0      0      0 0.00      0     0          0    1.27     0  0   0.42  0       0      0.0       0  0   0     0
# conference charSemicolon charRoundbracket charSquarebracket charExclamation charDollar charHash capitalAve capitalLong capitalTotal type
# 1           0         0.000            0.000                 0           0.778      0.000    0.000      3.756          61          278 spam
# 7           0         0.000            0.054                 0           0.164      0.054    0.000      1.671           4          112 spam
# 9           0         0.000            0.271                 0           0.181      0.203    0.022      9.744         445         1257 spam
# 12          0         0.022            0.044                 0           0.663      0.000    0.000      1.243          11          184 spam
# 14          0         0.000            0.000                 0           0.000      0.000    0.000      2.083           7           25 spam
# 16          0         0.000            0.063                 0           0.572      0.063    0.000      5.659          55          249 spam

## #########################################
## Slide 10

table(trainSpam$type)

# nonspam    spam 
# 1381     906 


## #########################################
## Slide 11

plot(trainSpam$capitalAve ~ trainSpam$type)

## #########################################
## Slide 12

plot(log10(trainSpam$capitalAve+1) ~ trainSpam$type)

## #########################################
## Slide 13 - Relationships between predictors

plot(log10(trainSpam[,1:4] + 1))

## #########################################
## Slide 14 - Clustering

hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

## #########################################
## Slide 15 - New Clustering

hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55] + 1))))
plot(hClusterUpdated)

## #########################################
## Slide 17 -  Statistical prediction/modeling

trainSpam$numType = as.numeric(trainSpam$type) - 1 
costFunction = function(x, y) sum(x != (y > 0.5)) 
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType") 
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam) 
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"


## #########################################
## Slide 18 - Get a measure of uncertainty

## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam) 
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"


## #########################################
## Slide 19 - Get a measure of uncertainty

## Classification table
table(predictedSpam, testSpam$type)
# predictedSpam nonspam spam
#       nonspam    1346  458
#          spam      61  449

# Error rate (off-diagonals of above: 61, 458)
(61 + 458)/(1346 + 458 + 61 + 449)
# [1] 0.2243