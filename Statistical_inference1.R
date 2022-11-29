data= read.csv("banknotes.csv")
fake= data[data$conterfeit==0,-1]
real= data[data$conterfeit==1,-1]


### MANOVA
### check for condition:
library(MVN)
mvn(data)
mvn(fake)
mvn(real)
### not multivariate normal

### check for equal variance
library(biotools)
boxM(data[,-1], data$conterfeit)
### not equal variance

#### hence not meet the criteria for MANOVA

#### apply MANOVA even if the criteria is not met.

attach(data)
summary(manova(cbind(Length, Left, Right, Bottom, Top, Diagonal)~conterfeit), test = "Wilks")

### there is a different in the mean of the type of money.
### However, Manova is not a good test for the data due to insufficent criteria is met.


### Logistic
library(ppsr)
set.seed(1)
score(data, data$Left,  data$conterfeit, algorithm = "logistic_reg")
sample = sample.int(n = nrow(data),size = round(.75*nrow(data)),
                    replace = FALSE)

train.data=  data[sample, ]
test.data =  data[-sample, ]

mylogis= glm(formula = conterfeit~ Length+ Left+ Right+ Bottom+ Top +Diagonal, data= train.data, family = binomial)

summary(mylogis)

## training accuracy
train.pred= ifelse(predict(mylogis, train.data[,-1], type = "response")<0.5, 0,1)
sum(train.pred== train.data[,1])/150


## testing accuracy 
pred= ifelse(predict(mylogis, test.data[,-1], type = "response")<0.5, 0,1)
sum(pred== test.data[,1])/50
## confusion matrix for testing accuracy
table(pred, test.data[,1])
