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
test.data =  data[-sample, -1]

mylogis= glm(formula = conterfeit~ Length+ Left+ Right+ Bottom+ Top +Diagonal, data= train.data, family = binomial)

summary(mylogis)

predict(mylogis, test.data, type = "response")

