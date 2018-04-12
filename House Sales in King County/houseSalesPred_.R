# Libraries
# install.packages("gvlma")
library(ggplot2)
library(corrplot)
library(dplyr)
library(gvlma)

# Read Input file
data.file = read.csv(file="House Sales in King County/Datasets/kc_house_data.csv", 
                     header = T, 
                     sep = ","
)
head(data.file)
str(data.file)

summary(data.file)

# Data preprocessing
data.file$id = NULL
data.file$date = NULL

# Train Test Split
set.seed(172)
sample = sample(1:nrow(data.file),size=0.7*nrow(data.file))
train.data.file = data.file[sample,]
test.data.file = data.file[-sample,]
nrow(data.file)
nrow(train.data.file)
nrow(test.data.file)

#Target variable normal distribution
price.ggplot = ggplot(train.data.file,aes(price)) + geom_density(kernel="gaussian") 
price.ggplot

price.log.ggplot = ggplot(train.data.file,aes(log(price))) + geom_density(kernel="gaussian")
price.log.ggplot

price.log10.ggplot = ggplot(train.data.file,aes(log10(price))) + geom_density(kernel="gaussian")
price.log10.ggplot

train.data.file$price.log = log(train.data.file$price)
train.data.file$price.log = log10(train.data.file$price)

sqft_living.ggplot = ggplot(train.data.file,aes(sqft_living)) + geom_density(kernel="gaussian") 
sqft_living.ggplot

sqft_living.ggplot = ggplot(train.data.file,aes(log(sqft_living))) + geom_density(kernel="gaussian") 
sqft_living.ggplot

train.data.file$sqft_living.log = log(train.data.file$sqft_living)

head(train.data.file)
str(train.data.file)

train.data.file$floors = as.factor(round(train.data.file$floors))
train.data.file$waterfront = as.factor(train.data.file$waterfront)
train.data.file$waterfront = as.factor(train.data.file$waterfront)
train.data.file$view = as.factor(train.data.file$view)
train.data.file$condition = as.factor(train.data.file$condition)
train.data.file$grade = as.factor(train.data.file$grade)
train.data.file$yr_built = as.factor(train.data.file$yr_built)
train.data.file$yr_renovated = as.factor(train.data.file$yr_renovated)
train.data.file$zipcode = as.factor(train.data.file$zipcode)


# Plots

floors.ggplot = ggplot(train.data.file,aes(floors,price.log))+geom_boxplot()
floors.ggplot


view.ggplot = ggplot(train.data.file,aes(view,price.log))+geom_boxplot()
view.ggplot

waterfront.ggplot = ggplot(train.data.file,aes(waterfront,price.log))+geom_boxplot()
waterfront.ggplot

zipcode.ggplot = ggplot(train.data.file,aes(zipcode,price.log))+geom_boxplot()
zipcode.ggplot

# ggplot(train.data.file, aes(floors, price))+geom_dotplot(binaxis = "y",stackdir = "center" , binwidth = 100, aes(fill=view, col = waterfront))

unique(train.data.file$view)
floors.view.waterfront.ggplot = ggplot(train.data.file, aes(floors, view)) + geom_jitter(aes(col=waterfront))
floors.view.waterfront.ggplot


grade.ggplot = ggplot(train.data.file,aes(grade,price))+geom_boxplot()
grade.ggplot


condition.ggplot = ggplot(train.data.file,aes(condition,price))+geom_boxplot()
condition.ggplot



temp = train.data.file[,c('price','price.log','bedrooms','bathrooms','sqft_living','sqft_living.log','sqft_lot','sqft_above','sqft_basement','sqft_living15','sqft_lot15')]
temp$sqft_diff = temp$sqft_lot-temp$sqft_living
# train.data.file[,c('sqft_living','sqft_lot','sqft_above','sqft_basement','sqft_living15','sqft_lot15')]
price.corplot = cor(temp)
corrplot(price.corplot, method="number")  
## Price is highly corelated to sqft_living, sqft_above and sqft_living15

train.data.file$cat.bathrooms=cut(train.data.file$bathrooms, c(-1,2,2.5,3.5,Inf))

bathrooms.ggplot = ggplot(train.data.file,aes(cat.bathrooms,price))+geom_boxplot()
bathrooms.ggplot


bedrooms.ggplot = ggplot(train.data.file,aes(as.factor(bedrooms),price))+geom_boxplot()
bedrooms.ggplot

lm1  = lm(price.log~sqft_living.log+bathrooms+waterfront+grade+condition,data = train.data.file)
summary(lm1)

lm2 = lm(price.log~sqft_living.log+bedrooms+waterfront+grade+condition,data = train.data.file)
summary(lm2)

lm3 = lm(price.log~sqft_living.log+bedrooms+waterfront+grade+condition+zipcode,data = train.data.file)
summary(lm3)

# plot(lm3)
# coef(lm3)
gvlma::gvlma(lm2)
gvlma::gvlma(lm3)

anova(lm2,lm3)

AIC(lm1)
BIC(lm1)

AIC(lm2)
BIC(lm2)

AIC(lm3)
BIC(lm3)

## Refining test file
test.data.file$sqft_living.log = log(test.data.file$sqft_living)
test.data.file$floors = as.factor(round(test.data.file$floors))
test.data.file$waterfront = as.factor(test.data.file$waterfront)
test.data.file$waterfront = as.factor(test.data.file$waterfront)
test.data.file$view = as.factor(test.data.file$view)
test.data.file$condition = as.factor(test.data.file$condition)
test.data.file$grade = as.factor(test.data.file$grade)
test.data.file$yr_built = as.factor(test.data.file$yr_built)
test.data.file$yr_renovated = as.factor(test.data.file$yr_renovated)
test.data.file$zipcode = as.factor(test.data.file$zipcode)


unique(test.data.file$grade)
unique(train.data.file$grade)

test.data.file_ = test.data.file[test.data.file$grade!= 1,]

saleProiceLogPred <- predict(lm1, test.data.file_) 
summary(saleProiceLogPred)
plot(saleProiceLogPred)

log10(10)

actuals.preds <- as.data.frame(cbind(actuals.log=log(test.data.file_$price), 
                                     predict.log=saleProiceLogPred,
                                     actuals=test.data.file_$price,
                                     predict = 2.303*saleProiceLogPred )) 

actuals.preds <- as.data.frame(cbind(actuals.log=log10(test.data.file_$price), 
                                     predict.log=saleProiceLogPred,
                                     actuals=test.data.file_$price,
                                     predict = exp(saleProiceLogPred/log(exp(1),base=10) )  )) 
plot(lm3)

par("mar")
par(mar=c(1,1,1,1))
plot(actuals.preds$predict , actuals.preds$actuals , 
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
