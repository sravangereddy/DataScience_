getwd()

install.packages("corrplot")
library(corrplot)
library(devtools)
library(ggplot2)
library(xda)

house.sales = read.csv(file="/Users/sravan/Documents/GitHub/DataAnalytics/Practice/House Sales in King County/Datasets/kc_house_data.csv",
                      sep = ",",
                      stringsAsFactors = F,
                      header = T )

summary(house.sales)
colnames(house.sales)
numSummary(house.sales)

# Dropping Id Column
house.sales.req = house.sales
house.sales.req$id=NULL

# Trimming date column to YYYYMMDD
summary(house.sales.req$date)
house.sales.req$date = substr(house.sales.req$date,1,8)
house.sales.req$date = as.numeric(house.sales.req$date)

# Adding new columns YYYYMM
house.sales.req$year.sale = substr(house.sales.req$date,1,6)
house.sales.req$year.sale = as.numeric(house.sales.req$year.sale)

##################################

# Traget vriable normal distribution
hist(house.sales.req$price)
hist(sqrt(house.sales.req$price))
hist(log(house.sales.req$price))

house.sales.req$price.log = log(house.sales.req$price)

unique(house.sales.req$bedrooms)

hist(house.sales.req$bedrooms)
hist(log(house.sales.req$bedrooms))

dfCor = cor(house.sales.req)
corrplot(dfCor)

## 
cor(house.sales.req$price, house.sales.req$yr_built)
cor(house.sales.req$price, house.sales.req$zipcode)
cor(house.sales.req$price, house.sales.req$grade)
cor(house.sales.req$price, house.sales.req$floors)
table(house.sales.req$price, house.sales.req$waterfront)
aggregate(x = house.sales.req, by = waterfront, FUN=mean(price))
?aggregate
