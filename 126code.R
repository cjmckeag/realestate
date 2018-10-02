# import data set realestate
library(readr)
realestate <- read_delim("~/Downloads/realestate.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
View(realestate)
# assign values to variables
attach(realestate)
sales.price=SalePrice
sq.feet=SqFeet
num.beds=Beds
num.baths=Baths
air.con=factor(Air)
garage.size=Garage
pool=factor(Pool)
year.built=Year
constr.quality=factor(Quality)
arch.style=Style
lot.size=Lot
near.highway=factor(Highway)
# form scatterplot matrices to find linear patterns
pairs(~sales.price+sq.feet+num.beds+num.baths+air.con+garage.size,main="Sales Price Against First 5 Variables")
pairs(~sales.price+pool+year.built+constr.quality+arch.style+lot.size+near.highway,main="Sales Price Against Last 6 Variables")
# use best subsets regression
library(leaps)
mod=regsubsets(cbind(sq.feet,num.beds,num.baths,air.con,garage.size,pool,year.built,constr.quality,arch.style,lot.size,near.highway),sales.price)
summary.mod=summary(mod)
best.subset.adjr2<-which.max(summary.mod$adjr2)
Best.subset.adjr2
# best model has 8 predictors
summary.mod$which
# shows us which predictors are best
best.model=lm(sales.price~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway)
summary(best.model)

yhat=fitted(best.model)
resid=sales.price-yhat
plot(yhat,resid,main="Residuals vs. Fitted Values",ylab='Residuals',xlab='Fitted')
# this residual vs fitted plot shows nonconstant variance! need to transform Y vals
logy=log(y)
log.model=lm(logy~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway)
log.yhat=fitted(log.model)
log.resid=logy-log.yhat
plot(log.yhat,log.resid,main='Log-transformed Residuals vs Fitted Values',xlab='Fitted',ylab='Residuals')
abline(0,0)
# looks better
# plot residuals against omitted predictors to ensure no linearity pattern
plot(num.baths,log.resid,xlab='Number of Bathrooms',ylab='Residuals',main='Residuals vs Number of Bathrooms')
plot(air.con,log.resid,xlab='Presence of Air Conditioning (0=No, 1=Yes)',ylab='Residuals',main='Residuals vs Air Conditioning')
plot(pool,log.resid,xlab='Presence of Pool (0=No, 1=Yes)',ylab='Residuals',main="Residuals vs Presence of Pool")
# no linear patterns
summary(log.model)
# some p values greater than 0.05, so we add interaction terms 
add1(log.model,~.sq.feet*num.beds,test='F',scope=~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway+sq.feet*num.beds)
# p value is significantly lower than 0.05 so we add it to the model
log.model=lm(logy~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway+sq.feet*num.beds)
summary(log.model)
# adjusted r2 has increased significantly. lets try to lower the p value of garage.size
add1(log.model,~.sq.feet*garage.size,test='F',scope=~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway+sq.feet*num.beds+sq.feet*garage.size)
# p value is significantly less than 0.05 so we add garage.size*sq.feet to the model
log.model=lm(logy~sq.feet+constr.quality+arch.style+year.built+lot.size+garage.size+num.beds+near.highway+sq.feet*num.beds+sq.feet*garage.size)
summary(log.model)
# all p values are less than 0.05 and r2 is high! this is an appropriate regression model.
log.yhat2=fitted(log.model)
log.resid2=logy-log.yhat2
plot(log.yhat2,log.resid2,xlab='Fitted Values',ylab='Residuals',main='Residuals vs Fitted Values')
abline(0,0)
# residuals vs fitted plot looks linear, equal variance, no outliers.
qqnorm(log.resid2)
qqline(log.resid2)
# residuals follow normal distribution
# use externally studentized residuals to find any outliers
ri=rstudent(log.model)
which(abs(ri)>3)
# 24th data point is the only outlier according to externally studentized residuals
# remove 24th point and compare with full data to see if influential
newrealestate<-realestate[-c(24),]
attach(newrealestate)
newsales.price=SalePrice
newsq.feet=SqFeet
newnum.beds=Beds
newnum.baths=Baths
newair.con=factor(Air)
newgarage.size=Garage
newpool=factor(Pool)
newyear.built=Year
newconstr.quality=factor(Quality)
newarch.style=Style
newlot.size=Lot
newnear.highway=factor(Highway)
log.model=lm(log(newsales.price)~newsq.feet+newconstr.quality+newarch.style+newyear.built+newlot.size+newgarage.size+newnum.beds+newnear.highway+newsq.feet*newnum.beds+newsq.feet*newgarage.size)
summary(log.model)

# point 24 is an outlier, but not influential. does not significantly change any p values, estimates, or adjusted r2
# use cook's distance as another measure to find outliers
ci=cooks.distance(log.model)
which(abs(ci)>0.5)

# according to cook's distance, there are no outliers potential outliers
