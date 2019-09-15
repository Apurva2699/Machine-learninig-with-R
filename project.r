data1= read.csv('house_data.csv')  #read the file
head(data1)
y<- data1$price
x<- data1$yr_built
x1=data1$sqft_living
x2 <- data1$floors
x3 <- data1$sqft_above

pairs(~y+x1,data=data1)      # plot the matrix i.e when thw Area is increases as well as price also be increases
boxplot(x1)                  # with outliers in column of sqft_living
                             
x1.low <- x1[x1< 4000]       # without outliers
boxplot(x1.low)

# modeling with help of summary
#model1
model1 <- lm(formula = y~x+x1+x2+x3, data = data1) 
summary(model1)
#Take the values from user
x = as.integer(readline(prompt="Enter your year: "));
x1 = as.integer(readline(prompt="Enter your sqft_living: "));
x2 = as.integer(readline(prompt="Enter your floor: "));
x3 =as.integer( readline(prompt="Enter your sq_above: "));
new = data.frame(x,x1,x2,x3)
predict(regressor,new)

#Here '.' means we are using all the predictors in the dataset.
#model 2
x4 <- data1$bedrooms
x5 <- data1$bathrooms
x6 <- data1$waterfront
x7 <- data1$view
model2 <- lm(formula = y~x4+x5+x6+x7, data = data1) # Pass more than two parameter by using lm function
summary(model2)
x4 = as.integer(readline(prompt="Enter no of bedroom: "));
x5 = as.integer(readline(prompt="Enter no of bathroom: "));
x6 = as.integer(readline(prompt="Enter no of waterfront: "));
x7 =as.integer( readline(prompt="Enter view "));
new = data.frame(x5,x2,x4,x6,x7)
s1 <-predict(regressor,new)   #predict the Rate from some parameter
print(s1)


