paris <- read.csv("D:/UIC/Sem-2/Ranga/AirBnb/M-0897X2 paris.csv")

str(paris)
#View(paris)
paris$X <- NULL
sum(is.na(paris))

# we see there are 16 NA values.
# we remove all those records who have zero reviews
sum(paris$reviews ==0)

#we removed all the recrd who dont have any review.
paris <- paris[paris$reviews!=0,]
sum(is.na(paris))

#As we see from the data tha the uprise value is derived by price/bedrooms.Hence we can impute the Na values to zero in such case
paris[is.na(paris)] <- mean(paris$uprice,na.rm = TRUE)
sum(is.na(paris))
#View(cor(paris,method = c("pearson")))

paris_model1 <-lm(formula = price~.,data = paris)
summary(paris_model1)

#now lets remove the variables based on our results from correlation matrix
paris1 <- paris
paris1[,c("savwish","beds","monthfee","weekfee","cleanfee","secdep","sentiment","min_stay","extpeop","rating")] <-NULL
paris_model2 <-lm(formula = price~.,data = paris1)
summary(paris_model2)



