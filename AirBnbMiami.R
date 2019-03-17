miami = read.csv("D:/UIC/Sem-2/Ranga/AirBnb/M-0897X1 miami.csv")

#To remove the Comma from the price value
miami$price = sub(",", "", miami$price)
View(miami)
#To remove the dollar symbol and covert it into numeric.
miami$price = as.numeric(gsub("\\$", "", miami$price))
View(miami$price)
str(miami)



sum(is.na(miami))
#we find there are 54 NA values in our data. Thus we need to remove those values from our data.
sum(miami$reviews == '0')
#Thus it is difficult to access the values if the review are mising. thus we clean the data by removing the records with
# zero reviews.
miami <-na.omit(miami) 
str(miami)
miami$X <- NULL

#thus we find out that we have 145 observation after clearing the NA values.

# Lets examine if the independent variables are correleated.
View(cor(miami,method = c("pearson")))

#we see that savwish and reviews are correlated.(Directly)
#we see that beds, bathroom, bedroom and accommodates are correlated. (Directly)
#we see monthfee and weekfee are correlated.(Directly)

#let's run a model with all the variables
miami_model1 <-lm(formula = price~.,data = miami)
(summary(miami_model1))

# we find the adj R square value to be .7082


#Now let's run a model with only the sigificant variables
miami1 <- miami
miami1[,c("sentiment","cleanfee","secdep","savwish","rating","reviews")] <- NULL
miami_model2 <-lm(formula = price~.,data = miami1)
summary(miami_model2)

#We see that the adj. R square values decrease.
#Thus we cant remove all the non significant variables.

#as we see that savwish and reviews both are correlated. Tuhs we shall keep only 1.
miami2 <- miami
miami2[,c("savwish")] <- NULL
miami_model2 <-lm(formula = price~.,data = miami2)
summary(miami_model2)

#correlation
miami3 <- miami2
miami3[,c("bathroom")] <- NULL
miami_model3 <-lm(formula = price~.,data = miami3)
summary(miami_model3)

#By removing cleanfee non significant variable we see tha the adj. R sq value has increased.
miami4 <- miami3
miami4[,c("cleanfee","secdep")] <- NULL
miami_model4 <-lm(formula = price~.,data = miami4)
summary(miami_model4)

