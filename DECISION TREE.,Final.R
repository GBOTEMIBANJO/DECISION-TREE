bank_data <- read.csv("bank.csv",header=TRUE)

names(bank_data)
head(bank_data)
tail(bank_data)
summary(bank_data)

#check dimension and number of data set
nrow(bank_data)
ncol(bank_data)
dim(bank_data)

#as.factor function convert a column into a factor column
bank_data$deposit1 <- as.factor(bank_data$deposit)
str(bank_data)

#set seed
set.seed(1321)

bd <- sample(2, nrow(bank_data), replace = TRUE, prob=c(0.8,0.2)) 

#assign the subset to training and testing
train<- bank_data[bd==1,]
validate<-bank_data[bd==2,]

#retreive the dimension pf the train data set
dim(validate)
dim(train)

#install "party" package
install.packages("party")

 #activate "party" package
library("party")


#usage of ctree() function
bank_data<-ctree(deposit1~age+balance+duration,data =train)

#draw the tree
print(bank_data)

#plot the tree
plot(bank_data)

#predict model
predict(bank_data)

tab<- table(predict(bank_data),train$deposit1)
print(tab)

#diag()
sum(diag(tab))/sum(tab)

1-sum(diag(tab))/sum(tab)

test_predict<-table(predict(bank_data, newdata=validate), validate$deposit1)
print(test_predict)

sum(diag(test_predict)/ sum(test_predict))


1-sum(diag(test_predict))/sum(test_predict)
