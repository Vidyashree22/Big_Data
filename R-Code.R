
## we would like to predict the Direction of the 
## stock price based on the predictor variables.

data <- read.csv(file.choose())
head(data)
## Since data in time series, then it not make sens to randomly 
## split data. Insted, we will use 2005 as our testing data and 
## the rest as our training data.
set.seed(1)
train=sample(1:nrow(data),57000)
test=-train

training_data=data
testing_data=read.csv(file.choose())
names(training_data)


model=glm(TARGET~.-ID,data=training_data,family="binomial")
summary(model)

predicted_y=predict(model,testing_data,type="response")
round(head(predicted_y),2)
dim(testing_data)

predicted_y_cat = rep(0,75819)
predicted_y_cat[predicted_y > 0.5]=1
head(predicted_y_cat,50)
dim(testing_data)
testing_y = testing_data$TARGET

## Confusion Matrix
table(testing_y,predicted_y_cat)

## Accuracy
mean(testing_y == predicted_y_cat)

##################################################
model_1=glm(TARGET~imp_ent_var16_ult1+imp_op_var41_efect_ult3+ind_var33_0+ind_var37_0+num_var13_largo+num_var14_0+num_op_var40_ult1+saldo_var20+imp_aport_var33_ult1+num_aport_var13_hace3+num_op_var39_comer_ult3+num_venta_var44_ult1+saldo_medio_var33_hace2 +saldo_medio_var33_ult1,data=training_data,family="binomial")
summary(model_1)

predicted_y_1=predict(model_1,testing_data,type="response")
round(head(predicted_y_1),2)
dim(testing_data)

predicted_y_cat_1 = rep(0,75819)
predicted_y_cat_1[predicted_y_1 > 0.5]=1
head(predicted_y_cat_1,50)
dim(predicted_y_cat)
testing_y = testing_data$TARGET

## Confusion Matrix
table(testing_y,predicted_y_cat_1)

## Accuracy
mean(testing_y == predicted_y_cat_1)

#############################
#Finding correlation
round(cor(training_data$num_op_var40_ult1,training_data$imp_aport_var33_ult1 ), 2)

write.csv(predicted_y_cat,file="C:/BDAP/testing.csv")
write.csv(predicted_y_cat_1,file="C:/BDAP/testing_1.csv")
