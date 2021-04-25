
library(tidyverse)
library(splitstackshape)
library(caTools)
library(zoo)


vaccine_data <- read.csv("C:/Users/user/Desktop/Prem/College/Sem 8/R/project/Covid-Vaccination-Progress/country_vaccinations.csv")

#drop unnecessary columns - iso code, source name and source website
vaccine_data = subset(vaccine_data, select = -c(iso_code, source_name, source_website))
colnames(vaccine_data)

#IMPORTANT

# Smooth out NA Values 
sum(is.na(vaccine_data))
# Vector of column names which have NA values
na.cols <- names(which(colSums(is.na(vaccine_data)) > 0))
na.cols
# for (col in na.cols) 
vaccine_data[, na.cols] <- na.approx(vaccine_data[, na.cols], na.rm="FALSE")

sum(is.na(vaccine_data))
View(vaccine_data)
names(which(colSums(is.na(vaccine_data)) > 0))

# Replace NA values with 0
vaccine_data[is.na(vaccine_data)] = 0

sum(is.na(vaccine_data))
View(vaccine_data)


india_vaccine_data <-  vaccine_data %>%
  filter(country %in% c("India" ))

india_vaccine_data = subset(india_vaccine_data, select = -c(country, vaccines))
colnames(india_vaccine_data)
sum(is.na(india_vaccine_data))

vaccine_data = subset(vaccine_data, 
                      select = -c(daily_vaccinations_raw, 
                                  total_vaccinations_per_hundred, 
                                  people_vaccinated_per_hundred, 
                                  people_fully_vaccinated_per_hundred,
                                  daily_vaccinations_per_million, 
                                  vaccines))

colnames(vaccine_data)

india_vaccine_data <-  vaccine_data %>%
  filter(country %in% c("India" ))
india_vaccine_data = subset(india_vaccine_data, 
                      select = -country)
colnames(india_vaccine_data)

india_vaccine_data$date = as.Date(india_vaccine_data$date)




# Splitting Dataset into Train and Test
set.seed(72)
sample <- sample.split(india_vaccine_data,SplitRatio = 0.7)
train <- subset(india_vaccine_data,sample == TRUE) 
test <- subset(india_vaccine_data, sample == FALSE)


# Creating the model

sum(is.na(india_vaccine_data))
str(india_vaccine_data)
colnames(india_vaccine_data)

model <- lm(total_vaccinations ~ ., data = india_vaccine_data)
summary(model)

#Predicting
pred <- predict(model, test)
numx <- nrow(test)
x_axis <- seq(numx)
df <- data.frame(x_axis, pred,test$total_vaccinations)
df

# for Plotting lets evaluate the first 100
# df <- df[1:101, ]
# test <- test[1:101, ]

#Plotting the predicted values against the actual values
pl <- ggplot(df, aes(x=x_axis))
pl <- pl + geom_line(aes(y=pred, colour="Predicted"))
pl <- pl + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
pl <- pl + geom_line(aes(y=test$total_vaccinations, colour="Actual"))
pl <- pl + geom_point(aes(x=x_axis, y=test$total_vaccinations, colour="Actual"))
pl <- pl + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
pl <- pl + xlab("Test samples") + ylab("Total Vaccinations") + ggtitle("Model Evaluation")
pl


#Evaluation
original <- test$total_vaccinations
predicted <- pred
d <- original-predicted
d
# Mean Squared Error (MSE) 
mse <- mean((d)^2)
paste("MSE:", mse)

# Mean Absolute Error (MAE) 
mae <- mean(abs(d))
paste("MAE:", mae)

# Root Mean Squared Error (RMSE) 
rmse <- sqrt(mse)
paste("RMSE:", rmse)

# R-squared
R2 <-  1-(sum((d)^2)/sum((original-mean(original))^2))
paste("R-squared:", R2)

