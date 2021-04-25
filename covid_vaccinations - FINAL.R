# Installing required packages
install.packages("tidyverse") # For dplyr and ggplot2
install.packages("zoo") # For dealing with NA values
install.packages("splitstackshape") # To split columns (csplit)
install.packages("caTools") # For splitting the dataset into train and test



# Load required packages
library(tidyverse)
library(zoo)
library(splitstackshape)
library(caTools)

vaccine_data <- read.csv("C:/Users/user/Desktop/Prem/College/Sem 8/R/project/Covid-Vaccination-Progress/country_vaccinations.csv")


#drop unnecessary columns - iso code, source name and source website
vaccine_data = subset(vaccine_data, select = -c(iso_code, source_name, source_website))
colnames(vaccine_data)

# Exploring the Dataset
ncol(vaccine_data)
nrow(vaccine_data)
dim(vaccine_data)
head(vaccine_data)
tail(vaccine_data)
str(vaccine_data)
summary(vaccine_data)


# Data visualization

dataplot <- vaccine_data %>% 
  select(country, vaccines) %>% group_by(country) %>%
  cSplit("vaccines", 
         ",", 
         direction="long") 
dataplot = unique(dataplot)

# Bar Plot
ggplot(dataplot, aes(x = vaccines, fill = vaccines)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Vaccine") + ylab("Number of Countries") +
ggtitle('Number of Countries using a particular Vaccine')

# Calculating percentage share of each company
dataplot <- 
  dataplot %>%
  group_by(vaccines) %>%
  summarise(total = n()) %>%
  mutate(percentage = round(total/sum(total)*100, 2))

# Pie chart
ggplot(dataplot,
       aes(x = '',
           y = total, 
           fill = vaccines)) + 
  geom_bar(stat = 'identity') +
  coord_polar('y') +
  ggtitle('Share of Vaccine Companies') +
  theme_void()

# Plotting the Global Vaccination Trends wrt monthly vaccination count


# Make a copy with all NA values = 0 (easier for plotting)
vaccine_data_copy <- vaccine_data
vaccine_data_copy[is.na(vaccine_data_copy)] = 0
vaccine_data_copy$date = as.Date(vaccine_data_copy$date)

dataplot <- vaccine_data_copy %>%
  select("date","people_vaccinated","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise(people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated))
  
ggplot(dataplot) +
  geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1) +
  geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Date",y ="Vaccination count", title  = "Global Vaccination Trends", col=element_blank()) +
  theme_bw() +
  scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))


# Plotting the Indian Vaccination Trends wrt monthly vaccination count
india_vaccine_data <-  vaccine_data_copy %>%
  filter(country %in% c("India"))

indiaplot <- india_vaccine_data %>%
  select("date","people_vaccinated","people_fully_vaccinated") %>%
  group_by(date) %>%
  summarise (people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated))

ggplot(indiaplot) +
  geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1) +
  geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Date",y ="Vaccination count", title  = "India Vaccination Trends", col=element_blank()) +
  theme_bw() +
  scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))

# Plotting the trend line of vaccinations
india_plot <- india_vaccine_data %>%
  select("date","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise (people_fully_vaccinated=sum(people_fully_vaccinated) / 1000000)


ggplot(india_plot, aes(x = date, y = people_fully_vaccinated)) +
  geom_smooth(color="red") +
  ggtitle('Trend in Vaccinations in India')+
  labs(x="Months",y ="Vaccination count in millions", col=element_blank())



# Pre Processing:

# Creating dataframe for Indian Vaccination Statistics
india_vaccine_data <- vaccine_data %>%
  filter(country %in% c("India" )) %>%
  select("date", "total_vaccinations", "people_vaccinated")
View(india_vaccine_data)

# Check which rows and cols have null values
sum(is.na(india_vaccine_data))
which(is.na(india_vaccine_data), arr.ind=TRUE)

# Smooth out NA Values 

# Vector of column names which have NA values
na.cols <- names(which(colSums(is.na(india_vaccine_data)) > 0))
na.cols

india_vaccine_data[, na.cols] <- na.approx(india_vaccine_data[, na.cols], na.rm="FALSE")

sum(is.na(india_vaccine_data))
colnames(india_vaccine_data)

# Change date to days from vaccination campaign began [1,2,3...]
indian_vaccine_data_per_day <- india_vaccine_data
indian_vaccine_data_per_day$date <- seq.int(nrow(indian_vaccine_data_per_day))
View(indian_vaccine_data_per_day)

# Splitting Dataset into Train and Test

set.seed(72)
sample <- sample.split(indian_vaccine_data_per_day,SplitRatio = 0.7)
train <- subset(indian_vaccine_data_per_day,sample == TRUE) 
test <- subset(indian_vaccine_data_per_day, sample == FALSE)
nrow(train)
nrow(test)

linear_regression_model <- lm(people_vaccinated ~ date + total_vaccinations, data = train)
summary(linear_regression_model)

# Predicting
pred <- predict(linear_regression_model, test)
numx <- nrow(test)
x_axis <- seq(numx)
pred_df <- data.frame(x_axis, pred,test$people_vaccinated)

# Converting values to millions
pred_df[2:3] <- pred_df[2:3] / 1000000
test_df <- test$people_vaccinated / 1000000
# Plotting the predicted values against the actual values
pl <- ggplot(pred_df, aes(x=x_axis))
pl <- pl + geom_line(aes(y=pred, colour="Predicted"))
pl <- pl + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
pl <- pl + geom_line(aes(y=test_df, colour="Actual"))
pl <- pl + geom_point(aes(x=x_axis, y=test_df, colour="Actual"))
pl <- pl + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
pl <- pl + xlab("Test Samples") + ylab("People Vaccinated (In Millions)") + ggtitle("Model Evaluation")
pl

# Evaluation
actual <- test$people_vaccinated
predicted <- pred
diff <- actual - predicted
diff

# Converting values to Millions
actual <- actual / 1000000
predicted <- predicted / 1000000
diff <- diff / 1000000
diff

# Mean Squared Error (MSE) 
mse <- mean((diff)^2)
paste("MSE:", mse)

# Mean Absolute Error (MAE) 
mae <- mean(abs(diff))
paste("MAE:", mae)

# Root Mean Squared Error (RMSE) 
rmse <- sqrt(mse)
paste("RMSE:", rmse)

# R-squared
R2 <-  1-(sum((diff)^2)/sum((actual-mean(actual))^2))
paste("R-squared:", R2)


# Prem: Maybe we show only up till here

# Additional, Polynomial Regression
polynomial_regression_model <- lm(people_vaccinated ~ poly(date + total_vaccinations, 2), data = train)
summary(polynomial_regression_model)

# Predicting
pred <- predict(polynomial_regression_model, test)
numx <- nrow(test)
x_axis <- seq(numx)
pred_df <- data.frame(x_axis, pred,test$people_vaccinated)
pred_df

#Plotting the predicted values against the actual values
pl <- ggplot(pred_df, aes(x=x_axis))
pl <- pl + geom_line(aes(y=pred, colour="Predicted"))
pl <- pl + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
pl <- pl + geom_line(aes(y=test$people_vaccinated, colour="Actual"))
pl <- pl + geom_point(aes(x=x_axis, y=test$people_vaccinated, colour="Actual"))
pl <- pl + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
pl <- pl + xlab("Test samples") + ylab("Total Vaccinations") + ggtitle("Model Evaluation")
pl

# Evaluation
actual <- test$people_vaccinated
predicted <- pred
diff <- actual - predicted

# Mean Squared Error (MSE) 
mse <- mean((diff)^2)
paste("MSE:", mse)

# Mean Absolute Error (MAE) 
mae <- mean(abs(diff))
paste("MAE:", mae)

# Root Mean Squared Error (RMSE) 
rmse <- sqrt(mse)
paste("RMSE:", rmse)

# R-squared
R2 <-  1-(sum((diff)^2)/sum((actual-mean(actual))^2))
paste("R-squared:", R2)

# Mean absolute percentage error (MAPE)
mape <- mean(abs((actual-pred)/actual) * 100)
paste("MAPE:", mape)


summary(linear_regression_model)
summary(polynomial_regression_model)

# Extra leave it as is for now

# ARIMA Model
library(forecast)
library(xts)


model_data <- xts(india_vaccine_data[3], order.by=as.Date(india_vaccine_data$date))
model <- auto.arima(model_data)
summary(model)
futurVal <- forecast(model, h = 100)
futurVal
predict(model, n.ahead = 5)
autoplot(futurVal)



