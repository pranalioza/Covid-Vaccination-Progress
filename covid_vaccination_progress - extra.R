# Installing required packages
install.packages("tidyverse")
install.packages("splitstackshape")
install.packages("caTools")
install.packages("zoo")

# Load required packages
library(tidyverse)
library(splitstackshape)
library(caTools)
library(zoo)

#exploring data

# vaccine_data <- read.csv("C:/Users/prana/Desktop/R Mini project/country_vaccinations.csv")
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

# Pre Processing:

# Check for NA values:
sum(is.na(vaccine_data))

# Columns with null values:
names(which(colSums(is.na(vaccine_data)) > 0)) 

# Total number of NA per feature
data.frame("Total NA" = colSums(is.na(vaccine_data))) %>%
  mutate ("Percentage of NA" = (colSums(is.na(vaccine_data))/nrow(vaccine_data)) %>% 
            round (3) * 100)


vaccine_data[is.na(vaccine_data)] = 0
# Check for NA values, should be 0
sum(is.na(vaccine_data)) 



# Statistics of Dataset
summary(vaccine_data$total_vaccinations)

# Unique countries in the dataset
vaccine_data%>%distinct(country)

# Number of countries included in the dataset
vaccine_data%>%distinct(country)%>%count()  


# Exploratory analysis 

sputnik_count <- vaccine_data %>% filter(vaccines %in% c("Sputnik V"))
sputnik_count%>%distinct(country) #shows names of countries with Sputnik vaccines

sputnik_count%>%distinct(country)%>%count() #count of those countries

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
  xlab("Vaccine") + ylab("Number of Countries")
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

#Plotting the count of Pfizer wrt countries

pfizer_data <- vaccine_data %>%
  filter(vaccines %in% c("Pfizer/BioNTech" )) %>%
  group_by(country) %>% 
  mutate(max_vaccine = max(total_vaccinations)) %>% 
  ungroup() %>%
  select("country", "max_vaccine") 
pfizer_data["max_vaccine"] <- pfizer_data["max_vaccine"] / 1000000
ggplot(pfizer_data) +
  geom_bar(aes(x = country, y = max_vaccine, fill = country), stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Country") + ylab("Vaccines Administered (in Millions[1,000,000])") +
  ggtitle('Count of Pfizer Vaccine')

#Plotting the Global Vaccination Trends wrt monthly vaccination count

vaccine_data$date = as.Date(vaccine_data$date)

dataplot <- vaccine_data %>%
  select ("date","people_vaccinated","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise (people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated))

ggplot(dataplot) +
geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1) +
geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1) +
scale_y_continuous(labels = scales::comma) +
labs(x="Date",y ="Vaccination count", title  = "Global Vaccination Trends", col=element_blank()) +
theme_bw() +
scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))


#Plotting the Indian Vaccination Trends wrt monthly vaccination count
india_vaccine_data <-  vaccine_data %>%
  filter(country %in% c("India" ))

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

# plotting the trend line of vaccinations
india_plot <- india_vaccine_data %>%
  select("date","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise (people_fully_vaccinated=sum(people_fully_vaccinated) / 1000000)


ggplot(india_plot, aes(x = date, y = people_fully_vaccinated)) +
  geom_smooth(color="red") +
  ggtitle('Trend in Vaccinations in India')+
  labs(x="Months",y ="Vaccination count in millions", col=element_blank())


# PENDING 
# TODO - make the model on just India data

# Model 
# Logistic Regression
# Preprocessing India Dataset:

colnames(india_vaccine_data)
# Drop country and vaccine name in dataset

# Which vaccines are used in India
unique(india_vaccine_data$vaccines)

india_vaccine_data = subset(india_vaccine_data, select = -c(country, vaccines))
colnames(india_vaccine_data)

# Check for NA values:
sum(is.na(india_vaccine_data))

# Columns with null values:
names(which(colSums(is.na(india_vaccine_data)) > 0)) 

# Total number of NA per feature
data.frame("Total NA" = colSums(is.na(india_vaccine_data))) %>%
  mutate ("Percentage of NA" = (colSums(is.na(india_vaccine_data))/nrow(india_vaccine_data)) %>% 
            round (3) * 100)


# Splitting Dataset into Train and Test
set.seed(72)
sample <- sample.split(vaccine_data,SplitRatio = 0.8)
train <- subset(vaccine_data,sample == TRUE) 
test <- subset(vaccine_data, sample == FALSE)


# Creating the model

model <- lm(total_vaccinations ~ ., data = vaccine_data)
summary(model)

#Predicting
pred <- predict(model, test)
numx <- nrow(test)
x_axis <- seq(numx)
df <- data.frame(x_axis, pred,test$total_vaccinations)
df

# for Plotting lets evaluate the first 100
df_100 <- df[1:100,]
test_100 <-  test[1:100,]
#Plotting the predicted values against the actual values
pl <- ggplot(df_100, aes(x=x_axis))
pl <- pl + geom_line(aes(y=pred, colour="Predicted"))
pl <- pl + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
pl <- pl + geom_line(aes(y=test_100$total_vaccinations, colour="Actual"))
pl <- pl + geom_point(aes(x=x_axis, y=test_100$total_vaccinations, colour="Actual"))
pl <- pl + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
pl <- pl + xlab("Test samples") + ylab("Total Vaccinations") + ggtitle("Model Evaluation")
pl


#Evaluation
original <- test$total_vaccinations
predicted <- pred
d <- original-predicted

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

# LOOK INTO ZOO PACKAGE na.approx() in r