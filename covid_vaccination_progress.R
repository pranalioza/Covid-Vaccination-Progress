# Installing required packages
install.packages("tidyverse")
library(tidyverse)
install.packages("splitstackshape")
library(splitstackshape)
install.packages("caTools")
library(caTools)

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

# Prem: I dont think we should do this since this is vaccination data of the entire world, 
# we cant just take the average vaccinations of the world and say put it into iran or some small country
# It can majorly fuck up the dataset
# Hence commented the below 3 lines of code

# replacing null values with mean of column
# for(column in names(df)){
#   df[[column]][is.na(df[[column]])]<-mean(df[[column]],na.rm=TRUE)
# }

# Prem: I think its best to delete the rows (most suitable) or maybe just make them 0? 
# Option 1
# sum(is.na(vaccine_data))
# rowSums(is.na(vaccine_data))
# nrow(vaccine_data)
# vaccine_data <- vaccine_data[rowSums(is.na(vaccine_data)) == 0,]
# nrow(vaccine_data) # Dataset goes from 13307 to 4651

# Option 2
vaccine_data[is.na(vaccine_data)] = 0

# Check for NA values, should be 0
sum(is.na(vaccine_data)) 

# Prem: Issue here since some rows have countries have used multiple vaccines
unique(vaccine_data$vaccines)


# Statistics of Dataset
# Prem: Are the below 5 lines of code necessary? Are we learning anything from it?
mean(vaccine_data$total_vaccinations)
mean(vaccine_data$people_fully_vaccinated)

median(vaccine_data$total_vaccinations)
quantile(vaccine_data$total_vaccinations)
var(vaccine_data$total_vaccinations)

# Prem: I think we can just do the following 1 line
summary(vaccine_data$total_vaccinations)

# Unique countries in the dataset
vaccine_data%>%distinct(country)

# Number of countries included in the dataset
vaccine_data%>%distinct(country)%>%count()  


# Exploratory analysis 
less_than_million <- vaccine_data[vaccine_data$daily_vaccinations_per_million < 1, ]
less_than_million  #show entries of countries with daily vaccinations less than 1 million

# Number of countries with daily vaccinations less than 1 million
count(less_than_million)

# Adding category of grade column  - is it to be done? can it be done? 
# Prem: what grade do you want to add? and purpose of it?

#Data visualization

vaccine_names = subset(vaccine_data, select = c(vaccines))

vaccine_names

pfizer_moderna_count = vaccine_names %>% filter(
  vaccines %in% c("Moderna, Oxford/AstraZeneca", "Pfizer/BioNTech" )
)

pfizer_moderna_count

# Prem: Is this what you wanted?

dataplot <- vaccine_data %>% filter(
  vaccines %in% c("Moderna, Oxford/AstraZeneca", "Pfizer/BioNTech" )
)  %>%
  select(country, vaccines) %>% group_by(country) %>%
  cSplit("vaccines", 
         ",", 
         direction="long")
print(dataplot)
ggplot(dataplot, aes(x=country, fill = country)) + geom_bar()


# Prem 

# we try to split names of vaccines
dataplot <- vaccine_data %>% 
  select(country, vaccines) %>% group_by(country) %>%
  cSplit("vaccines", 
         ",", 
         direction="long")

# Bar Plot
ggplot(dataplot, aes(x = vaccines, fill = vaccines)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle('Vaccines per companies')

# Calculating percentage share of each company
dataplot <- 
  dataplot %>%
  group_by(vaccines) %>%
  summarise(total = n()) %>%
  mutate(percentage = round(total/sum(total)*100, 2)) 
print(dataplot)

# Pie chart
ggplot(dataplot,
       aes(x = '',
           y = total, 
           fill = vaccines)) + 
  geom_bar(stat = 'identity') +
  coord_polar('y') +
  ggtitle('Share of Vaccine Companies') +
  theme_void()


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


# PENDING 
# TODO - make the model on just India data

# Model 
# Logistic Regression


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

