
# Turtle Games
# Analysis of Sales Data

# Here we aim to provide insight into:
# - the impact of each product on sales 
# - the reliability of the sales data 
# - the relationship between global, NA, and EU sales 

############### Explore Data ############### 

# Load packages 
library(tidyverse)
library(moments)
library(data.table)
library(plotly) 

# Load data
game_sales <- read.csv('turtle_sales.csv', header=T)

# View data 
dim(game_sales)
str(game_sales)
head(game_sales)

# Remove unnecessary columns 
sales <- select(game_sales, -c(Ranking, Year, Publisher))

# Check for missing values 
colSums(is.na(sales))

# Set product as a categorical variable 
sales$Product <- as.factor(sales$Product)

# View summary statistics 
summary(sales)

# Group the sales for each product together 
# As currently these are separated by platform
group_sales <- sales %>% group_by(Product) %>%
  summarise(Total_Global_Sales = sum(Global_Sales), 
            Total_NA_Sales = sum(NA_Sales), 
            Total_EU_Sales = sum(EU_Sales))

# View summary statistics 
summary(group_sales)

# Number of products
dim(group_sales)

# Histograms and boxplots to investigate distributions of sales 
qplot(Total_Global_Sales, data=group_sales)
qplot(Total_NA_Sales, data=group_sales)
qplot(Total_EU_Sales, data=group_sales)
qplot(Total_Global_Sales, data=group_sales, geom='boxplot')
qplot(Total_NA_Sales, data=group_sales, geom='boxplot')
qplot(Total_EU_Sales, data=group_sales, geom='boxplot')

# Improve boxplots with ggplot2
ggplot(group_sales, aes(Total_Global_Sales)) +
  geom_boxplot(fill='steelblue', outlier.color='hotpink', outlier.size=3) +
  labs(title='Global Sales Boxplot')
ggplot(group_sales, aes(Total_NA_Sales)) + 
  geom_boxplot(fill='steelblue', outlier.color='hotpink', outlier.size=3) +
  labs(title='North America Sales Boxplot')
ggplot(group_sales, aes(Total_EU_Sales)) + 
  geom_boxplot(fill='steelblue', outlier.color='hotpink', outlier.size=3) +
  labs(title='Europe Sales Boxplot')

# Summary:
# There are no missing values in our final dataset
# Outliers are present in each of the three sales columns
# We have 175 rows of data  


############### Product Sales ############### 

# Top 3 games by sales in each area
top_3_global <- head(arrange(group_sales, desc(Total_Global_Sales)), n=3)
top_3_global
top_3_NA <- head(arrange(group_sales, desc(Total_NA_Sales)), n=3)
top_3_NA
top_3_EU <- head(arrange(group_sales, desc(Total_EU_Sales)), n=3)
top_3_EU

# Total sale amounts in each area 
sum(group_sales$Total_Global_Sales)
sum(group_sales$Total_NA_Sales)
sum(group_sales$Total_EU_Sales)

# Identify top 10 games by global sales 
top_sales <- head(arrange(group_sales, desc(Total_Global_Sales)), n=10)
# Plot top 10 global games 
ggplot(top_sales, aes(Product, Total_Global_Sales)) + geom_col(fill='steelblue') + coord_flip() +
  scale_x_discrete(limits=top_sales$Product) +
  labs(title='Top 10 Products by Global Sales')

# Top 10 global games, split by platform 
top_with_platform <- filter(sales, Product %in% top_sales$Product)
# Plot by platform
ggplot(top_with_platform, aes(x=Product, y=Global_Sales, fill=Platform)) + geom_col() + 
  coord_flip() + scale_x_discrete(limits=top_sales$Product) + 
  labs(title='Top 10 Products by Global Sales', subtitle='By Platform')

# Top 10 global games, split by area 
top_sales <- mutate(top_sales, Total_Other_Sales = Total_Global_Sales - Total_NA_Sales - Total_EU_Sales)
top_sales_melt = reshape2::melt(top_sales, id.vars = c('Product'),
                                measure.vars = c('Total_NA_Sales', 'Total_EU_Sales', 'Total_Other_Sales'))
# Plot by area
ggplot(top_sales_melt, aes(x=Product, y=value, fill=variable)) + geom_col(position='fill') +
  scale_x_discrete(limits=top_sales$Product) + 
  labs(y='Proportion', title='Proportion of Sales from each Area', 
       subtitle='For Top 10 Products by Global Sales', fill='Area') 

# Sales with genre information 
genre_sales <- sales %>% group_by(Product, Genre) %>%
  summarise(Total_Global_Sales = sum(Global_Sales), 
            Total_NA_Sales = sum(NA_Sales), 
            Total_EU_Sales = sum(EU_Sales))
# Plot interactive NA vs EU scatterplot with genre 
interactive_genre <- ggplot(genre_sales, aes(x=Total_NA_Sales, y=Total_EU_Sales, col=Genre)) + 
  geom_point(size=2) + labs(title='Scatterplot of NA Sales and EU Sales with Genre')
ggplotly(interactive_genre)

# Summary:
# We have identified the products with the highest global sales (107, 515, 123)
# The largest customer base is in North America (by sales)
# Suggest different genres might be preferred in NA (platform) and EU (sports)


############### Reliability of Data ############### 

# Testing for normality (plots)
qqnorm(group_sales$Total_Global_Sales, 
       main='Global Sales Normal Q-Q Plot', col='steelblue', pch=16)
qqline(group_sales$Total_Global_Sales, col='orange', lwd=2)
qqnorm(group_sales$Total_NA_Sales, 
       main='NA Sales Normal Q-Q Plot', col='steelblue', pch=16)
qqline(group_sales$Total_NA_Sales, col='orange', lwd=2)
qqnorm(group_sales$Total_EU_Sales, 
       main='EU Sales Normal Q-Q Plot', col='steelblue', pch=16)
qqline(group_sales$Total_EU_Sales,col='orange', lwd=2)

# Testing for normality (Shapiro-Wilk)
shapiro.test(group_sales$Total_Global_Sales)
shapiro.test(group_sales$Total_NA_Sales)
shapiro.test(group_sales$Total_EU_Sales)

# Skewness and kurtosis
skewness(group_sales$Total_Global_Sales)
kurtosis(group_sales$Total_Global_Sales)
skewness(group_sales$Total_NA_Sales)
kurtosis(group_sales$Total_NA_Sales)
skewness(group_sales$Total_EU_Sales)
kurtosis(group_sales$Total_EU_Sales)

# Plot histograms to display skew 
ggplot(group_sales, aes(Total_Global_Sales)) +
  geom_histogram(fill='hotpink') + 
  labs(title='Global Sales Histogram')
ggplot(group_sales, aes(Total_NA_Sales)) +
  geom_histogram(fill='hotpink') + 
  labs(title='North America Sales Histogram')
ggplot(group_sales, aes(Total_EU_Sales)) +
  geom_histogram(fill='hotpink') + 
  labs(title='Europe Sales Histogram')

# Summary:
# The sales data has very high skewness and kurtosis values. Histograms and Shapiro-Wilk tests also indicate non-normal data.
# We can see that the sales data is positively skewed, with some high outliers. We may wish to remove extreme outliers. 
# However, the main issue is that the residuals are not normally distributed (indicated by the QQ plots).
# This breaks an assumption of linear modelling analysis and may mean that the resulting model is not reliable. 


############### Relationships between Sales ###############

# Scatter plots to investigate relationships between sales 
qplot(Total_NA_Sales, Total_EU_Sales, data=group_sales)
qplot(Total_Global_Sales, Total_EU_Sales, data=group_sales)
qplot(Total_Global_Sales, Total_NA_Sales, data=group_sales)

# Simple Linear Regression  

# NA and EU sales 
# Find correlation 
cor(group_sales$Total_NA_Sales, group_sales$Total_EU_Sales)
# Linear regression model 
model1 <- lm(Total_EU_Sales ~ Total_NA_Sales, data=group_sales)
summary(model1)
# Plot residuals 
plot(model1$residuals, col='steelblue', pch=16, ylab='Residuals', 
     main='Residal Plot for NA ~ EU Sales Regression Model')
# Plot scatter and fitted model  
ggplot(group_sales, aes(x=Total_NA_Sales, y=Total_EU_Sales)) + 
  geom_point(color='steelblue', size=2) + geom_smooth(method=lm, col='orange') + 
  labs(title='Linear Regression Model for NA Sales and EU Sales')

# Global and NA sales
# Find correlation 
cor(group_sales$Total_Global_Sales, group_sales$Total_NA_Sales)
# Linear regression model 
model2 <- lm(Total_Global_Sales ~ Total_NA_Sales, data=group_sales)
summary(model2)
# Plot residuals 
plot(model2$residuals, col='steelblue', pch=16, ylab='Residuals', 
     main='Residal Plot for Global ~ NA Sales Regression Model')
# Plot scatter and fitted model  
ggplot(group_sales, aes(x=Total_NA_Sales, y=Total_Global_Sales)) + 
  geom_point(color='steelblue', size=2) + geom_smooth(method=lm, col='orange') + 
  labs(title='Linear Regression Model for NA Sales and Global Sales')

# Global and EU sales 
# Find correlation
cor(group_sales$Total_Global_Sales, group_sales$Total_EU_Sales)
# Linear regression model 
model3 <- lm(Total_Global_Sales ~ Total_EU_Sales, data=group_sales)
summary(model3)
# Plot residuals
plot(model3$residuals, col='steelblue', pch=16, ylab='Residuals', 
     main='Residal Plot for Global ~ EU Sales Regression Model')
# Plot scatter and fitted model  
ggplot(group_sales, aes(x=Total_EU_Sales, y=Total_Global_Sales)) + 
  geom_point(color='steelblue', size=2) + geom_smooth(method=lm, col='orange') + 
  labs(title='Linear Regression Model for EU Sales and Global Sales')

# Multiple Linear Regression 

# Multiple linear regression model 
mlr_model <- lm(Total_Global_Sales ~ Total_NA_Sales+Total_EU_Sales, data=group_sales)
summary(mlr_model)

# Checking accuracy of the model 
# Choose values to check predictions against 
check_forecasts <- filter(group_sales, Product %in% c(107, 326, 3267, 6815, 2877))
check_forecasts
NA_to_forecast = c(34.0, 22.1, 8.56, 7.63, 2.73)
EU_to_forecast = c(23.8, 0.52, 6.71, 3.89, 0.65)

# Predict values with model 
sales_to_forecast <- data.frame(Total_NA_Sales=NA_to_forecast,
                           Total_EU_Sales=EU_to_forecast)
check_forecasts$Predicted_Global_Sales <- predict(mlr_model, newdata=sales_to_forecast)

# View observed vs predicted values 
View(select(check_forecasts, c(Total_Global_Sales, Predicted_Global_Sales)))

# Summary:
# NA sales, EU sales, and global sales are all positively correlated. 
# NA sales and EU sales can be used in a multiple linear model to predict global sales.
# This model has a strong adjusted R squared score and appears to be fairly accurate. 


############### Conclusions ############### 

# Key results from the sales data:

# Product 107 had the highest NA, EU, and global sales. 
# The sales data is very positively skewed, and the residuals are not normally distributed. 
# NA and EU sales are positively correlated with global sales and with each other.  

# For further detail, please refer to the full report.
