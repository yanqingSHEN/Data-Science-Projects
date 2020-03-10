# Installing required packages

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)


# Reading the dataset

vehicles = read.csv('craigslistVehiclesFull.csv')


# Understanding the data

names(vehicles)
summary(vehicles)
head(vehicles)
colSums(is.na(vehicles))


# Filtering out the data for North Carolina

length(unique(vehicles$state_code))
unique(vehicles$state_name)

vehicles2 = vehicles %>%
  filter(state_code=='NC')


# Data Cleaning - removing unnecessary variables like latitude, longitude, url, etc
# We remove size because too many null values and can't be imputed
# We remove city and make because of too many categories which can't be grouped
# State code and state name become redundant because we only take data for NC

vehicles3 = subset(vehicles2,
                   select=-c(long,lat,county_fips,vin,state_fips,state_name,url
                             ,image_url,make,weather,size,city,state_code))            
head(vehicles3)


# Filtering out null records in columns like year, fuel, title status and transmission

usedcars = vehicles3 %>%
  filter(year!="",fuel!="",title_status!="",transmission!="")


# Null value imputation - filling categorical variables by unknown 

usedcars[usedcars==""] <- NA

usedcars$manufacturer <- as.character(usedcars$manufacturer)
usedcars$manufacturer[is.na(usedcars$manufacturer)] <- "Unknown"

usedcars$condition <- as.character(usedcars$condition)
usedcars$condition[is.na(usedcars$condition)] <- "Unknown"

usedcars$cylinders <- as.character(usedcars$cylinders)
usedcars$cylinders[is.na(usedcars$cylinders)] <- "Unknown"

usedcars$drive <- as.character(usedcars$drive)
usedcars$drive[is.na(usedcars$drive)] <- "Unknown" 

usedcars$type <- as.character(usedcars$type)
usedcars$type[is.na(usedcars$type)] <- "Unknown" 

usedcars$paint_color <- as.character(usedcars$paint_color)
usedcars$paint_color[is.na(usedcars$paint_color)] <- "Unknown" 


# Null Value Imputation and Outlier treatment for Odometer and price column
# We cap the values greater than 5 times the 99 percentile value.
# Impute the odometer column by its mean
# Filter out manual errors in filling of prices. Ex- Price = 1 or Price = 123456

mean_year = usedcars %>%
            group_by(year) %>%
            summarize(mean=mean(odometer,na.rm=TRUE),n())

mean= mean(usedcars$odometer,na.rm=TRUE)

summary(usedcars$odometer)
quantile(usedcars$odometer,probs = c(0.01,0.99),na.rm = TRUE)

usedcars %>%
  filter(odometer > quantile(usedcars$odometer,probs = c(0.99),na.rm = TRUE))

usedcars$odometer[usedcars$odometer>=5*quantile(usedcars$odometer,probs = c(0.99),na.rm = TRUE)] <- 
  quantile(usedcars$odometer,probs = c(0.99),na.rm = TRUE)*5

summary(usedcars$odometer)
quantile(usedcars$price,probs = c(0.01,0.99),na.rm = TRUE)

usedcars$price[usedcars$price>=5*quantile(usedcars$price,probs = c(0.99),na.rm = TRUE)] <- 
  quantile(usedcars$price,probs = c(0.99),na.rm = TRUE)*5

usedcars=usedcars %>%
  filter(year > 1920) 

usedcars=usedcars %>%
  filter(price < 100000) %>%
  filter(price > 100) 

usedcars$odometer[is.na(usedcars$odometer)] <- mean


# We add the variable Tier (1,2,3) which corresponds uniquely to each county 
# and is based on the socio-economic factors of the county.
# Tier 1 corresponds to the county with the highest factors. Ex- High income, Low employment

county_tier = read.csv("county_tier.csv")
names(county_tier)

usedcars2 = merge(y=usedcars,x=county_tier,by.y = "county_name",by.x = "County")
head(usedcars2)

# Data Exploration
# 1.	Year & Price
ggplot(data = usedcars2, aes(x = year, y = price)) +
  geom_point(shape = '.', alpha = .3, position = 'jitter') +
  geom_smooth()

# 2.  Cylinder & Price
ggplot(data = usedcars2, aes(x = price, fill = cylinders)) +
  geom_histogram(binwidth = 1000, color = "grey")

ggplot(data = usedcars2,
       aes(x = cylinders, y = price, fill = cylinders)) +
  geom_boxplot() + coord_flip()

# 3.	Transmission & Price
ggplot(data = usedcars2, aes(x = price, fill = drive)) +
  geom_histogram(binwidth = 1000)

# 4.	Fuel & Price
ggplot(data = usedcars,aes(x = fuel, y = price, fill = fuel)) +
  geom_boxplot()

# 5.	Odometer & Year & Price
ggplot(usedcarsod, aes(y = odometer, x= year)) +
  geom_point(shape = 21, alpha = .1, size = 1, position = 'jitter') +
  geom_smooth()

ggplot(usedcars_od, aes(x = odometer, y = log_price)) +
  geom_point(shape = 21, alpha = .05, size = 1, position = "jitter") +
  geom_smooth(color = "blue3")

# 6.	Condition & Price
ggplot(usedcars, aes(x = condition, y = price, fill = condition)) +
  geom_boxplot()


# 7.  Title Status & Price
ggplot(usedcars, aes(x = title_status, y = price, fill = title_status)) +
  geom_boxplot()

# 8.	Price & year & drive
ggplot(data = usedcars, aes(x = year, y = log_price, col = drive)) +
  geom_point(colour = 'lightskyblue3', shape = '.', alpha = .25, 
             position = 'jitter') +
  geom_smooth()

# 9.	Tier & price & condition
ggplot(usedcars, aes(x = as.factor(Tier), y = price, fill = as.factor(Tier))) +
  geom_point(shape = 21, alpha = .5, size = 1, position = "jitter") +
  facet_wrap(~condition)

# 10.	Type & drive
ggplot(usedcars, aes(x = type, y = log_price, fill = type)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=60, hjust=1))


# Modelling
# Basic model

FitAll2 = lm(price ~ odometer+year+as.factor(condition)+year*odometer
             +year*as.factor(condition),data=vehicle)
summary(FitAll2)
step(FitAll2)

# Log model
FitAll3 = lm(price ~ log(odometer)+year+as.factor(condition)+year*log(odometer)
             +year*as.factor(condition),data=vehicle)
summary(FitAll3)
step(FitAll3)

# Intuitive model
FitAll4 = lm(price ~ odometer+year+as.factor(condition)+as.factor(cylinders)+
               as.factor(fuel)+as.factor(drive)+as.factor(transmission),data=vehicle)
summary(FitAll4)
step(FitAll4)

# Backward selection

FitAll = lm(price ~ as.factor(Tier)+year+as.factor(condition)+as.factor(cylinders)
            +as.factor(fuel)+odometer+as.factor(title_status)
            +as.factor(transmission)+as.factor(drive)+as.factor(type)
            +as.factor(paint_color),data=vehicle)
summary(FitAll)
FitAll2 = lm(price ~ year+as.factor(condition)+as.factor(cylinders)
             +as.factor(fuel)+odometer+as.factor(title_status)
             +as.factor(transmission)+as.factor(drive)+as.factor(type)
             +as.factor(paint_color),data=vehicle)
summary(FitAll2)
step(FitAll2)

# Forward Selection
fit.nothing = lm(price ~ 1, data=vehicle)
summary(fit.nothing)
extractAIC(fit.nothing)[2]

fit.year = lm(price ~ year, data=vehicle)
summary(fit.year)
extractAIC(fit.year)

step(fit.nothing,direction 
     = 'forward', scope=formula('FitAll'))





