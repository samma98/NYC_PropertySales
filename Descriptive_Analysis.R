install.packages("dplyr")
install.packages("stats4")
install.packages("tidyverse")
install.packages("stats")
install.packages("caTools")


#Libraries

library(dplyr)
library(stats4)
library(tidyr)
library(stats)
library(tidyverse)
library(lubridate)
library(caTools)  
library(ggplot2)

#Importing the data and getting an overview

nycprop=read.csv('E:\\3rd year 2nd semester\\ST3082-statistical learning\\2nd project\\nyc-rolling-sales.csv')
#Importing the data and getting an overview

nycprop=read.csv('E:\\3rd year 2nd semester\\ST3082-statistical learning\\2nd project\\nyc-rolling-sales.csv')
#how many rows and columns?
cat(nrow(nycprop), "rows and", ncol(nycprop), "columns\n")

#print first five rows
head(nycprop)

#General summary of dataset
summary(nycprop)
names(nycprop)
View(nycprop)

###--- Data Cleaning ---###

#drop unnecessary column 
nycprop=subset(nycprop,select=-c(X,APARTMENT.NUMBER,EASE.MENT))
dim(nycprop)

str(nycprop)
attach(nycprop)

#remove duplicates

nrow(unique(nycprop))==nrow(nycprop) #Found that there are duplicates
duplicate_rows = duplicated(nycprop)
count_duplicates <- sum(duplicate_rows)
count_duplicates #There are 765 duplicates

nycprop = unique(nycprop)
count_unique = nrow(nycprop)
count_unique #There are 83783 unique values

#Removing rows 

colSums(nycprop==0)
nycprop=subset(nycprop,SALE.PRICE!=0) #remove rows having 0 prices.
nycprop=subset(nycprop,LAND.SQUARE.FEET!=0 & GROSS.SQUARE.FEET!=0)
colSums(nycprop==0)
dim(nycprop)

View(nycprop)

###missing values

#Some Columns have '-', so need to numeric. Then '-' becomes NA.
nycprop$LAND.SQUARE.FEET = as.numeric(nycprop$LAND.SQUARE.FEET)
nycprop$GROSS.SQUARE.FEET = as.numeric(nycprop$GROSS.SQUARE.FEET)
nycprop$SALE.PRICE = as.numeric(nycprop$SALE.PRICE)
colSums(is.na(nycprop)) #check missing values
dim(nycprop)
colSums(nycprop==0)


# Creating a new column called 'Property_Age'
nycprop <- nycprop %>% mutate(`Property_Age` = 2017 - `YEAR.BUILT`)

# Updating SALE.DATE by removing TIME part
nycprop <- nycprop %>% separate(col = "SALE.DATE", into = c("SALE.DATE", "TIME"), sep = " ")
nycprop=subset(nycprop,select=-c(TIME))

# Remove NA of SALE.PRICE
nycprop <- nycprop[!is.na(nycprop$SALE.PRICE),]
View(nycprop)





# Count of missing values
colSums(is.na(nycprop))

100 * colSums(is.na(nycprop)) / nrow(nycprop)


#note: add a DISCRIPTIONNNN on missing values from paper

# Remove rows missing data
nycprop <- na.omit(nycprop)
# Recheck the count of missing values
apply(is.na(nycprop), 2, sum)

# Convert from scientific notation to describe dataset
summary(nycprop)
#There are minimum values of zero for columns that do not make sense. For example, year built has a minimum value of 0.


#check dataframe where year_built is 0 years
nycprop[nycprop['YEAR.BUILT']==0]

#remove rows where year_built = 0
nycprop <- nycprop[nycprop$YEAR.BUILT != 0, ]

# Remove outliers of zero gross square feet and land square feet.
nycprop <- nycprop[nycprop$LAND.SQUARE.FEET  != 0, ]
nycprop <- nycprop[nycprop$ GROSS.SQUARE.FEET != 0, ]


dim(nycprop)



### Data Type conversions to the Data set

nycprop$BUILDING.CLASS.CATEGORY=as.factor(nycprop$BUILDING.CLASS.CATEGORY)
nycprop$BOROUGH=as.factor(nycprop$BOROUGH)
levels(nycprop$BOROUGH) = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
nycprop=subset(nycprop,SALE.PRICE>1000)
#nycprop=subset(nycprop,SALE.PRICE<5000000)
class(nycprop$BUILDING.CLASS.CATEGORY)
levels(nycprop$BUILDING.CLASS.CATEGORY)
hist(nycprop$SALE.PRICE)
dim(nycprop)






#splitting to training and testing
set.seed(1234) 
sample = sample.split(nycprop$SALE.PRICE, SplitRatio = 0.8)
train = subset(nycprop, sample == TRUE)
test = subset(nycprop, sample == FALSE)
write.csv(train, file = "train.csv", row.names = FALSE)
write.csv(test, file = "test.csv", row.names = FALSE)

nycprop=train
attach(nycprop)
dim(nycprop)


# Recheck the count of missing values
apply(is.na(nycprop), 2, sum)

# Remove rows missing data
nycprop <- na.omit(nycprop)

# Recheck the count of missing values
apply(is.na(nycprop), 2, sum)

#-------------Graphs-----------------------------#

#Distribution of Sales Price

ggplot(nycprop[(nycprop$SALE.PRICE > 100) & (nycprop$SALE.PRICE < 5000000),], aes(x=SALE.PRICE)) + 
  geom_histogram(bins=100, fill="lightblue", color="black") + 
  scale_x_continuous(limits=c(-100000, 5000000), expand=c(0,0), 
                     breaks=seq(0, 5000000, by=250000)) + 
  xlab("Sale Price") +
  ggtitle("Distribution of Sale Price") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

#log transformed sales price
# Load the required library
library(ggplot2)

# Create a histogram of the log of sales price
ggplot(nycprop, aes(x = log(SALE.PRICE))) + 
  geom_histogram(fill = "lightblue", color = "black") + 
  xlab("Log Sale Price") +
  ggtitle("Histogram of Log Sale Price") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

# Load the required library
library(ggplot2)

# Create a histogram of the log of sales price
ggplot(nycprop, aes(x = log(SALE.PRICE))) + 
  geom_histogram(fill = "lightblue", color = "black") + 
  xlab("Log Sale Price") +
  ggtitle("Histogram of Log Sale Price") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

# Load the required library
library(ggplot2)

# Create a Q-Q plot of the log of sales price
ggplot(nycprop, aes(sample = log(SALE.PRICE))) + 
  stat_qq() +
  xlab("Theoretical Quantiles") + 
  ylab("Sample Quantiles") + 
  ggtitle("Q-Q Plot of Log Sale Price") +
  theme_classic()

# Load the required library
library(stats)

# Apply the Shapiro-Wilk test
shapiro.test(log(nycprop$SALE.PRICE))




#still skewed


#Sales Prices by Boroughs
df2 = nycprop[((nycprop$SALE.PRICE > 100000) & (nycprop$SALE.PRICE < 5000000)),]
df2 <- df2[!is.na(df2$BOROUGH),]
df2$BOROUGH <- factor(df2$BOROUGH)
levels(df2$BOROUGH) = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
View(df2)
ggplot(df2, aes(x = df2$BOROUGH, y = df2$SALE.PRICE, fill = BOROUGH)) + 
  geom_boxplot() + 
  ggtitle("Sales Prices by Boroughs") +
  xlab("Borough") +
  ylab("Sale Price") +
  scale_fill_manual(values = c("Manhattan" = "blue",
                               "Bronx" = "green",
                               "Brooklyn" = "purple",
                               "Queens" = "red",
                               "Staten Island" = "grey")) +
  theme(plot.background = element_rect(fill = "white"))


# Load the required library
library(dplyr)

# Group the data by borough and compute the sale price mean for each group
grouped_data <- nycprop %>% 
  group_by(BOROUGH) %>% 
  summarize(mean_sale_price = mean(SALE.PRICE))

# Apply the Kruskal-Wallis test
kruskal.test(SALE.PRICE ~ BOROUGH, data = nycprop)

#p-value < 2.2e-16 hence different boroughs have different mean sales prices



months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colors = hsv(h = seq(0, 1, length.out = 12), s = 0.5, v = 0.5)


#log(Price) Vs log(Gross Square feet)
ggplot(data = df2, aes(x = log(GROSS.SQUARE.FEET), y = log(SALE.PRICE), color = `BOROUGH`)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~ df2$BOROUGH) +
  ggtitle("Price Vs Gross Square Footage in NYC",
          subtitle = "Distribution of Sale Price vs Gross Square feet Borough-wise") +
  scale_y_continuous("Property Sale Price", labels = scales::dollar) +
  scale_x_continuous("Gross Square Footage", labels = scales::comma) +
  theme(legend.position = "bottom")

df3=subset(df2, BOROUGH=1)
View(df3)


#Demand for each borough
nycprop_borough_count <- nycprop %>%
  group_by(BOROUGH) %>%
  summarize(count = n())
ggplot(nycprop_borough_count, aes(x = reorder(BOROUGH, count), y = count, fill = BOROUGH)) +
  geom_bar(stat = "identity") +
  xlab("Borough") +
  ylab("Count") +
  ggtitle("Demand for each borough (No.of sales borough-wise)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





nycprop$SALE.DATE <- as.Date(nycprop$SALE.DATE, "%m/%d/%Y") # ensure the date column is in the proper format
nycprop$sale_month <- month(nycprop$SALE.DATE) # extract the month from the date column
nycprop$sale_year <- year(nycprop$SALE.DATE) # extract the year from the date column

ggplot(nycprop[nycprop$sale_month %in% 1:12, ], aes(x = as.factor(nycprop$sale_month), y = nycprop$sale_price)) + 
  geom_boxplot(aes(fill = as.factor(nycprop$sale_month))) + 
  scale_fill_manual(values = colors, breaks = 1:12, labels = months) + 
  ggtitle("Housing Prices by Months") + 
  xlab("Month") + 
  ylab("Sale Price") + 
  theme_classic()

View(df2)
high_priced_sales <- subset(nycprop,GROSS.SQUARE.FEET > 1000000/2)
View(high_priced_sales)
length(high_priced_sales)
(nycprop$SALE.PRICE)

#Highest demand neighbourhood in nyc
df1 <- as.data.frame(table(nycprop$BOROUGH, nycprop$NEIGHBORHOOD))
names(df1) <- c('BOROUGH','NEIGHBORHOOD', 'Freq')
df1 <- df1 %>% arrange(desc(Freq)) %>% head(20)

p1 <- ggplot(df1, aes(x = `NEIGHBORHOOD`, y = `Freq`, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Most-in demand Neighborhood in NYC", subtitle = "Top Neighborhoods by Number") +
  theme(legend.position = "bottom") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Neighborhood") 
p1


#Most in-demand buildings in NYC by borough
df1 <- as.data.frame(table(nycprop$BOROUGH, nycprop$`BUILDING.CLASS.CATEGORY`))
names(df1) <- c('BOROUGH','BUILDING CLASS CATEGORY', 'Freq')
df1 <- df1 %>% group_by(BOROUGH) %>% arrange(desc(Freq)) %>% head(25)

ggplot(df1, aes(x = `BOROUGH`, y = `Freq`, fill = `BUILDING CLASS CATEGORY`)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Most In-Demand Buildings in NYC by Borough", subtitle = "Top types of Buildings sold in NYC") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Borough")  


