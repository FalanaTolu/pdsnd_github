
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)
names(ny)

head(wash)
names(wash)

head(chi)
names(chi)

# Your solution code goes here

library(tidyverse)
 
# Age group is defined by the age range and for the purposes of this exercise this is:
# WW2 (1922 – 1927),Post War(1928 – 1945),Boomers(1946 – 1964),Gen X(1965 – 1980),Millenials(1981 – 1996),Gen Z(1997 – 2012)

# Some of the data entries are empty, so we set them to NA 

chi[chi==''] = NA
ny[ny==''] = NA

# First, we create a histogram estimating the number of bikeshare users(bikers) in Chicago according to their birth year

chic = na.omit(chi)
ggplot(aes(x=Birth.Year), data=chic) + 
        geom_histogram(binwidth = 1, color = 'black', fill = '#A52A2A') + 
scale_x_continuous(limits = c(1935,2001), breaks=seq(1935,2001,5)) +  
ggtitle('Histogram of Birth Year of Bikers in Chicago') + xlab('Birth Year of Bikers') +
    ylab('Number of Bikers')

# For the histogram, years earlier than 1935 in the dataset are assumed to be outliers 
# (inner fence [(Q3-Q1)*1.5] is 1954-2010, outer fence [(Q3-Q1)*3] is 1933-2031)

# It is obvious from the graph that bikeshare users in Chicago are mostly millenials(in the 1981-1996 age range),
# followed by Gen X (1965-1980)

# We also create a histogram estimating the number of bikeshare users(bikers) in New York according to their birth year

 
qplot(x=Birth.Year, data=subset(ny, !is.na(Birth.Year)), binwidth = 1,
      main='Histogram of Birth Year Distribution of Bikers in New York', xlab='Birth Year of Bikers', ylab='Number of Bikers', 
      colour=I('black'), fill=I('#483D8B')) + scale_x_continuous(limits=c(1935,2001), breaks=(seq(1935,2001,5)))

# Years earlier than 1935 in the dataset are assumed to be outliers (inner fence is 1943-2015, outer 1916-2042)

# From the graph we can see that the age distribution of bikeshare users in New York follow the same trend as those in Chicago.
# Bikers are mostly millenials(in the 1981-1996 age range), followed by Gen X (1965-1980)

# Finally, since the trends are the same for both cities, we combine the data from both cities in a new data frame called ny_chi
# while creating a new column called city to indicate which city each row comes from and another column called Age.Group 
# to indicate the different categories of age ranges
#  WW2 (1922 – 1927),Post War(1928 – 1945),Boomers(1946 – 1964),Gen X(1965 – 1980),Millenials(1981 – 1996),Gen Z(1997 – 2012)

ny$city = 'New York'
chi$city = 'Chicago'
ny_chi = rbind(ny, chi)

ny_chi$Age.Group =  ifelse(ny_chi$Birth.Year < 1922, 'NA', ifelse(ny_chi$Birth.Year >= 1922 & ny_chi$Birth.Year < 1928, 'WW2', 
                    ifelse(ny_chi$Birth.Year >= 1928 & ny_chi$Birth.Year < 1946, 'Post War', 
                    ifelse(ny_chi$Birth.Year >= 1946 & ny_chi$Birth.Year < 1965, 'Boomers', 
                    ifelse(ny_chi$Birth.Year >= 1965 & ny_chi$Birth.Year < 1981, 'Gen X', 
                    ifelse(ny_chi$Birth.Year >= 1981 & ny_chi$Birth.Year < 1997, 'Millenials', 'Gen Z'))))))



# Then we create a histogram estimating the number of bikeshare users(bikers) according to their birth year for both cities

ggplot(aes(x = Birth.Year, fill = city), data = ny_chi) + geom_bar(position = 'dodge') + 
scale_x_continuous(limits = c(1935,2001), breaks=seq(1935,2001,5)) +  
ggtitle('Histogram of Birth Year of Bikers in Chicago and New York') + xlab('Birth Year of Bikers') +
    ylab('Number of Bikers')

# Using the summary function for the Birth.Year column in the data frame for both cities confirms the observations from the
# histogram. The median and third quartile both fall in the 1981-1996 range.
    
summary(ny$Birth.Year)

summary(chi$Birth.Year)         

# We can also group the birth years in each city according to the age ranges to get a count of the most popular groupings

ageGroup.ny = cut(na.omit(ny$Birth.Year), breaks=c(1921, 1927, 1945, 1964, 1980, 1996, Inf), 
                  labels=c('WW2', 'Post War', 'Boomers', 'Gen X', 'Millenials', 'Gen Z'))

table(ageGroup.ny)

ageGroup.chi = cut(na.omit(chi$Birth.Year), breaks=c(1921, 1927, 1945, 1964, 1980, 1996, Inf), 
                  labels=c('WW2', 'Post War', 'Boomers', 'Gen X', 'Millenials', 'Gen Z'))
table(ageGroup.chi)

# We can also use the count the Age.Group column in the combined data frame ny_chi
table(ny_chi$Age.Group)   

# Information on gender and birth year is available only in Chicago and New York.
# First, we create a boxplot comparing the birth years of bikers in Chicago according to their genders.

ggplot(aes(x = Gender, y = Birth.Year), data = chic) + geom_boxplot() + 
 ggtitle('Boxplot of Birth Year vs Gender of Users in Chicago') +
       xlab('Gender of Users') + ylab('Birth Year of Users') + 
scale_y_continuous(breaks = pretty(c(1935,2001), n = 10), limits = c(1935,2001))

# From the boxplot, it seems that although there are more male bikeshare users than females, there are proportionately
# more younger females.
# The median line for females is around 1986 and for males is around 1983. Around 25% of females were born after 1990, 
# for males 25% were born after 1989

# We also create a boxplot of bikers in New York comparing their birth using their genders.

qplot(x = Gender, y = Birth.Year, data = subset(ny, !is.na(Gender)), geom = 'boxplot', 
      main='Boxplot of Birth Year vs Gender of Users in New York', ylab='Birth Year of Users', xlab='Gender of Users') +
        coord_cartesian(ylim = c(1935,2001)) + scale_y_continuous(breaks = pretty(c(1935,2001), n = 10))

# It seems that the trend also holds in New York, there are more male bikeshare users than females but proportionately
# more younger females.
# The median line for females is around 1982 and for males is around 1980. Around 25% of females were born after 1989, 
# for males 25% were born after 1987

# Since the trend is the same we can use the combined data frame ny_chi to see it more clearly

qplot(x = Gender, y = Birth.Year, data = subset(ny_chi, !is.na(Gender)), geom = 'boxplot', 
      main='Boxplot of Birth Year vs Gender of Users in Chicago and New York', ylab='Birth Year of Users', 
      xlab='Gender of Users') + facet_wrap(~city) +
        coord_cartesian(ylim = c(1960,2001)) + scale_y_continuous(breaks = pretty(c(1935,2001), n = 10))

# Females in both cities are slightly younger than males overall

# We can count the total amount of users for both genders using the table function

table(chi$Gender)

table(ny$Gender)

# Using the by function we can confirm the observations from the boxplot for both Chicago and New York, that although there are
# more males than females, females are slightly younger overall

by(chi$Birth.Year, chi$Gender,summary)

by(ny$Birth.Year, ny$Gender,summary)

# Your solution code goes here

# To answer this question, we create a scatterplot of the trip duration against the birth years categorising the data by gender

ggplot(chic,aes(x=Birth.Year,y=Trip.Duration,col=Gender))+ geom_point(aes(color=Gender), alpha = 1/2) + 
geom_smooth(se=FALSE) + ylim(0, 3500) + scale_x_continuous(limits = c(1935, 2001), breaks = seq(1935, 2001, by = 5)) +
 ggtitle('Scatterplot of Trip Duration of Bikers in Chicago') +
       xlab('Birth Year of Bikers') + ylab('Trip Duration')

# For Trip Duration in Chicago, the inner fence is -693-2206.2, outer fence is -1780.2-3293.4, so we set the limit on the
# y-axis to 0-3500

# Apart from the fact that there seems to be more trips between 1980-1995, the trip duration looks to be fairly 
# evenly balanced with age.
# The regression line shows however that females are taking longer trip durations

# We also do the same for New York

qplot(x=Birth.Year,y=Trip.Duration,data=subset(ny, !is.na(Gender)),col=Gender, 
      main='Scatterplot of Trip Duration of Bikers in New York', xlab='Birth Year of Bikers', ylab='Trip Duration') + 
geom_point(alpha = 1/2) + geom_smooth(se=FALSE) + ylim(0, 3500) + 
scale_x_continuous(limits = c(1935, 2001), breaks = seq(1935, 2001, by = 5))

# For Trip Duration in New York, the inner fence is -656.5-2075.5, outer fence is -1681-3100, so we set the limit on the
# y-axis to 0-3500

# Trip duration looks to be evenly disributed by age
# Generally however(especially between 1945-1998), it seems females are taking longer trip durations.

# We can use the summary function to find the summary statistics for trip duration in both cities to calculate the fences, so
# we can set limits
summary(chi$Trip.Duration)

summary(ny$Trip.Duration)

# Using the by function we can examine the way tip duration according to the different genders
by(chi$Trip.Duration, chi$Gender,summary)

by(ny$Trip.Duration, ny$Gender,summary)

# This confirms our observations from the plot that females are generally taking longer trip durations

# Using the combined dataframe ny_chi, we can also examine the trip durations by the different age groups
by(ny_chi$Trip.Duration, ny_chi$Age.Group, summary)
# Using the median (and ignoring the NAs), it confirms the even distribution of trip duration across ages

system('python -m nbconvert Explore_bikeshare_data.ipynb')
