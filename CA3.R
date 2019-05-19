#importing the Employment data 
employment_data <- read.csv("Employment1.csv", header = TRUE)
employment_data

#structure of the data
str(employment_data)

#importing the crime data 
crime_data <- read.csv("crime1.csv", header = TRUE)
crime_data

#structure of the data
str(crime_data)

#merging both crime and Employment data into a single dataframe
data_merge <- merge(crime_data, employment_data)
data_merge
head(data_merge)

#structure of the dataframe
str(data_merge)

#importing library lattice 
library(lattice)

#to check the normality, histogram is plotted for the Employment variable in specific counties
histogram(~Employment | County, data = data_merge)

#histogram for the Total.crime variable 
histogram(~Total.crime | County, data = data_merge)

#QQ plot is used to compare two variables by plotting against each other 
with(data_merge,
     qqplot(Total.crime, 
            Employment,
            main = "Comparing Employment and Total crime",
            xlab = "Total Crime",
            ylab = "Employment"))

#now, since the inference is not clear, a reference qq line plotted using qq line function 
#for Total.crime variable
with(data_merge, {
  qqnorm(Total.crime,
         main = "Total crimes")
  qqline(Total.crime)
})

#now, adding the reference qq line for Employment variable
with(data_merge, {
  qqnorm(Employment,
         main = "Employment")
  qqline(Employment)
})

#Plotting the qqplot for Donegal county and adding refrence qq line to it for both 
#the Employment and Total.crime variables
with(data_merge,
     qqplot(Total.crime[County == "Donegal"], 
            Employment[County == "Donegal"],
            main = "Crime vs Employment in Donegal",
            xlab = "County Total crime",
            ylab = "county Total Employment"))
with(data_merge, {
  qqnorm(Total.crime[County == "Donegal"],
         main = "Donegal")
  qqline(Total.crime[County == "Donegal"])
})
with(data_merge, {
  qqnorm(Employment[County == "Donegal"],
         main = "Donegal")
  qqline(Employment[County == "Donegal"])
}) 

#Plotting the qqplot for Kilkenny county and adding refrence qq line to it for both 
#the Employment and Total.crime variables

with(data_merge,
     qqplot(Total.crime[County == "Kilkenny"], 
            Employment[County == "Kilkenny"],
            main = "Crime vs Employment in Kilkenny",
            xlab = "County Total crime",
            ylab = "county Total Employment"))
with(data_merge, {
  qqnorm(Total.crime[County == "Kilkenny"],
         main = "Kilkenny")
  qqline(Total.crime[County == "Kilkenny"])
})
with(data_merge, {
  qqnorm(Employment[County == "Kilkenny"],
         main = "Kilkenny")
  qqline(Employment[County == "Kilkenny"])
}) 

#since the normality and distribution of the data is not concluded,
#the shapiro-wilk test is used for checking normality for Total.crime variable and Employment variable
normality_test <- shapiro.test(data_merge$Total.crime)
normality_test$p.value

normality_test <- shapiro.test(data_merge$Employment)
normality_test$p.value

#the p-values are lower than the cutoff values, hence its is not normally distributed

#Hypothesis test is executed to find the correlation and p value for both the variables
hypothesis  <- cor.test(x=data_merge$Total.crime, y=data_merge$Employment, method = 'spearman', exact = F)
hypothesis
#hypothesis test resulting alternative hypothesis omitting the null hypothesis since 
#the p-value is below 0.05

library(pwr)
#Cohen.ES function is used generate the optimal sample size with test 'r'
effecting_size <- cohen.ES(test = "r", size = "large")
effecting_size

#pwr.r.test (power analysis) is tested to find the correlation power calculation 
#and to find the optimal sample size
size_sample <- pwr.r.test(r=effecting_size$effect.size, sig.level = 0.05, power = 0.9, alternative = "two.sided")
size_sample

#Plotting the sample size
plot(size_sample)

#
library(dplyr)
#since the optimal sample size is found to be 38, data sample is generated with 38 records
data_sample <- sample_n(data_merge, 38)
data_sample
nrow(data_sample)

#Correlation test is used to find the relationship between two variables using the spearman method
correlation_test <- cor.test(data_sample$Total.crime, data_sample$Employment, method = 'spearman', exact = F)
correlation_test

#the correlation is found to be positive, proving the existence of relationship
#between two variables.
