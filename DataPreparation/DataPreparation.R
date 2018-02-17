#install.packages("ggplot2")
#install.packages("dplyr")

library(ggplot2)
library(dplyr)

# type the path to the dataset here
directory <-  "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataPreparation/Population_wrong.csv"

# reading csv file
population <- read.csv(directory, header=F, col.names=c("Sex", "Year", "Amount"))

# let's see what we have (first six tuples)
head(population)

# now change our dataframe the way that
# we only have a year and a number of people in general (men and women)
population_total <- aggregate(list(Total=population$Amount), 
                              list(Year=population$Year),
                              sum)

# also look at our new dataframe
head(population_total)

# plot total population in dots
ggplot(population_total, aes(x=population_total$Year, y=population_total$Total)) +
  geom_point(shape=21, fill="blue", color="darkred", size=1) +
  labs(title="Total population of Iceland from 1841 to 2017", x="Year", y="Total") +
  scale_x_continuous(breaks=seq(1840, 2020, by=20)) +
  scale_y_continuous(breaks=seq(100000, 900000, by=50000))

# looking for outliers using
# Chebyshev inequality (mean(x)+-k*s contains at least 8/9 of data, where s - standard deviation) with k = 3
# (we can also consider "-" but it is redundantly)
outliers <- population_total %>% filter(Total >
                              mean(population_total$Total) + 3 * sd(population_total$Total))

# left data is
left <- population_total %>% filter(Total <=
                                      mean(population_total$Total) + 3 * sd(population_total$Total))

# approximating outliers 
for (i in 1:length(outliers$Year)){
  outliers$Total[i] <- tail(left, 1)$Total + ((mean(population_total$Total) + 3 * sd(population_total$Total) -
                         tail(left, 1)$Total)/length(outliers$Year) * i)
}

# approximated data
new_population_total <- data.frame(rbind(left, outliers))

# let's see what we've done
ggplot(new_population_total, aes(x=new_population_total$Year, y=unlist(new_population_total$Total))) +
  geom_point(shape=21, fill="blue", color="darkred", size=1) +
  labs(title="Total population of Iceland from 1841 to 2017 (approximated)", x="Year", y="Total") +
  scale_x_continuous(breaks=seq(1840, 2020, by=20)) +
  scale_y_continuous(breaks=seq(100000, 900000, by=50000))

# check that the median is the same before and after transformations
median_prev <-  median(population_total$Total)
median_prev
median_new <-  median(unlist(new_population_total$Total))
median_new

# boxplot showing the outlier in male group
ggplot(population, aes(x=population$Sex, y=population$Amount)) +
  geom_boxplot() + 
  labs(title = "Population of Iceland by sex from 1841 to 2017",x="Sex", y="Total") +
  scale_y_continuous(breaks=seq(100000, 600000, by=50000))

# boxplot on total population before and after approximating the outlier respectively
options(stringsAsFactors=F)
n <- data.frame(rbind(cbind(population_total$Total, group="g1(before)"), 
           cbind(unlist(new_population_total$Total), group="g2(after)")))
colnames(n) <- (c("Total", "Group"))
n$Group <- as.factor(n$Group)
n$Total <- as.integer(n$Total)

ggplot(n, aes(n$Group, n$Total)) +
  geom_boxplot() +
  labs(title="Population of Iceland from 1841 to 2017",
       x="Before & After the approximation", y="Total") +
  scale_y_continuous(breaks=seq(100000, 900000, by = 50000))