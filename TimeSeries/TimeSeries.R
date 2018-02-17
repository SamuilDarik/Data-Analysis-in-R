#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("dplyr")
library(tidyr)
library(ggplot2)
library(dplyr)

# type the path to the data here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/TimeSeries/Passengers through Reykjavik airport.csv"
passengers <- read.csv(directory)

# traspose our dataframe, leave two columns and delete X at the begining of Year&Month
passengers <- gather(passengers, "Year&Month", "Total", -1)[, -1] 
passengers$`Year&Month` <- gsub('X', '', passengers$`Year&Month`)

# let's look at our data
ggplot(passengers, aes(x=passengers$`Year&Month`, y=passengers$Total, group=1)) +
  geom_point(shape=21, fill="red", color="darkred", size=1.5) + geom_line(color="red", size=0.8) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks=seq(30000, 340000, by = 10000)) +
  theme(axis.text.x = element_text(size = 8))

# looking for the outliers with the window = 12 and k = 3 (in Chebyshev inequality)
for (i in seq(1, nrow(passengers), by=12)) {
  temp <- passengers[i:(i+11),]
  outliers_m <- temp %>% filter(Total > mean(temp$Total) + 3 * sd(temp$Total))
  outliers_l <- temp %>% filter(Total < mean(temp$Total) - 3 * sd(temp$Total))
  print(i)
  print(outliers_m)
  print(outliers_l)
}
# so we see that the dataset doesn't have the outliers

new_passengers <- passengers

# Simple Moving Average with the length of the interval g = 3, p = 1 (g = 2*p +1)
for (i in 2:(nrow(passengers)-1)) {
  new_passengers[i, 2] <- sum(passengers[i-1, 2], passengers[i, 2], passengers[i+1, 2])/3
}

ggplot() +
  geom_point(data=passengers,
             aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Before"), 
             shape=21, fill="red", size=1.5) +
  geom_line(data=passengers,
            aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Before"), 
            size=0.8) + 
  geom_point(data=new_passengers,
             aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1, colour="After"), 
             shape=21, fill="blue", size=1.5) +
  geom_line(data=new_passengers,
            aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1, colour="After"), 
            size=0.8) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks=seq(30000, 340000, by = 10000)) +
  scale_colour_manual(name=element_blank(), values=c("Before"="red", "After"="blue"))

# estimation of linear trend parameters

# coefficients for the least squares equation in the method of least squares
sum_t <- as.numeric(sum(1:nrow(passengers)))
sum_y <- as.numeric(sum(new_passengers$Total))
sum_t2 <- as.numeric(sum((1:nrow(passengers))^2))
sum_ty <- as.numeric(sum(c(1:nrow(passengers)) * new_passengers$Total))
b <- (nrow(passengers) * sum_ty - sum_t * sum_y)/(nrow(passengers) * sum_t2 - (sum_t)^2)
a <- (sum_y * sum_t2 - sum_ty * sum_t)/(nrow(passengers) * sum_t2 - (sum_t)^2)
u <- a + b * (1:nrow(passengers))

trend <- data.frame(new_passengers$`Year&Month`, u)
names(trend) <- c("Year&Month", "value")

# let's look at the trend
ggplot() +
  geom_point(data=new_passengers,
             aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1), 
             shape=21, fill="blue", color="darkred", size=1.5) + 
  geom_line(data=new_passengers,
            aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1), 
            color="blue", size=0.8) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks=seq(30000, 340000, by = 10000)) +
  geom_line(data=trend, aes(x=trend$`Year&Month`, y=trend$value, group=1), 
            color="purple", size=0.8)

# periodic component (remove a trend from the time series)

r <- new_passengers$Total - u
remains <- data.frame(new_passengers$`Year&Month`, r)
names(remains) <- c("Year&Month", "value")

# time series without trend
ggplot(remains, aes(x=remains$`Year&Month`, y=remains$value, group=1)) +
  geom_point(shape=21, fill="pink", color="darkred", size=1.5) +
  geom_line(color="pink", size=0.8) + 
  labs(title="Remains", x="Year&Month", y="value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks = seq(-60000, 150000, by = 10000))

# estimation of the periodic component in the additive model of time series

period <- 12
m <- nrow(passengers)%/%period  # integer number of periods
s <- rep(0, period)
for (t in 1:period) {
  sum_temp <- 0
  for (j in 0:(m-1)) {
    sum_temp <- sum(sum_temp, new_passengers$Total[t+j*12], -u[t+j*12])
    #sum_temp <- sum(sum_temp, new_passengers[t+j*12, 2]/u[t+j*12])
  }
  s[t] <- 1/m * sum_temp
}

# application of the additive model of forecast to the time series
new_passengers$Total <- u + s
ggplot() +
  geom_point(data=passengers,
             aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Before"), 
             shape=21, fill="red", size=1.5) +
  geom_line(data=passengers,
            aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Before"), 
            size=0.8) + 
  geom_point(data=new_passengers,
             aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1, colour="After"), 
             shape=21, fill="blue", size=1.5) +
  geom_line(data=new_passengers,
            aes(x=new_passengers$`Year&Month`, y=new_passengers$Total, group=1, colour="After"), 
            size=0.8) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks=seq(-10000, 340000, by = 10000)) +
  scale_colour_manual(name=element_blank(), values=c("Before"="red", "After"="blue"))

# estimating the quality of forecasting time series

# the mean absolute error
mae <- mean(abs(passengers$Total - new_passengers$Total))
mae

# the root mean squared error
rmse <- (mean((passengers$Total - new_passengers$Total)^2)) ^ 0.5
rmse

# the mean percentage error
mpe <- mean((passengers$Total - new_passengers$Total)/passengers$Total) * 100
mpe

# the mean absolute percentage error
mape <- mean(abs(passengers$Total - new_passengers$Total)/passengers$Total) * 100
mape

# absolute deviation
ad <- sum(abs(new_passengers$Total - mean(passengers$Total)))
ad

# the mean absolute deviation
mad <-mean(abs(new_passengers$Total - mean(passengers$Total)))
mad

# determination coef
r2 <- sum((new_passengers$Total - mean(passengers$Total)) ^ 2)/
  sum((passengers$Total - mean(passengers$Total)) ^ 2)
r2

# Thale's discrepancy ratio
v <- (sum((passengers$Total - new_passengers$Total)^2)/
        (sum(passengers$Total^2)+sum(new_passengers$Total^2))) ^ 0.5
v
