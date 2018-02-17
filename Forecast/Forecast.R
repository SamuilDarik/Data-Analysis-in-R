#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("forecast")
library(tidyr)
library(ggplot2)
library(forecast)

# type the path to the data here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/Forecast/Passengers through Reykjavik airport.csv"
passengers <- read.csv(directory)

# traspose our dataframe, leave two columns and delete X at the begining of Year&Month
passengers <- gather(passengers, "Year&Month", "Total", -1)[, -1] 
passengers$`Year&Month` <- gsub('X', '', passengers$`Year&Month`)

# let's look at our data
ggplot(passengers, aes(x=passengers$`Year&Month`, y=passengers$Total, group=1)) +
  geom_point(shape=21, fill="red", color="darkred", size=1.5) + geom_line(color="red", size=0.8) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7)) +
  scale_y_continuous(breaks=seq(30000, 340000, by = 10000))

# now we are going to separate our time series into two parts.
# the big part is used to pick up parameters for the Holt-Winters method, which we can
# test on the small part
number_of_periods <- 8
period <- 12
prev_passengers <- passengers[1:(period*number_of_periods),]

# create a time series
passengers.ts <- ts(prev_passengers$Total, start = c(2008, 1), frequency = 12)

# apply the library HoltWinters method (multiplicative model)
passengers.hw <- HoltWinters(passengers.ts, seasonal = "mult")

# look at parameters
passengers.hw

# let's forecast
passengers.fc <- forecast(passengers.hw, 20)

new_values <- as.numeric(passengers.fc$mean)
next_passengers <- passengers[(period*number_of_periods+1):nrow(passengers),]
next_passengers$Total <- new_values

# the comparison of the initial data and the forecast
ggplot() +
  geom_point(data=passengers,
             aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Data"), 
             shape=21, fill="red", size=1.5) +
  geom_line(data=passengers,
            aes(x=passengers$`Year&Month`, y=passengers$Total, group=1, colour="Data"), 
            size=0.8) + 
  geom_point(data=next_passengers,
             aes(x=next_passengers$`Year&Month`, y=next_passengers$Total, group=1, colour="Forecast"), 
             shape=21, fill="blue", size=1.3) +
  geom_line(data=next_passengers,
            aes(x=next_passengers$`Year&Month`, y=next_passengers$Total, group=1, colour="Forecast"), 
            size=0.5) + 
  labs(title="Passengers through Reykjavik airport", x="Year&Month", y="Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9)) +
  scale_y_continuous(breaks=seq(-10000, 360000, by = 10000)) +
  scale_colour_manual(name=element_blank(), values=c("Data"="red", "Forecast"="blue"))

# Thale's discrepancy ratio on the forecast part
v <- (sum((passengers$Total[(nrow(prev_passengers)+1):nrow(passengers)] - next_passengers$Total)^2)/
        (sum(passengers$Total[(nrow(prev_passengers)+1):nrow(passengers)]^2)+sum(next_passengers$Total^2))) ^ 0.5
v
# we have the coef = 0.1299712, so our forecast is not bad