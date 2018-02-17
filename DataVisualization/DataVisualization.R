#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("grid")
#install.packages("gridExtra")
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)

# pie chart

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Passengers from abroad on luxury liners.csv"
passengers <- read.csv(directory, col.names=c("Country", "Total"))

# let's leave only first four tuples and consider others as a sum
passengers <- rbind(head(passengers, 4), 
                    data.frame(Country="Other Countries", Total=sum(tail(passengers$Total), 11)))

# preparations for a plot
values <- as.numeric(as.vector(passengers$Total))
labels <- as.vector(passengers$Country)
slices <- round(100*values/sum(values), 1)
slices_labels <- paste(slices, "%", sep="")

# plot a pie chart
pie(slices,labels=slices_labels, col=rainbow(length(slices)), cex=0.5,
      main = "Passengers from abroad on luxury liners at the Port of Reykjavik")
legend(1, 1, labels, cex=0.7, fill=rainbow(length(slices)))

# bar chart

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Use of social media by individuals.csv"
using_networks <- read.csv(directory, col.names=c("Country", "Total"))

# plot a bar chart
ggplot(using_networks, aes(x=using_networks$Country, y=using_networks$Total)) +
  geom_col(fill="pink", color="darkred", size=0.05) +
  labs(title="Use of social media by individuals (networks), 2013", x="Country", y="People(thousands)") +
  scale_y_continuous(breaks=seq(0, 60, by=5)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# scatter chart

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Graduated students by age.csv"
education <- read.csv(directory, col.names=c("Level","Age", "Total"))

# plot a scatter chart
ggplot(education, aes(x=education$Age, y=education$Total)) +
  geom_point(shape=21, fill="pink", color="darkred", size=4) +
  labs(title="Graduated students by age, level - ISCED 3, 2013", x="Age", y="People") +
  scale_y_continuous(breaks=seq(200, 2500, by=100)) +
  theme(axis.text=element_text(size=6))

# multi scatter chart

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Graduated students by age_1.csv"
education <- read.csv(directory,
                      col.names=c("Age","1995-1996","1996-1997","1997-1998","1998-1999","1999-2000","2000-2001",
                                    "2001-2002","2002-2003","2003-2004","2004-2005","2005-2006","2006-2007","2007-2008","2008-2009","2009-2010",
                                    "2010-2011","2011-2012","2012-2013"))

# transform the dataset
new_education <- gather(education, "Years", "Total", -1)

# plot a multiscatter chart
ggplot(new_education, aes(new_education$Age, new_education$Total, col = new_education$Years)) +
  geom_point() +
  labs(title="Graduated students by age, level - ISCED 3, 1995 - 2013", x="Age", y="People", color = "Year") +
  scale_y_continuous(breaks=seq(200, 2500, by=100)) +
  theme(axis.text=element_text(size=6), legend.text=element_text(size=6))

# parallel coord

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Temperature in Reykjavik.csv"
temperature <- read.csv(directory, col.names=c("Month", "Temperature"))
temperature$Month <- factor(temperature$Month, levels=unique(temperature$Month))

# type the path to the dataset here
directory <- "c:/Users/sam29/Projects/R/Data-Analysis-in-R/DataVisualization/Hours of sunshine in Reykjavik.csv"
hours_sunshine <- read.csv(directory, col.names = c("Month", "Hours"))
hours_sunshine$Month <- factor(hours_sunshine$Month, levels=unique(hours_sunshine$Month))

# bar chart for one dataset
p <- ggplot(hours_sunshine, aes(x=hours_sunshine$Month,
                                y=hours_sunshine$Hours)) +
  geom_col(fill="pink", color="darkred", size=0.05) + 
  labs(x="Month", y="Hours of sunshine") +
  scale_y_continuous(breaks=seq(0, 190, by=10)) +
  theme(axis.text=element_text(size=6))

# dot plot for another
q <- ggplot(temperature, aes(x=temperature$Month, y=temperature$Temperature, group=1)) +
  geom_point(shape=21, fill="red", color="darkred", size=4) + geom_line(color="red") +
  labs(y="Temperature") +
  scale_y_continuous(breaks=seq(-1, 12, by=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=6))

# union these two plots
grid.arrange(q, p, top="Temperature & Hours of sunshine in Reykjavik, 2014")