#CSC465 - Homework 1
#Author: Hugo Sancen
#09/29/15

#prep required packages
require("ggplot2")
library("scales")

##################################
#Problem 5: Intel-1998 dataset
##################################

#load in data
intel <- read.csv("datasets\\Intel-1998.csv", header = T, stringsAsFactors = F)
#change Date to date format (note %y is for '98 and %Y is for 1998)
intel$Date <- as.Date(intel$Date, format = "%m/%d/%Y")

#a) closing price vs date line chart
#changed line thickness so it stands out more; changed the date format to a
#more traditional format to increase precision; changed the breaks to 2 months
#to reduce clutter; tried to reduce padding on the x-axis but only partially
#successful; eliminated padding on the y-axis to reduce wasted space; adj.
#the y-axis limits to make sure all data was visible in frame; adjusted y-axis
#breaks to reduce clutter but maintain precision; adjusted y-axis name to 
#"Closing Price" to make more clear
qplot(Date, Close, data = intel, geom = "line") + geom_line(size = 0.75) +
  scale_x_date(labels = date_format("%m/%d/%y"), breaks = "2 month",
               limits = c(as.Date("1998-1-1"),as.Date("1999-1-1")),
               expand = c(0.05,0)) +
  scale_y_continuous(limits = c(60, 130), expand = c(0,0),
                     breaks = seq(60,130,10), name = "Closing Price") +
  ggtitle("Daily Closing Price on Intel Stock")

#b) volume vs date bar chart
#changed the date format to a more traditional format to increase precision;
#changed the breaks to 2 months to reduce clutter; tried to reduce padding on
#the x-axis but only partially successful; eliminated padding on the y-axis to
#reduce wasted space; adj.the y-axis limits to make sure all data was visible
#in frame; adjusted y-axis breaks to reduce clutter but maintain precision;
#adjusted y-axis labels to be shown in millions to save space; change y-axis
#name to reflect actual scale being used
qplot(Date, Volume, data = intel, geom = "bar", stat = "identity") +
  scale_x_date(labels = date_format("%m/%d/%y"), breaks = "2 month",
               limits = c(as.Date("1998-1-1"),as.Date("1999-1-1")),
               expand = c(0.05,0)) +
  scale_y_continuous(limits = c(0, 400000000), expand = c(0,0),
                     breaks = seq(0,400000000,50000000),
                     labels = seq(0,400,50), name = "Volume (in millions)") +
  ggtitle("Daily Trading Volume of Intel Stock")
#A bar chart is more appropriate for volume data because the data is discrete
#not continuous. In a line chart, you are measuring the change in a continuous
#variable over time. In a bar chart you are looking at point-in-time snapshots
#of the data and comparing them to other point-in-time snapshots that are for
#the most part independent of each other.

#c) daily stock volume histogram
#changed the binwidth to 50mm; changed the scale on the x-labels for easier
#reading; removed minor breaks b/c they don't add value in a histogram;
#updated x-axis name to reflect scale change; change y-axis breaks to improve
#precision; eliminated margin padding on y-axis to reduce wasted space; added
#white lines to the histrom to make it easier to identify the bins
qplot(Volume, data = intel, geom = "histogram", binwidth = 50000000,
      col = I("white")) +
  scale_x_continuous(labels = seq(0,400,50), limits = c(0, 400000000),
                     breaks = seq(0,400000000,50000000),
                     minor_breaks = seq(0,400000000,50000000),
                     name = "Volume (in millions)") +
  scale_y_continuous(limits = c(0,180), breaks = seq(0,180,20),
                     expand = c(0,0)) +
  ggtitle("Distribution of Daily Trading Volumes for Intel Stock")

#d)
#create new column w/ 'Range'
intel$Range <- intel$High - intel$Low
#changed binwidth to 50mm; changed scale on x-labels for easier reading;
#updated x-axis name to reflect scale change; changed y-axis breaks to improve
#precision
qplot(Volume, Range, data = intel) +
  scale_x_continuous(labels = seq(0,400,50), limits = c(0, 400000000),
                     breaks = seq(0,400000000,50000000),
                     name = "Volume (in millions)") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) + 
  ggtitle("Daily Volume vs Daily Trading Range for Intel Stock")

#e)
#default line was a polynomial surface, explicitly changed method to "lm" for
#linear model
qplot(Volume, Range, data = intel, geom = c("point", "smooth"),
      method = "lm") +
  scale_x_continuous(labels = seq(0,400,50), limits = c(0, 400000000),
                     breaks = seq(0,400000000,50000000),
                     name = "Volume (in millions)") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  ggtitle("Daily Volume vs Daily Trading Range for Intel Stock")

##################################
#Problem 7: infant dataset
##################################

#load in the data
infant <- read.csv(file = "datasets\\InfantData.csv", header = T)

#a) length vs weight scatterplot
#changed the axis names to clearer meaning; adjusted x-axis breaks so not on
#decimal points; increased the size of the data points to better distinguish
#the color between boy and girl
qplot(Height.in, Weight.lbs, col = Sex, size = I(5), data = infant) +
  scale_x_continuous(name = "Height (in.)", limits = c(15, 25),
                     breaks = seq(15,25,2)) +
  scale_y_continuous(name = "Weight (lbs.)") +
  ggtitle("Height and Weight of Infants")

#b, c, d)
# ? ? ? 
qplot(Height.in, Weight.lbs, col = Sex, data = infant) +
  geom_point(shape = 3, size = 5) +
  geom_smooth(size = 2, method = "lm", se = F) +
  scale_x_continuous(name = "Height (in.)", limits = c(15, 25),
                     breaks = seq(15,25,2)) +
  scale_y_continuous(name = "Weight (lbs.)") +
  ggtitle("Height and Weight of Infants")

