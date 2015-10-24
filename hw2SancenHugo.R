#CSC465 - Homework 2
#Author: Hugo Sancen
#10/05/15

#prep required packages
require("ggplot2")
library("scales")
require("data.table")

#############################
#2. Perception Test Data
#############################

#load in data & create new columns to measure accuracy
exp <- read.csv("datasets\\PerceptionExperiment2007-2015Fall.csv", header = T)
exp$AbsDelta <- abs(exp$Response - exp$TrueValue)
exp$Delta <- exp$Response - exp$TrueValue

#a) what does the overall data look like in terms of judging accuracy?
p1 <- ggplot(data = exp, aes(x = Delta))
p1 + geom_histogram(position = 'identity', binwidth = 0.05) +
  scale_x_continuous(breaks = seq(-0.8,0.8,0.1), limits = c(-0.8,0.8),
                     name = "Delta (Percepted Measure - True Measure)") +
  ggtitle("Overall Perception Accuracy")

#b) what does the distribution look per method?
p2 <- ggplot(data = exp, aes(x = Test, y = Delta))
p2 + geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(name = "Delta (Percepted Measure - True Measure)",
                     limits = c(-1, 1)) +
  scale_x_discrete(name = "Graphic Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Perception by Graphic Type w/ Jitter") +
  geom_point(position = position_jitter(width =0.025), alpha = 0.075)
  
#c) is there an outlier in the original data?
p3 <- ggplot(data = exp, aes(y = Subject, x = Test))
p3 + geom_point(position = position_jitter(width = 0.025), alpha = 0.075)

#d) which charts were generally overestimated or underestimated?
p4 <- ggplot(data = exp, aes(x = Test, y = Delta))
p4 + geom_bar(stat = "summary", fun.y = "median", position = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name = "Median Delta (Percepted Measure-True Measure)") +
  ggtitle("Median Delta by Test Type")

#e) compare displays 1 and 2 for subjects 56-73. any difference?
exp2e <- exp[exp$Subject >= 56 & exp$Subject <=73,]
p5 <- ggplot(data = exp2e, aes(x = factor(Display), y = Delta))
p5 + geom_boxplot() + scale_x_discrete(name = "Display") +
  scale_y_continuous(name = "Delta (Percepted Measure-True Measure)") +
  ggtitle("Aggregate Difference of measurement Delta for Subjects 56-73")

#f) can I find something interesting about the data on my own?
#who did the absolute worst overall?
#need a new column to get unique subject by trial
exp$UniqueSubject <- paste0(exp$Subject, exp$Trial)
p6 <- ggplot(data = (exp[exp$AbsDelta > 0.6,]),
             aes(x = factor(UniqueSubject), y = AbsDelta))
p6 + geom_bar(stat = "summary", fun.y = "median", position = "identity") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.2),
                     name = "Median of Abs. Value of Delta for All Tests") +
  scale_x_discrete(name = "Unique Subject (by Subject Number and Trial)") +
  ggtitle("Who is the worst at approximating?
          (Unique Subject w/ Median Abs. Delta > 0.6 Shown Only)")

#############################
#3. Intel Data
#############################

#load in data & fix date
intel <- read.csv(file = "datasets\\Intel1990-2000.csv", header = T, 
                  stringsAsFactors = F)
intel$Date <- as.Date(intel$Date,format = "%m/%d/%Y")

#a) create a line graph w/ thickness representing volume
p7 <- ggplot(data = intel, aes(x = Date, y = Adj.Close))
p7 + geom_line(aes(size = Volume/1000000)) +
  scale_y_continuous(name = "Adj. Close ($)") +
  guides(size = guide_legend(title = "Volume (in millions)")) +
  ggtitle("Intel Stock Price")

#b) make volume the color of the chart
p8 <- ggplot(data = intel, aes(x = Date, y = Adj.Close))
p8 + geom_line(aes(color = Volume/1000000), size = 3) +
  scale_color_gradient() +
  scale_y_continuous(name = "Adj. Close ($)") +
  guides(color = guide_legend(title = "Volume (in millions)")) +
  ggtitle("Intel Stock Price")

#c) turn the y into a log scale
p9 <- ggplot(data = intel, aes(x = Date, y = log(Adj.Close,2)))
p9 + geom_line(aes(size = Volume/1000000)) +
  scale_y_continuous(name = "Adj. Close ($)") +
  guides(size = guide_legend(title = "Volume (in millions)")) +
  ggtitle("Intel Stock Price")

#create a new dataset w/ by years only
intel$year <- year(intel$Date)
intel <- as.data.table(intel)
intelyear <- intel[i = T,
                   j = list(ClosePrice = mean(Adj.Close),
                            AnnualVol = sum(as.numeric(Volume))),
                   by = year]

p10 <- ggplot(data = intelyear, aes(x = year, y = log(ClosePrice,2)))
p10 + geom_line(aes(size = AnnualVol/1000000000)) +
  scale_x_continuous(breaks = seq(1990,2000,1)) +
  scale_y_continuous(name = "log_2 of Adj. Close", breaks = seq(-2,6,1), 
                     limit = c(-1,6)) +
  guides(size = guide_legend(title = "Volume (in billions)")) +
  ggtitle("Intel Stock Price")

###############################
#4. Montana Population dataset
###############################

#load in data
mtana <- read.csv("datasets\\MontanaPopulationData.csv", header = T)

#a) how many times has the population doubled?
#set the starting point
PopStart <- mtana[mtana$Year == min(mtana$Year),"Population"]
p11 <- ggplot(data = mtana, aes(x = Year, y = log(Population/PopStart,2)))
p11 + geom_line() + scale_y_continuous(limits = c(0,3),
                                       breaks = seq(0,3,1), 
                                       minor_breaks = seq(0,3,0.25),
                                       name = "log_2(Population / Starting Population 1890)") +
  scale_x_continuous(limits = c(1890, 2000),
                     breaks = seq(1890,2000,10),
                     minor_breaks = FALSE) +
  ggtitle("Montana Population Growth")

#b) lets measure the percentage rate of change
p12 <- ggplot(data = mtana, aes(x = Year, y = log(Population/PopStart)))
p12 + geom_line() + scale_y_continuous(limits = c(0,2),
                                       breaks = seq(0,2,0.2), 
                                       name = "ln(Population / Starting Population 1890)") +
  scale_x_continuous(limits = c(1890, 2000),
                     breaks = seq(1890,2000,10),
                     minor_breaks = FALSE) +
  ggtitle("Montana Population Growth")

#c) what yrs was % increase greater than 15%
p13 <- ggplot(data = mtana, aes(x = Year, y = log(Population/PopStart)))
p13 + geom_line() + scale_y_continuous(limits = c(0,1.95),
                                       breaks = seq(0,2,0.15), 
                                       minor_breaks = FALSE,
                                       name = "ln(Population / Starting Population 1890)") +
  scale_x_continuous(limits = c(1890, 2000),
                     breaks = seq(1890,2000,10),
                     minor_breaks = FALSE) +
  ggtitle("Montana Population Growth")

###############################
#5. Messier dataset
###############################

#load in data
mess <- read.csv("datasets\\MessierData.csv", header = T)
#cleanup column headers
colnames(mess) <- gsub("\\.", "", colnames(mess))
#need to order Kind by median distance
makeOrderNumeric = order(as.numeric(by(mess$DistanceLY,
                                       mess$Kind, median)))
mess$KindReordered = ordered(mess$Kind, levels(mess$Kind)[makeOrderNumeric])
#reorder my dataframe
mess <- mess[order(mess$KindReordered),]
mess$Numbered <- seq(1,110,1)

#a, b, c) plot the messier objects by distance
p14 <- ggplot(data = mess, aes(x = Numbered, y = log10(DistanceLY)))
p14 + geom_point(aes(color = KindReordered), size = 5, alpha = 0.75) + 
  scale_y_continuous(name = "log_10(Distance in light-years)",
                     breaks = seq(2,8,1), limits = c(2,8),
                     minor_breaks = seq(2,8,0.5)) +
  scale_x_discrete(name = "",labels = element_blank(),
                   breaks = element_blank(), expand = c(-1.05,1.05)) +
  ggtitle("Messier Object by Distance and Kind")

#d) create a scatter plot of distance vs visual magnitude
p15 <- ggplot(data = mess, aes(x = ApparentMagnitude, y = log10(DistanceLY)))
p15 + geom_point(aes(alpha = -ApparentMagnitude), size = 5) +
  scale_y_continuous(name = "log_10(Distance in light-years)",
                     breaks = seq(2,8,1), limits = c(2,8),
                     minor_breaks = seq(2,8,0.5)) +
  scale_x_continuous(name = "Apparent Magnitude (larger number represents
                     less visible item in sky)",
                     limits = c(1,13), breaks = seq(1,13,2)) +
  ggtitle("Messier Object by Distance and Apparent Magnitude")

###############################
#5. Portland Water level data
###############################

#load in the data
port <- read.csv(file = "datasets\\PortlandWaterLevel2003.csv", header = T,
                 stringsAsFactors = F)
#concatenate date & time
port$dateTime <- paste(port$Date, port$Time, sep=" ")
port$dateTime <- strptime(port$dateTime, format="%m/%d/%Y %H:%M")
port$Date <- as.Date(port$Date, format = "%m/%d/%Y")
port$Time <- gsub(":00","",port$Time)
#reorder Time levels
port$Time <- factor(port$Time)
port$Time <- factor(port$Time, levels(port$Time)[c(1:2,13,18:24,3:12,14:17)])


#a) show the progression of tides over period of time
p16 <- ggplot(data = port, aes(x = Time, y = Date, fill = WL))
p16 + geom_tile() + ggtitle("Water Level by Day and Hour in 2003") +
  scale_x_discrete(name = "Time (in hour of day)")


#b) plot a time series of WL
p17 <- ggplot(data = port, aes(x = dateTime, y = WL))
weights = rep(1/180, 180)
port$weightedAverage = filter(port$WL, weights, sides=2)
p17 + geom_line() + geom_line(color = "red",
                              aes(x = dateTime, y = port$weightedAverage))
