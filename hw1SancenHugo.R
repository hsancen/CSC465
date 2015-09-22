#CSC465 - Homework 1
#Author: Hugo Sancen
#09/29/15

#prep required packages
require("ggplot2")

##################################
#Problem 5: Intel-1998 dataset
##################################

#load in data
intel <- read.csv("datasets\\Intel-1998.csv", header = T, stringsAsFactors = F)

#a) closing price vs date
qplot(Date, Close, data = intel, geom = c("point","smooth"), aes(group =1))
