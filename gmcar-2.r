
library("ggplot2")
ds = read.table("gmcar_price.txt", header=T, sep='\t')

##############################################################
# Scatter plots
##############################################################

# From last time
plot(ds$Mileage, ds$Price, col=ds$Make)

# ggplot (and its convenience function 'qplot') is a more powerful visualization package for R
# the syntax of q-plot is similar to plot, but it allows us to set the "context" of the plot
# with a 'data=ds' command.  With this, fields are allowed to be referred to without the leading 'ds$'
qplot(Mileage, Price, data=ds)

# Like plot, we can add options for color and size.  All properties can be set either to a constant
# or to a field
qplot(Mileage, Price, col=Make, data=ds)
qplot(Mileage, Price, col=Make, size = 5, data=ds)
qplot(Mileage, Price, col=Make, size = Doors, data=ds)

# Now we change the scale by "adding" an option to the scale_size.  This is a very common syntax
# in ggplot that we will see more of as we go
qplot(Mileage, Price, col=Make, size = Doors, data=ds) + scale_size(range = c(3, 4))

##############################################################
# Line Graphs
##############################################################

x = seq(0, 10, by=.1)
y = x * x + rnorm(length(x)) * 1.0
qplot(x, y, col="red", geom="line")

qplot(x, y, col="red", geom="line") + geom_line(size=2)

qplot(x, y, col=y, geom="line") + geom_line(size=2)

qplot(x, y, col="red", geom="line") + geom_line(size=2) + 
  scale_x_continuous(limits=c(-2, 10), breaks = 1:10) +
  scale_y_continuous(limits = c(-10, 110)) + theme_minimal()


##############################################################
# Box plots
##############################################################

qplot(Make, Price, data=ds, geom="boxplot")

qplot(Make, Price, data=ds, geom="boxplot") + 
      geom_boxplot(fill="yellow", color="blue") + 
      geom_point(color="red")

qplot(Make, Price, data=ds, geom="boxplot") + 
  geom_boxplot(fill="yellow", color="blue") + 
  geom_point(position=position_jitter(w=.1, h = 0), col="red", alpha=.5, size=2)

qplot(Make, Price, data=ds, geom="boxplot") + 
  geom_boxplot(fill="yellow", color="blue", notch=T) + 
  geom_point(position=position_jitter(w=.1, h = 0), col="red", alpha=.5, size=2)


