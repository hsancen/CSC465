
# First we create some sample data.  

n.each = 1000
ID = 1:1000
dataA = rnorm(n.each, 2, 1)
selector = round(runif(n.each, 0, 1))
dataB = (1.0 - selector) * rnorm(n.each, 1.2, .25) + selector * rnorm(n.each, 2.8, .25)
data = c(dataA, dataB)
values = data
dataGroup = rep(c("A", "B"), each=n.each)
numericGroup = rep(c(1, 2), each=n.each)

par(mfrow=c(1, 1))

# We can view and compare the summary statistics with a box plot.  
boxplot(data ~ dataGroup, main="Box plot", 
        col=rep(c("lightgreen", "lightblue"), 2))

# If we want to get a deeper view of the distribution we can add the data as a 1D histogram.
# The main trick here is that the categories in the box plot are mapped to 1 and 2, not 0 
# and 1.  Also we jitter the x-values a bit with the rnorm function and then give them some
# transparency so we can see their clustering a bit better.
points(numericGroup + rnorm(n.each * 2, 0, .05), col=rgb(1, 0, 0, .2), alpha=.5, data)

# First we turn off bounding boxes in the plot window to clean things up a bit
par(bty="n")

par(mfrow=c(2, 1))
hist(dataA, col="lightgreen")
hist(dataB, col="cyan")

require(ggplot2)

# ggplot has a very nice feature for overlaying plots.  First, using qplot
par(mfrow=c(1, 1))
qplot(data, fill=dataGroup, alpha=.5, position="Identity")

# Or with the full ggplot command.  
#
# The first important point is that ggplot ONLY deals with data frames
# the only trick here is that we cannot put the two columns in a list 
# with c(,) before creating the data frame because lists have to be 
# of uniform type.  So we create the dataframe with one column and then
# add the second column afterwards.
ds = as.data.frame(values)
ds$group = as.factor(dataGroup)
ggplot(ds, aes(values, fill = dataGroup)) + geom_histogram(alpha = 0.5, position = 'identity')

# Or graph them as a density histogram (counts as percentages of 100%)
ggplot(ds, aes(values, fill = dataGroup)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

# Better yet, we can smooth out the data and display fitted density curves that simulate
# what the data would look like for a large number of samples!
ggplot(ds, aes(values, fill = group)) + geom_density(alpha=.3)

# First we look at a basic violin plot feature in ggplot2.  The options
# Here are pretty limited, but it can give a nice quick look.
require(ggplot2)
qplot(dataGroup, data, geom="violin", fill=factor(dataGroup))

# Now we look at a more full featured violin plot function built in its own
# setparate package "vioplot".  You will need to download and install it.
# This adds a box-plot in the center ofthe violin plot to give us more information.
require(vioplot)
vioplot(dataA, dataB, names=c("A", "B"), col=c("lightgreen", "cyan"))

# If we want to color these differently, we have to use multiple vioplot 
# commands with different colors

# set up frame, without axes
plot(1,1,xlim=c(.5,2.5),ylim=range(data),type="n",
     xlab="",ylab="",axes=FALSE)

## bottom axis, with user-specified labels
axis(side=1,at=1:2,labels=c("A","B"))
axis(side=2)
vioplot(dataA,at=1,col="lightgreen",add=TRUE)
vioplot(dataB,at=2,col="cyan",add=TRUE)

require(beanplot)
# The "what" parameter controlls whether four elements are displayed
# I've taken out the total average line and the bean average lines
beanplot(dataA, dataB, what=c(0, 1, 0, 1), ll=.01, 
         col=list("lightgreen", "cyan"))

