
# First we create some sample data.  What this simulates is two populations
# That are each tested against two separate treatments.  The populations are
# Groups 1 and 2, and the treatments are called A and B.  So there are four
# sets of observations 1-A, 1-B, 2-A and 2-B
n.each = 1000
ID = 1:1000
A1 = rnorm(n.each, 2, 1)
A2 = rnorm(n.each, 1.5, 2)
B1 = rnorm(n.each, 4, 1.5)
B2 = rnorm(n.each, 0, 1)
values = c(A1, A2, B1, B2)
treatment = rep(c("A", "B"), each=n.each*2)
group = rep(c(1, 2, 1, 2), each=n.each)
groupTreat = rep(c("A1", "A2", "B1", "B2"), each=n.each)

dataset = cbind(values, treatment, group)
write.table(dataset, file="distributionEx.csv", sep=",")

# First we turn off bounding boxes in the plot window to clean things up a bit
par(bty="n")

# Now we get a first view with a box plot.  We plot values based on the 
# interaction of group and treatment
boxplot(values ~ group*treatment, main="Box plot", 
        col=rep(c("purple", "lightblue"), 2))

# First we look at a basic violin plot feature in ggplot2.  The options
# Here are pretty limited, but it can give a nice quick look.
require(ggplot2)
qplot(factor(groupTreat), values, geom="violin", fill=factor(group))

# Now we look at a more full featured violin plot function built in its own
# setparate package "vioplot".  You will need to download and install it.
# This adds a box-plot in the center ofthe violin plot to give us more information.
require(vioplot)
vioplot(A1, B1, A2, B2, names=c("A1", "B1", "A2", "B2"), col="cyan")

# For our last improvement, we use a function generously posted by someone on 
# the internet.  It is provided in a "violinPlotHelper.R" file in this folder
# The file contains a comment with the website of the source.

# First we create a blank plot area.  Note that we set the limits on the 
# plot area here.  In x, since the x-varable is categorical, we will be
# using A and B with two bars each, one set around x = 1 and one set around
# x = 2.  What makes this confusing is that they will be labeled A and B
# but remember that since they are appearing at a certain place horizontally
# in the graph that they have an underlying coordinate.
plot(x=NULL, y=NULL, xlim = c(0.5, 2.5), ylim=c(min(values), max(values)),
     type="n", ann=FALSE, axes=F)

# Now we give a set of axes.  Note how we label x-values 1 and 2 here.  This
# matches what we said above.
axis(1, at=c(1, 2),  labels=c("A", "B"))
axis(2)

# Now we go through each of the treatments and draw half-violin plots for 
# each.  The A treatments are divided into the two groups, group 1 on the 
# left and group 2 on the right.  The main purpose of the vioplot2 helper
# function defined in this folder is to get only a half-plot so we can
# plot two of these side-by-side.

# !!!!! Requires ViolinPlotHelper.R

for (i in unique(treatment)) 
{
  for (j in unique(group))
  {
    vioplot2(values[which(treatment == i & group == j)],
             at = ifelse(i == "A", 1, 2),
             side = ifelse(j == 1, "left", "right"),
             col = ifelse(j == 1, "lightgreen", "lightblue"),
             add = T)
  }
}
title("Violin plot", xlab="Treatment")
legend("bottomright", fill = c("lightgreen", "lightblue"),
       legend = c("Group 1", "Group 2"), box.lty=0)

