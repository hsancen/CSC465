
# First you need to navigate to the folder holding
# both this script and the gmcar_price.txt" dataset
# Make sure you "set as working directory"
# in the R-Studio file pane's "More" menu.

ds = read.table("gmcar_price.txt", header=T, sep='\t')

plot(ds$Mileage, ds$Price)

# We can also color by the Make if we specify the optional
# 'col' parameter and give it the Make field

plot(ds$Mileage, ds$Price, col=ds$Make)

# Now we can add a best-fit (regression) line to the data
# using the 'lm' linear model function.  Note the ordering
# of the parameters here and the fact that they are connected
# with a ~ rather than ,

fit = lm(ds$Price ~ ds$Mileage)
abline(fit)

# If we want to plot only the buicks in the dataset, 
# we can make a subset of the data and then plot that
# Note that there are several ways to do this including
# the subset function shown below and also the "which" 
# function

dBuick = subset(ds, Make=='Buick')
plot(dBuick$Mileage, dBuick$Price)

