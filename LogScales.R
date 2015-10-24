
ds = read.table("datasets\\WorldPopulation.csv", header=T, sep=",")

require(ggplot2)

head(ds)

# With base plotting (Note that we are subsetting so that we only
# consider A.D. years)
plot(ds$year[13:111], ds$Population[13:111], type="l", 
     ylab="Population (Millions)", xlab="Year (A.D.)")

# We can plot with a logarithmic scale by setting the "log" parameter
# to indicate which axes we want.  Either "x", "y" or "xy", in this
# case we only want the y-axis to be a logarithmic scale.
#
# Note that the base functionality selects some nice divisions
# for the log scale that are readable.
plot(ds$year[13:111], ds$Population[13:111], type="l", log="y",
     ylab="Population (Millions)", xlab="Year (A.D.)")

# For scientific audiences, we might want them to be able to read 
# certain differences right off the log scale (like doubling, or 
# small percentage change).  In this case we want the scale to reflect
# the log of the value, not the actual value.  
#
# We do this by taking the log of the field (which takes the log of
# each individual sample).  The log function lets us specify the base
# as an optional second parameter, the default is 10.
plot(ds$year[13:111], log(ds$Population[13:111], 2), 
     type="l", ylab="log(Population (Millions))", xlab="Year (A.D.)")

# This graph compares the popluation as percentage growth from the initial
# population in year 1.  How many times has the population doubled since 
# year 1?
plot(ds$year[13:111], log(ds$Population[13:111] / ds$Population[13], 2), 
     type="l", ylab="log(Population (Millions))", xlab="Year (A.D.)")





# Or in ggplot.  Notice that we can grab the plotting surface with 
# data attached from ggplot as its return value and then add various
# data representations to the graph.  The graph will not display
# until we have added some kind of geometry.
p = ggplot(ds[13:111, ], aes(x=year, y=Population))
p + geom_line(color="red", size=1.3) + ylab("Population (Millions)")

# We can change the scale to logarithmic by setting the type of scale
# one of the built-in types is the log_10 scale showing order of magnitude
# 
# Notice how unsatisfying the y-scale is here.  The reason is that 
# ggplot is using a log_10 scale, and it is only giving us one tick mark 
p + geom_line(color="red", size=1.3) + scale_y_log10() + ylab("Population (Millions)")

# You can get a better set of subdivisions using the "coord_trans" 
# modifier to the graph
p + geom_line(color="red", size=1.3) + coord_trans(y = "log10") + ylab("Population (Millions)")


# Just like the base plotting we can also just transform the underlying 
# variable and see the logs on the axis instead
p = ggplot(ds[13:111, ], aes(x=year, y=log(Population, 2)))
p + geom_line(color="red", size=1.3) + ylab("log_2(Population (Millions))")

# Or to get the growth from the start value.  Remember here that since we've already
# subsetted the dataset from 13:111, the first population is "Population[1]"
p = ggplot(ds[13:111, ], aes(x=year, y=log(Population / Population[1], 2)))
p + geom_line(color="red", size=1.3) + ylab("log_2(Population / Year 1 Population)")
