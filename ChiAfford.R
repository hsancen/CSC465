#This code explores the Chicago Institute for Housing dataset and will
#produce a visualization that provides insight on home/rent affordability
#by neighborhood or Census Tract

#Initialization borrowed from Gene Leynes @geneorama
#===================================================

## Remove all objects; perform garbage collection
rm(list=ls())
gc(reset=TRUE)

## Check for dependencies
if(!"geneorama" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){
    install.packages('devtools')
  }
  devtools::install_github('geneorama/geneorama')
}

## Load libraries
geneorama::detach_nonstandard_packages()
# geneorama::loadinstall_libraries(c("geneorama", "knitr", "caret", "gbm", 
#     						   "glmnet", "ROCR", "pROC", "plyr", "class", 
# 								   "hmeasure", "randomForest", 
# 								   "AppliedPredictiveModeling", "data.table", 
# 								   "doParallel", "e1071", "rpart"))
geneorama::loadinstall_libraries(c("geneorama", "knitr", "data.table", "ggplot2"))
#===================================================


#Load in & prep data
#===================================================

chi <- read.csv(file = "chicago.csv", header = T, stringsAsFactors = F)
chi <- as.data.table(chi)
str(chi)

#variables of interest appear to be first 8 columns and last 2
chi <- chi[,c(1:8,56:57), with = F]
str(chi)

#gross rent and home value should be integers. change "-" to 0
chi[Median.Gross.Rent == "-", Median.Gross.Rent := "0"]
chi[Median.Home.Value == "-", Median.Home.Value := "0"]
#convert these variables to numeric
chi[,Median.Gross.Rent := as.numeric(Median.Gross.Rent)]
chi[,Median.Home.Value := as.numeric(Median.Home.Value)]
str(chi)

#let's look at some of the neighborhoods
unique(chi$City..Suburb)
NROW(unique(chi$Community.Area))
#there are 77 neighborhoods. how are the Census Tracts distributed across
#these neighborhoods?
p.hoods <- ggplot(data = chi)
p.hoods + geom_bar(aes(x = Community.Area, y = ..count..))

#bar chart not a good way to look at them. too many neighborhoods. way too
#many tracts.


#if i want to look at rents. do I have any outliers?
p.rents <- ggplot(data = chi)
p.rents +
  geom_histogram(aes(x = Median.Gross.Rent)) +
  scale_x_continuous(name = "Median Gross Rent") +
  ggtitle("Distribution of Median Gross Rent per Chicago Census Tracts")


#Lets start looking at maps instead
#===================================================

#leaflet
#===================================================
#load req'd packages
geneorama::loadinstall_libraries(c("leaflet", "magrittr",
                                   "rgdal", "RColorBrewer"))


#load in Chicago community areas & tracts shape file
hoods <- readOGR(dsn = ".",layer = "chicomm", verbose = F)
tracts <- readOGR(dsn = ".", layer = "cb_2014_17_tract_500k", verbose = F)
#format community area strings to map in data later
hoods$DISTITLE <- as.character(hoods$DISTITLE)
hoods$DISTITLE <- toupper(hoods$DISTITLE)

#check to see if the community names are the same
sort(hoods$DISTITLE) == sort(unique(chi$Community.Area))

#this will subset the tracts to only include the tracts in the chi dataset
tracts <- subset(tracts, tracts$GEOID %in% chi$TRACTFIPS10)

#this is how to append data to a spdf. appending attributes from chi dataset
#sp@data = data.frame(sp@data, df[match(sp@data$col, df$col),])
tracts@data = data.frame(tracts@data, chi[match(tracts@data$GEOID, chi$TRACTFIPS10),])


#create a color palette
pal <- colorNumeric(palette = brewer.pal(7,"PuBu"),
                    domain = tracts@data$Median.Gross.Rent)

#concatenate strings for a more descriptive popup
tract_popup <- paste0("<p>Neighborhood: ",
                      tracts@data$Community.Area,
                      "</p>",
                      "Median Gross Rent: $",
                      as.character(tracts@data$Median.Gross.Rent))


#test plot
p.test <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = hoods,
              smoothFactor = 0.2,
              stroke = T,
              fillOpacity = 0,
              color = "blue") %>%
  addPolygons(data = tracts,
              smoothFactor = 0.2,
              stroke = F,
              fillOpacity = 0.75,
              color = ~pal(Median.Gross.Rent),
              popup = tract_popup)
#   addCircleMarkers(data = chi,
#                    lng = ~LON,
#                    lat = ~LAT,
#                    popup = ~as.character(Median.Household.Income))
  
p.test

#how to concatenate strings for a more descriptive popup
# state_popup <- paste0("<strong>Estado: </strong>", 
#                       mexico$name, 
#                       "<br><strong>PIB per c?pita, miles de pesos, 2008: </strong>", 
#                       mexico$gdp08)


#===================================================