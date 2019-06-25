if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
install.packages("fiftystater")
library("fiftystater") # To add Alaska and Hawaii
library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps) # Creates high quality maps that may be shaded or projected in a variety of ways.

# Get a shape file of states in the US
usa.df <- fifty_states
colnames(usa.df)[6] <- "State"

# Get the data to be plotted
usa.dat <- read.csv("Data.csv", header = T, sep = ",")

#$ sign used to extract column names from the dataset
usa.dat$State <- tolower(usa.dat$State)
usa.dat$Clinton..<- as.numeric(gsub("%", "", usa.dat$Clinton..))
usa.dat$Trump..<- as.numeric(gsub("%", "", usa.dat$Trump..))
usa.dat <- usa.dat[,c("State", "Clinton..", "Trump..")]
usa.Hillary <- usa.dat[usa.dat$Clinton.. < 50 , c('State','Clinton..')]
usa.Donald <- usa.dat[usa.dat$Trump..>=50, c('State','Trump..')]

# Merge the data with the shape file

usa.df <- join(usa.df, usa.dat, by = "State", type = "inner")
usaDonald.df <- join(usa.df, usa.Donald, by = "State", type = "inner")
usaHillary.df <- join(usa.df, usa.Hillary, by = "State", type = "inner")


# Abbreviations of states and where thy should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usa.df$State # exclude Hawaii as there is no data for this state
states <- states[subset, ]


# A function that plots the map based on the data given
p1 <- function(data, title)
{
  
  ggp<- ggplot()+geom_polygon(data = usa.df, aes(x = long, y = lat, group = group, fill = Clinton..), color = "black", size = 0.15) +
    scale_fill_gradient2(midpoint=50,low="red3", mid="aliceblue",high="deepskyblue3",breaks=c(0,25,50,75,100),labels=c("Trump..","75","50","75","Clinton.."),limits=c(0,100))+
    theme_nothing(legend = TRUE) + labs(title = title, fill = "") + 
    
    
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 2)
  return (ggp)
}

figure.title <- "State-by-state percentage vote share of President Trump in
the 2016 US presidential election"

#saves the map to a file in jpg format named ElectionResult2016
ggsave(p1(usa.df, figure.title), height = 4, width = 4*1.9,
       file = "Elections2016.jpg")


