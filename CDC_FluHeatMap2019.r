# --------------- Load all required libraries

library(ggplot2)
library(reshape2)
library(maps)
library(mapproj)

#-----------Load the map coordinates as a data frame into "states". Argument "state"- States of the US
getwd()
setwd("D:/Study Material/Projects/Twitter Flu Analysis/Outputs")
states <- map_data("state")
head(states)
map('state', fill=TRUE, col=palette())

write.csv(states,"states.csv")
nrow(states)
heatmap_data <- read.csv(file.choose()) #-- StateDataforMap_2018-19week6.csv downloaded from CDC website
head(heatmap_data)

# -------- COnvert the names of the states to lower case as map_data outputs states in lowercase
heatmap_data$STATENAME = tolower(heatmap_data$STATENAME)
write.csv(heatmap_data,"CDCHeatmapWithLowerCase.csv")


# --  join "states" and "heatmap_data" into new Data Frame "merged_states" to add the lat and lon to the "states" data frame
merged_states <- merge(states, heatmap_data, by.x = "region",by.y = "STATENAME")  #joined "states" and "heatmap_data" to add te lat and lon to the state data frame
write.csv(merged_states,"merged_states.csv")
head(merged_states)

map1 <- ggplot(merged_states, aes(x = long, y = lat, group = group))+
        geom_polygon(aes(fill=ACTIVITY.LEVEL))+
        coord_map()+ggtitle("2018-19 Influenza Season Week 6")+
        theme(plot.title = element_text(hjust = 0.5))+
        guides(fill=guide_legend(title="Activity Level"))

map1 #-- CDC_FluHeatMap2019.png

