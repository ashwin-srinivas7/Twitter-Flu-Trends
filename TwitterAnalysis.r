
setwd("D:/Study Material/Projects/Twitter Flu Analysis/Outputs")

# ------------ Install and load all libraries
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("mkearney/rtweet") #install dev version of rtweet

install.packages("httpuv")
install.packages("stringi")   # for using stri_enc_toutf8() to encode the string
library(stringi)
library(rtweet)
library(ggmap)
library(RJSONIO)
library(ggplot2)
library(maps)

# --------- Create the twitter token using create_token() for accessing twitter data
## Before doing that, set the callback URL for the application in dev.twitter.com as http://127.0.0.1:1410

appname <- "Ash_Lab1"
key <- "XGrXBWBAL5HnqdlDm9LNTiN6C"
secret <- "BQaArLP6Lsc7RGDNQa7QhTWx8tN0c7xGxhz5lDppfX9ZVmqHrU"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

## ---------- Test code to extract tweets with #seattle 
#rt <- data.frame(search_tweets(
 # "#seattle", n = 180, include_rts = FALSE
#))

#rt <- apply(rt,2,as.character)   #include this line to avoid error while exporting to csv
#write.csv(rt,file="rt.csv")
#head(rt)
#class(rt)


#---------------------------------------------------------


#come up with keywords for twitter searches about flu
t_keys <- cbind("flu sick",
                "flu vaccine",
                "flu virus",
                "flu ill",
                "flu doctor",
                "flu symptom",
                "fly doctor",
                "flu season",
                "flu outbreak",
                "flu cases",
                "flu areas",
                "flu foods",
                "flu effects",
                "flu fever",
                "flu cough",
                "flu catch",
                "flu feeling",
                "flu hospital",
                "flu type",
                "flu influenza",
                "flu contagious",
                "flu sucks")

for(i in 1:22)
{
  tweet_info <- search_tweets(t_keys[,i], geocode = lookup_coords("usa"), retryonratelimit = TRUE,include_rts = FALSE, n=10000)
  user_info <- users_data(tweet_info)
  ai <- paste("A", i, sep = "")
  bi <- paste("B", i, sep = "")
  assign(ai, tweet_info)
  assign(bi, user_info)
  
}

twitter_data <- rbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22)
twitter_data<- apply(twitter_data,2,as.character)
twitter_data <- data.frame(twitter_data)
save_as_csv(twitter_data,"twitter_data.csv")

user_data <- rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,B17,B18,B19,B20,B21,B22)
user_data<- apply(user_data,2,as.character)
user_data <- data.frame(user_data)
user_data$location = tolower(user_data$location)  #convert locations to lowercase
save_as_csv(user_data,"finalUser_data.csv")

userData_locations <- user_data$location          ##store the locations in a different vector

userData_locations <- userData_locations[userData_locations!=""]  #remove blank values from this vector
head(userData_locations)

write.csv(userData_locations,"userlocations.csv")
#cleaned data - removed special characters manually on Excel
register_google(key="AIzaSyAreX0LejrXWQqZqYQbFMfCmxFLVI6zFPE")  #register with google before using geocode to get latitude and longitude


latlon_locations <- c()
head(latlon_locations)
userData_locations <- read.csv(file.choose()) # userlocations.csv - manually clean this file. remove special ASCII characers from location
head(userData_locations)
userData_locations <- apply(userData_locations,2,as.character)
userData_locations <- userData_locations[userData_locations!=""]
latlon_locations <- geocode(userData_locations)  ##get latitude and longitude using google API
head(latlon_locations)
latlon_locations <- apply(latlon_locations,2,as.character)
write.csv(latlon_locations,"coordinatesOfTweets.csv")

## ----------------------- REVERSE GEOCODING (convert lat lon coordinates into states)

latlon_locations <- read.csv(file.choose()) #choose "coordinatesOfTweets.csv. Did this as my global environment was cleared.

latlon_locations <- latlon_locations[complete.cases(latlon_locations), ] #keep only complete cases i.e. remove NA rows

# latlon_locations[1,]


## drop the first column 
drops <- c("X")

latlon_locations <- latlon_locations[ , !(names(latlon_locations) %in% drops)]

## swap the latitude and longitutes as the API takes latitude and longitude
temp <- latlon_locations$lon
latlon_locations$lon <- latlon_locations$lat
latlon_locations$lat <- temp

## Note that the column labels will not change. only the values will be swapped. 

nrow(latlon_locations)

data.json <- c()
i<-0
for(i in 1:23503){
  #replace the spaces with %20 and append the lat lon string. This is going to be appended to the URL below to obtain the political area data
  latlngStr <- gsub(' ','%20', paste(latlon_locations[i,], collapse=",")) #collapse the commas as it is a csv
  #when the lat lon string is appended to this URL, the API will obtain data in the JSON format
  connectStr <- paste("http://www.datasciencetoolkit.org/coordinates2politics/",latlngStr, sep="")
  con <- url(connectStr)
  data.json[i] <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
}

data.json1 <- data.json
d <- unlist(data.json1)
i <- 0
j <- c()
for(i in 1:length(d)){
  if(d[i] == "state"){
    j <- rbind(j,d[i+1])           ## Bind all the states together into vector j from the JSON object
  }
}

write.table(j,"FinalLocationsList.csv")

j1 <- read.csv(file.choose())            ##Closed workspace so choose the file "FinalLocationsList.csv

head(j1)
j1$count <- rep(1,nrow(j1)) # make new column called "count" and give value 1 to each row to count

agg_j1 <- aggregate(newcol ~ politics.name, data = j1, FUN = function(x)sum(x, na.rm = TRUE)) 


head(agg_j1)
nrow(agg_j1)

head(j1)

write.csv(agg_j1,"FinalCountOfStates.csv")

states <- map_data("state")
agg_j1$region <- tolower(agg_j1$State)
map.df <- merge(states,agg_j1, by="region", all.x=T)   ## Merge the states obtaines from JSON to the "state" object by region
head(map.df)
map.df <- map.df[order(map.df$order),]
# ----------- Plot the heatmap

map1 <- ggplot(map.df, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=count))+
  coord_map()+ggtitle("2018-19 Influenza Season Twitter Data")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Activity Level"))


map1
