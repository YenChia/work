library(jsonlite)
library(ggplot2)
library(plyr)
library(dplyr)

jsonData <- fromJSON("台灣縣市輪廓經緯度.geojson")

city <- jsonData$features$properties
names(city)[2] <- "code"
city$id <- 1:nrow(city)
city$sale <- round(rnorm(nrow(city),100,20),0)

cityData <- jsonData$features$geometry$coordinates
mapdata <- data.frame()
for (i in 1:length(cityData)) {
  citymapdata <- cityData[[i]]
  dim(citymapdata)=c(length(citymapdata)/2,2)
  citymapdata <- data.frame(citymapdata)
  names(citymapdata)[1:2] <- c("lon", "lat")
  citymapdata$id <- i
  citymapdata$group <- as.numeric(paste0(i,".",1))
  citymapdata$order <- 1:dim(citymapdata)[1]
  mapdata <- rbind(mapdata, citymapdata)
}

mymapdata <- merge(mapdata, city)
lon <- sapply(mymapdata[,"lon"], "[", 1)
lat <- sapply(mymapdata[,"lat"], "[", 1)
mymapdata[,2:3] <- NULL
mymapdata <- cbind(mymapdata, lon, lat)

a <- sapply(jsonData$features$geometry$coordinates[[12]][1], "[")
b <- data.frame(lon = a[1:(length(a)/2)], lat = a[(length(a)/2+1):1768])
mymapdata <- mymapdata[-which(mymapdata[,"County_ID"]==65),]
b <- cbind(b, group = 12.1, order = 1, Shape_Leng = 4.920444, code = 0.184777, C_Name = "新北市", County_ID = 65, name = "新北市", sale = 72, id = 1)
mymapdata <- rbind(mymapdata , b)

midpos <- function(x) mean(range(x, na.rm = TRUE))
centres <- ddply(mymapdata, .(name), colwise(midpos, .(lon, lat)))

ggplot(mymapdata, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = sale), colour = "grey95") +
  scale_fill_gradient(low = "black", high = "steelblue") +
  geom_text(aes(label = name), data = centres) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(mymapdata, aes(lon, lat, group = group, fill = sale)) +
  geom_polygon(colour = "grey95") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

data <- mymapdata[,c("name", "County_ID", "lon", "lat")]
write.table(data, file = "台灣縣市邊界經緯度座標.csv", sep = ",", row.names = FALSE)

#########################
#########################
####### 換算縣市 ########  經度在前12X.XXX  緯度在後2X.XXX
#########################
#########################

library(readxl)
library(stringr)
library(reshape2)

mData <- read.csv(file = "台灣縣市邊界經緯度座標.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "character", "numeric", "numeric"))
a <- unique(mData[,2])
comparison <- data.frame(name = unique(mData[,1]), com = 0, stringsAsFactors = FALSE)
data10002 <- mData[mData[,2]==a[1],];data10002_01 <- rbind(data10002[2:nrow(data10002),], data10002[1,])
data10015 <- mData[mData[,2]==a[2],];data10015_01 <- rbind(data10015[2:nrow(data10015),], data10015[1,])
data09020 <- mData[mData[,2]==a[3],];data09020_01 <- rbind(data09020[2:nrow(data09020),], data09020[1,])
data10008 <- mData[mData[,2]==a[4],];data10008_01 <- rbind(data10008[2:nrow(data10008),], data10008[1,])
data10013 <- mData[mData[,2]==a[5],];data10013_01 <- rbind(data10013[2:nrow(data10013),], data10013[1,])
data10005 <- mData[mData[,2]==a[6],];data10005_01 <- rbind(data10005[2:nrow(data10005),], data10005[1,])
data68000 <- mData[mData[,2]==a[7],];data68000_01 <- rbind(data68000[2:nrow(data68000),], data68000[1,])
data64000 <- mData[mData[,2]==a[8],];data64000_01 <- rbind(data64000[2:nrow(data64000),], data64000[1,])
data10017 <- mData[mData[,2]==a[9],];data10017_01 <- rbind(data10017[2:nrow(data10017),], data10017[1,])
data09007 <- mData[mData[,2]==a[10],];data09007_01 <- rbind(data09007[2:nrow(data09007),], data09007[1,])
data10009 <- mData[mData[,2]==a[11],];data10009_01 <- rbind(data10009[2:nrow(data10009),], data10009[1,])
data10018 <- mData[mData[,2]==a[12],];data10018_01 <- rbind(data10018[2:nrow(data10018),], data10018[1,])
data10004 <- mData[mData[,2]==a[13],];data10004_01 <- rbind(data10004[2:nrow(data10004),], data10004[1,])
data10020 <- mData[mData[,2]==a[14],];data10020_01 <- rbind(data10020[2:nrow(data10020),], data10020[1,])
data10010 <- mData[mData[,2]==a[15],];data10010_01 <- rbind(data10010[2:nrow(data10010),], data10010[1,])
data10007 <- mData[mData[,2]==a[16],];data10007_01 <- rbind(data10007[2:nrow(data10007),], data10007[1,])
data66000 <- mData[mData[,2]==a[17],];data66000_01 <- rbind(data66000[2:nrow(data66000),], data66000[1,])
data63000 <- mData[mData[,2]==a[18],];data63000_01 <- rbind(data63000[2:nrow(data63000),], data63000[1,])
data10014 <- mData[mData[,2]==a[19],];data10014_01 <- rbind(data10014[2:nrow(data10014),], data10014[1,])
data67000 <- mData[mData[,2]==a[20],];data67000_01 <- rbind(data67000[2:nrow(data67000),], data67000[1,])
data10016 <- mData[mData[,2]==a[21],];data10016_01 <- rbind(data10016[2:nrow(data10016),], data10016[1,])
data65000 <- mData[mData[,2]==a[22],];data65000_01 <- rbind(data65000[2:nrow(data65000),], data65000[1,])
rm(mData)

###########################
######## 單點測試 #########
###########################

a <- c(121.281103, 23.912634)
comparison[comparison[,1]==data10002[1,1],2] <- mean(sign((data10002[,4] - data10002_01[,4]) * (a[1] - data10002[,3]) - (a[2] - data10002[,4]) * (data10002[,3] - data10002_01[,3]))[1:(nrow(data10002)-1)])
comparison[comparison[,1]==data10015[1,1],2] <- mean(sign((data10015[,4] - data10015_01[,4]) * (a[1] - data10015[,3]) - (a[2] - data10015[,4]) * (data10015[,3] - data10015_01[,3]))[1:(nrow(data10015)-1)])
comparison[comparison[,1]==data09020[1,1],2] <- mean(sign((data09020[,4] - data09020_01[,4]) * (a[1] - data09020[,3]) - (a[2] - data09020[,4]) * (data09020[,3] - data09020_01[,3]))[1:(nrow(data09020)-1)])
comparison[comparison[,1]==data10008[1,1],2] <- mean(sign((data10008[,4] - data10008_01[,4]) * (a[1] - data10008[,3]) - (a[2] - data10008[,4]) * (data10008[,3] - data10008_01[,3]))[1:(nrow(data10008)-1)])
comparison[comparison[,1]==data10013[1,1],2] <- mean(sign((data10013[,4] - data10013_01[,4]) * (a[1] - data10013[,3]) - (a[2] - data10013[,4]) * (data10013[,3] - data10013_01[,3]))[1:(nrow(data10013)-1)])
comparison[comparison[,1]==data10005[1,1],2] <- mean(sign((data10005[,4] - data10005_01[,4]) * (a[1] - data10005[,3]) - (a[2] - data10005[,4]) * (data10005[,3] - data10005_01[,3]))[1:(nrow(data10005)-1)])
comparison[comparison[,1]==data68000[1,1],2] <- mean(sign((data68000[,4] - data68000_01[,4]) * (a[1] - data68000[,3]) - (a[2] - data68000[,4]) * (data68000[,3] - data68000_01[,3]))[1:(nrow(data68000)-1)])
comparison[comparison[,1]==data64000[1,1],2] <- mean(sign((data64000[,4] - data64000_01[,4]) * (a[1] - data64000[,3]) - (a[2] - data64000[,4]) * (data64000[,3] - data64000_01[,3]))[1:(nrow(data64000)-1)])
comparison[comparison[,1]==data10017[1,1],2] <- mean(sign((data10017[,4] - data10017_01[,4]) * (a[1] - data10017[,3]) - (a[2] - data10017[,4]) * (data10017[,3] - data10017_01[,3]))[1:(nrow(data10017)-1)])
comparison[comparison[,1]==data09007[1,1],2] <- mean(sign((data09007[,4] - data09007_01[,4]) * (a[1] - data09007[,3]) - (a[2] - data09007[,4]) * (data09007[,3] - data09007_01[,3]))[1:(nrow(data09007)-1)])
comparison[comparison[,1]==data10009[1,1],2] <- mean(sign((data10009[,4] - data10009_01[,4]) * (a[1] - data10009[,3]) - (a[2] - data10009[,4]) * (data10009[,3] - data10009_01[,3]))[1:(nrow(data10009)-1)])
comparison[comparison[,1]==data10018[1,1],2] <- mean(sign((data10018[,4] - data10018_01[,4]) * (a[1] - data10018[,3]) - (a[2] - data10018[,4]) * (data10018[,3] - data10018_01[,3]))[1:(nrow(data10018)-1)])
comparison[comparison[,1]==data10004[1,1],2] <- mean(sign((data10004[,4] - data10004_01[,4]) * (a[1] - data10004[,3]) - (a[2] - data10004[,4]) * (data10004[,3] - data10004_01[,3]))[1:(nrow(data10004)-1)])
comparison[comparison[,1]==data10020[1,1],2] <- mean(sign((data10020[,4] - data10020_01[,4]) * (a[1] - data10020[,3]) - (a[2] - data10020[,4]) * (data10020[,3] - data10020_01[,3]))[1:(nrow(data10020)-1)])
comparison[comparison[,1]==data10010[1,1],2] <- mean(sign((data10010[,4] - data10010_01[,4]) * (a[1] - data10010[,3]) - (a[2] - data10010[,4]) * (data10010[,3] - data10010_01[,3]))[1:(nrow(data10010)-1)])
comparison[comparison[,1]==data10007[1,1],2] <- mean(sign((data10007[,4] - data10007_01[,4]) * (a[1] - data10007[,3]) - (a[2] - data10007[,4]) * (data10007[,3] - data10007_01[,3]))[1:(nrow(data10007)-1)])
comparison[comparison[,1]==data66000[1,1],2] <- mean(sign((data66000[,4] - data66000_01[,4]) * (a[1] - data66000[,3]) - (a[2] - data66000[,4]) * (data66000[,3] - data66000_01[,3]))[1:(nrow(data66000)-1)])
comparison[comparison[,1]==data63000[1,1],2] <- mean(sign((data63000[,4] - data63000_01[,4]) * (a[1] - data63000[,3]) - (a[2] - data63000[,4]) * (data63000[,3] - data63000_01[,3]))[1:(nrow(data63000)-1)])
comparison[comparison[,1]==data10014[1,1],2] <- mean(sign((data10014[,4] - data10014_01[,4]) * (a[1] - data10014[,3]) - (a[2] - data10014[,4]) * (data10014[,3] - data10014_01[,3]))[1:(nrow(data10014)-1)])
comparison[comparison[,1]==data67000[1,1],2] <- mean(sign((data67000[,4] - data67000_01[,4]) * (a[1] - data67000[,3]) - (a[2] - data67000[,4]) * (data67000[,3] - data67000_01[,3]))[1:(nrow(data67000)-1)])
comparison[comparison[,1]==data10016[1,1],2] <- mean(sign((data10016[,4] - data10016_01[,4]) * (a[1] - data10016[,3]) - (a[2] - data10016[,4]) * (data10016[,3] - data10016_01[,3]))[1:(nrow(data10016)-1)])
comparison[comparison[,1]==data65000[1,1],2] <- mean(sign((data65000[,4] - data65000_01[,4]) * (a[1] - data65000[,3]) - (a[2] - data65000[,4]) * (data65000[,3] - data65000_01[,3]))[1:(nrow(data65000)-1)])
comparison[which(abs(comparison[,2])==max(abs(comparison[,2]))),1]

###########################
######## 檔案判定 #########
###########################

# tData <- as.data.frame(read_excel(path = "", sheet = 1, skip = 1, col_names = TRUE))
tData[,8] <- as.numeric(tData[,8])
tData[,9] <- as.numeric(tData[,9])

taiwan <- function(x) {
  a <- tData[,8:9]
  comparison[comparison[,1]==data10002[1,1],2] <- mean(sign((data10002[,4] - data10002_01[,4]) * (a[x,1] - data10002[,3]) - (a[x,2] - data10002[,4]) * (data10002[,3] - data10002_01[,3]))[1:(nrow(data10002)-1)])
  comparison[comparison[,1]==data10015[1,1],2] <- mean(sign((data10015[,4] - data10015_01[,4]) * (a[x,1] - data10015[,3]) - (a[x,2] - data10015[,4]) * (data10015[,3] - data10015_01[,3]))[1:(nrow(data10015)-1)])
  comparison[comparison[,1]==data09020[1,1],2] <- mean(sign((data09020[,4] - data09020_01[,4]) * (a[x,1] - data09020[,3]) - (a[x,2] - data09020[,4]) * (data09020[,3] - data09020_01[,3]))[1:(nrow(data09020)-1)])
  comparison[comparison[,1]==data10008[1,1],2] <- mean(sign((data10008[,4] - data10008_01[,4]) * (a[x,1] - data10008[,3]) - (a[x,2] - data10008[,4]) * (data10008[,3] - data10008_01[,3]))[1:(nrow(data10008)-1)])
  comparison[comparison[,1]==data10013[1,1],2] <- mean(sign((data10013[,4] - data10013_01[,4]) * (a[x,1] - data10013[,3]) - (a[x,2] - data10013[,4]) * (data10013[,3] - data10013_01[,3]))[1:(nrow(data10013)-1)])
  comparison[comparison[,1]==data10005[1,1],2] <- mean(sign((data10005[,4] - data10005_01[,4]) * (a[x,1] - data10005[,3]) - (a[x,2] - data10005[,4]) * (data10005[,3] - data10005_01[,3]))[1:(nrow(data10005)-1)])
  comparison[comparison[,1]==data68000[1,1],2] <- mean(sign((data68000[,4] - data68000_01[,4]) * (a[x,1] - data68000[,3]) - (a[x,2] - data68000[,4]) * (data68000[,3] - data68000_01[,3]))[1:(nrow(data68000)-1)])
  comparison[comparison[,1]==data64000[1,1],2] <- mean(sign((data64000[,4] - data64000_01[,4]) * (a[x,1] - data64000[,3]) - (a[x,2] - data64000[,4]) * (data64000[,3] - data64000_01[,3]))[1:(nrow(data64000)-1)])
  comparison[comparison[,1]==data10017[1,1],2] <- mean(sign((data10017[,4] - data10017_01[,4]) * (a[x,1] - data10017[,3]) - (a[x,2] - data10017[,4]) * (data10017[,3] - data10017_01[,3]))[1:(nrow(data10017)-1)])
  comparison[comparison[,1]==data09007[1,1],2] <- mean(sign((data09007[,4] - data09007_01[,4]) * (a[x,1] - data09007[,3]) - (a[x,2] - data09007[,4]) * (data09007[,3] - data09007_01[,3]))[1:(nrow(data09007)-1)])
  comparison[comparison[,1]==data10009[1,1],2] <- mean(sign((data10009[,4] - data10009_01[,4]) * (a[x,1] - data10009[,3]) - (a[x,2] - data10009[,4]) * (data10009[,3] - data10009_01[,3]))[1:(nrow(data10009)-1)])
  comparison[comparison[,1]==data10018[1,1],2] <- mean(sign((data10018[,4] - data10018_01[,4]) * (a[x,1] - data10018[,3]) - (a[x,2] - data10018[,4]) * (data10018[,3] - data10018_01[,3]))[1:(nrow(data10018)-1)])
  comparison[comparison[,1]==data10004[1,1],2] <- mean(sign((data10004[,4] - data10004_01[,4]) * (a[x,1] - data10004[,3]) - (a[x,2] - data10004[,4]) * (data10004[,3] - data10004_01[,3]))[1:(nrow(data10004)-1)])
  comparison[comparison[,1]==data10020[1,1],2] <- mean(sign((data10020[,4] - data10020_01[,4]) * (a[x,1] - data10020[,3]) - (a[x,2] - data10020[,4]) * (data10020[,3] - data10020_01[,3]))[1:(nrow(data10020)-1)])
  comparison[comparison[,1]==data10010[1,1],2] <- mean(sign((data10010[,4] - data10010_01[,4]) * (a[x,1] - data10010[,3]) - (a[x,2] - data10010[,4]) * (data10010[,3] - data10010_01[,3]))[1:(nrow(data10010)-1)])
  comparison[comparison[,1]==data10007[1,1],2] <- mean(sign((data10007[,4] - data10007_01[,4]) * (a[x,1] - data10007[,3]) - (a[x,2] - data10007[,4]) * (data10007[,3] - data10007_01[,3]))[1:(nrow(data10007)-1)])
  comparison[comparison[,1]==data66000[1,1],2] <- mean(sign((data66000[,4] - data66000_01[,4]) * (a[x,1] - data66000[,3]) - (a[x,2] - data66000[,4]) * (data66000[,3] - data66000_01[,3]))[1:(nrow(data66000)-1)])
  comparison[comparison[,1]==data63000[1,1],2] <- mean(sign((data63000[,4] - data63000_01[,4]) * (a[x,1] - data63000[,3]) - (a[x,2] - data63000[,4]) * (data63000[,3] - data63000_01[,3]))[1:(nrow(data63000)-1)])
  comparison[comparison[,1]==data10014[1,1],2] <- mean(sign((data10014[,4] - data10014_01[,4]) * (a[x,1] - data10014[,3]) - (a[x,2] - data10014[,4]) * (data10014[,3] - data10014_01[,3]))[1:(nrow(data10014)-1)])
  comparison[comparison[,1]==data67000[1,1],2] <- mean(sign((data67000[,4] - data67000_01[,4]) * (a[x,1] - data67000[,3]) - (a[x,2] - data67000[,4]) * (data67000[,3] - data67000_01[,3]))[1:(nrow(data67000)-1)])
  comparison[comparison[,1]==data10016[1,1],2] <- mean(sign((data10016[,4] - data10016_01[,4]) * (a[x,1] - data10016[,3]) - (a[x,2] - data10016[,4]) * (data10016[,3] - data10016_01[,3]))[1:(nrow(data10016)-1)])
  comparison[comparison[,1]==data65000[1,1],2] <- mean(sign((data65000[,4] - data65000_01[,4]) * (a[x,1] - data65000[,3]) - (a[x,2] - data65000[,4]) * (data65000[,3] - data65000_01[,3]))[1:(nrow(data65000)-1)])
  return(comparison[which(abs(comparison[,2])==max(abs(comparison[,2]))),1])
}
system.time({ d <- sapply(1:nrow(tData), taiwan) })
tData <- cbind(tData, 途經縣市 = d, stringsAsFactors = FALSE)
rm(d)
