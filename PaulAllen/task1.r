library(plyr)
library(reshape2)
require(RgoogleMaps)
require(sp)
library(dismo)

source("/home/wyu/Projects/R/PaulAllen/prepareData.r")

tweets = prepareData("/home/wyu/Projects/R/PaulAllen/phailin_tweets.csv");

# twits   = read.csv("/home/wyu/Projects/R/PaulAllen/phailin_tweets.csv" ,stringsAsFactors=FALSE) 
# ## split the time stamp into separate columns
# date    = data.frame(do.call('rbind', strsplit(as.character(twits$Time),'/',fixed=TRUE))); 
# hr.min  = data.frame(do.call('rbind', strsplit(as.character(date$X3),' ',fixed=TRUE))); 
# min.sec = data.frame(do.call('rbind', strsplit(as.character(hr.min$X2),':',fixed=TRUE))); 
# time    = cbind(date[,c(1,2)], hr.min[,1], min.sec)
# colnames(time) = c('Month','Day','Hour','Min','Sec')
# twits = cbind(twits, time)
# rm(date, hr.min, min.sec, time)

# counts the rows by date
counts.day = aggregate(id~Day, data=tweets, length)
counts.min = aggregate(id~Day+Min, data=tweets, length)
plot(counts.day)
plot(counts.min)

# # reformat the hash tags
# co.Gopalpur = as.matrix(geocode("Gopalpur, India")[  ,c('longitude','latitude')])
# co.Mumbai   = as.matrix(geocode("Mumbai, India")[    ,c('longitude','latitude')])
# co.NewDelhi = as.matrix(geocode("New  Delhi, India")[,c('longitude','latitude')])
# 
# twits$HashTag=NA; twits$fromMumbai=NA; twits$fromGopalpur=NA; twits$NewDelhi=NA
# tws = list(); k=0;
# for (i in seq(1:dim(twits)[1])) 
# {
#   # populate the distances
#   if (is.na(twits[i,]$Tweet_GPS_long)==F&is.na(twits[i,]$Tweet_GPS_lat)==F)
#   {
#     co = matrix(c(twits[i,]$Tweet_GPS_long, twits[i,]$Tweet_GPS_lat), ncol=2);
#     twits[i,]$fromNewDelhi=spDistsN1(co, co.NewDelhi[1,], longlat=TRUE)/1.6
#     twits[i,]$fromMumbai  =spDistsN1(co, co.Mumbai[  1,], longlat=TRUE)/1.6
#     twits[i,]$fromGopalpur=spDistsN1(co, co.Gopalpur[1,], longlat=TRUE)/1.6
#   }
#   htags=strsplit(twits$Hashtags_In_Text[i], ',');
#   for (j in seq(1:length(htags[[1]])))
#   {
#     row = twits[i,];
#     k   = k+1;
#     row$HashTag = tolower(htags[[1]][j]);
#     tws[[k]] = row
#   }  
# }
# tweets = do.call(rbind, tws);
# tweets$HashTag = as.factor(tweets$HashTag)
# tweets$Month   = as.factor(tweets$Month)
# tweets$Day     = as.factor(tweets$Day)
# tweets$Hour    = as.factor(tweets$Hour)
# tweets$Min     = as.factor(tweets$Min)
# 
# rm(row, i,j,k, tws, htags)
     
# data inventory
ftable(tweets[c("HashTag", "Month")])

# the top 10 tags
t10 = c('#phailin','#cyclonephailin','#odisha','#india','#cyclone','#news','#andhrapradesh','#phailinfury','#bhubaneswar','#gopalpur','#nari')

tweets10 = droplevels(subset(tweets, HashTag %in% t10));
ftable(tweets10[c("HashTag", "Day")])
ftable(tweets10[c("HashTag")])

# subset without re-tweet
noretweets = droplevels(subset(tweets, Is_Retweet==0));
noGPS      = droplevels(subset(noretweets, is.na(Tweet_GPS_lat)|is.na(Tweet_GPS_long)));
GPS        = droplevels(subset(noretweets, is.na(Tweet_GPS_lat)==F&is.na(Tweet_GPS_long)==F));

ftable(droplevels(subset(noretweets, HashTag %in% t10))[c("HashTag", "Day")])
