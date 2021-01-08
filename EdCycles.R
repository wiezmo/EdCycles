# script to run just the code for the choose your own project

### install and load required files

if(!require(rvest)) install.packages("rvest", 
                                     repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", 
                                       repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                            repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", 
                                            repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", 
                                     repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", 
                                   repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", 
                                    repos = "http://cran.us.r-project.org")

gbm


library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)

# define the target web address

targetpage<-'https://edinburghcyclehire.com/open-data/historical'

# read in the targetnodes on the targetpage

targetnodes<-read_html(targetpage) %>%
  html_nodes(".hHEtRQ") %>% 
  html_attr(name='content')

# now we can read each of the links. this may take a while
cycleframe<-do.call(rbind,lapply(targetnodes,read.csv))

save(cycleframe,file='cycleframe.Rda')


# we also scrape elevation data

library(httr)
library(jsonlite)


# define a function to read elevation data from open-elevation
scrape_elev <- function (indat)
{
  u <- "https://api.open-elevation.com/api/v1/lookup"
  h <- c ("Accept" = "application/json",
          "Content-Type" = "application/json")
  dat <- indat %>%
    jsonlite::toJSON ()
  b <- paste0 ('{"locations":', dat, '}')
  res <- httr::POST (u, httr::add_headers (.headers = h), body = b)
  httr::content (res, as = 'text', encoding = "UTF-8",
                 type = "application/xml") %>%
    jsonlite::fromJSON()
  
}


# here the data is loaded from the local respository to avoid a repeat of the download. 
# This also allows analysis when temporary without internet access.
load('cycleframe.Rda') 
eledata<-cycleframe %>%distinct(start_station_id,.keep_all = TRUE)%>%
  select(start_station_latitude,start_station_longitude)
colnames(eledata)<-c('latitude','longitude')
head(eledata)

## The following code is commented out, as it can take a while to estbalish the connection.
# # now we implement the scrape function for the elevations, that we used earlier
# out<-scrape_elev(eledata)
# head(out$results)
# 
# # The elevation data for the start stations can now be stored in a seperate dataframe.
# startele<-data.frame(start_station_id=cycleframe %>%distinct(start_station_id),
#                      start_station_elevation=out$results$elevation)
# 
# 
# # now add the elevations to the original dataframe
# new<-left_join(cycleframe,startele,by="start_station_id")
# 
# 
# # now the process can be repeated for the end stations
# # create a dataframe that can be used with the required format
# eledata<-cycleframe %>%distinct(end_station_id,.keep_all = TRUE)%>%
#   select(start_station_latitude,start_station_longitude)
# colnames(eledata)<-c('latitude','longitude')
# 
# # get the data for each end station
# out<-scrape_elev(eledata)
# 
# endele<-data.frame(end_station_id=cycleframe %>%distinct(end_station_id),
#                    end_station_elevation=out$results$elevation)
# 
# # now add the end elevations to the original dataframe
# new<-left_join(new,endele,by="end_station_id")
# 
# 
# # now store it all as a dataframe locally
# 
# cycleframe_plus<-new
# 
# save(cycleframe_plus,file='cycleframe_plus.Rda')

# let's remove trips that are longer than 12 hours and shorter than 2 minutes

load('cycleframe_plus.Rda')
cycleframe_plus<-cycleframe_plus%>%filter(duration<12*60*60,duration>60*2)

workdays<-c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# let's modify/add some time/date data

new<-cycleframe_plus  %>%
  mutate(day=weekdays(as.Date(started_at)),
         date=as.Date(started_at),
         time=format(as.POSIXct(started_at), format = "%H:%M:%S"),
         daytype=as.factor(c('weekend', 'weekday')
                           [(weekdays(as.Date(started_at)) %in% 
                               workdays)+1L]),
         hour=hour(hms(as.character(time))),
         month= as.factor(month.abb[month(date)]),
         year=year(date),
         day=weekdays(as.Date(started_at)))


# remove stations with less than 100 trips
new<-new%>%group_by(start_station_name) %>% filter(n()>100) %>% ungroup()


# code to generate a dataframe with net_starts

starts<-new%>%group_by(as.factor(start_station_id),month,daytype,hour,year,day)%>%
  summarise(starts=(n()),
            elevation=mean(start_station_elevation)) %>%
  `colnames<-`(c('station_id','month','daytype','hour','year','day','starts','elevation'))

ends<-new%>%group_by(as.factor(end_station_id),month,daytype,hour,year,day) %>% 
  summarise(ends=n()) %>%
  `colnames<-`(c('station_id','month','daytype','hour','year','day','ends')) %>% ungroup()


net<-inner_join(starts,ends,by=c('station_id','hour','month','daytype','year','day')) %>%
  mutate(net_starts=starts-ends)


# now do the data analysi for the duration

set.seed(1984)

new<-cycleframe_plus%>% mutate(day=weekdays(as.Date(started_at)),
                               date=as.Date(started_at),
                               time=format(as.POSIXct(started_at), format = "%H:%M:%S"),
                               daytype=as.factor(c('weekend', 'weekday')
                                                 [(weekdays(as.Date(started_at)) %in% 
                                                     workdays)+1L]),
                               hour=hour(hms(as.character(time))),
                               month= month(date),
                               year=year(date)) %>%
  select(-c('start_station_name','end_station_name','start_station_description',
            'end_station_description','end_station_id','end_station_elevation',
            'end_station_longitude','end_station_latitude',
            'started_at','ended_at','time','date')) # removed cols



# now select a smaller subset from the trimmed down dataset.

new_s<-new[sample(nrow(new),size=25000),]


ti <- createDataPartition(y=new_s$duration, times = 1, p = 0.5, list = FALSE)

test_set <- new_s[ti, ]
train_set <- new_s[-ti, ]

# we define a function to get the Root measn squared error in minutes
RMSE <- function(true_values, pred_values){
  sqrt(mean((true_values - pred_values)^2,na.rm = T))/60
}

# simple RMSE based just on mean trip duration
y_h_mean<-mean(train_set$duration)

rmse_mean<-RMSE(test_set$duration,y_h_mean)
rmse_mean


# try linear regression

linfit<-lm(duration~.,data=train_set)

y_h_lin<-predict(linfit,test_set)


rmse_lin<-RMSE(test_set$duration,y_h_lin)
rmse_lin

# code to assess random forest classifier when the trip duration is split into classes
new_c<-new_s
# cut the duration into three classes.
new_c$duration<-cut(new_c$duration,3,
                    labels=c('short','medium','long'))

test_set <- new_c[ti, ]
train_set <- new_c[-ti, ]

train_rf <- randomForest(duration ~., data = train_set) 
y_h_rf<-predict(train_rf,test_set)

confusionMatrix(y_h_rf,test_set$duration)

# let's try a different cut version

library(Hmisc)

new_c<-new_s
# cut the duration into three classes.
new_c$duration<-cut2(new_c$duration,g=3)
# note that cut2() does not support levels or labels

test_set <- new_c[ti, ]
train_set <- new_c[-ti, ]

train_rf <- randomForest(duration ~., data = train_set) 
y_h_rf<-predict(train_rf,test_set)

confusionMatrix(y_h_rf,test_set$duration)



# now try to filter out values above the 95% percentile
new <- new %>% filter(duration < quantile(.$duration, 0.95))


new_s<-new[sample(nrow(new),size=25000),]

ti <- createDataPartition(y=new_s$duration, times = 1, p = 0.5, list = FALSE)

test_set <- new_s[ti, ]
train_set <- new_s[-ti, ]

linfit<-lm(duration~.,data=train_set)

y_h_lin<-predict(linfit,test_set)


rmse_lin<-RMSE(test_set$duration,y_h_lin)
rmse_lin


####### okay, let's move to prediction of the net starts

# take out starts and ends,
# if we include them as features we will get perfect predictions
net<-net%>%select(-c('starts','ends'))


# we also redefine our RMSE function, as we are not predicting
# minutes anymore
RMSE <- function(true_values, pred_values){
  sqrt(mean((true_values - pred_values)^2,na.rm = T))
}


# again we work with a smaller sample set
net_s<-net[sample(nrow(net),size=40000),]


ti <- createDataPartition(y=net_s$net_starts, times = 1, p = 0.5, list = FALSE)


test_set <- net_s[ti, ]
train_set <- net_s[-ti, ]


# try simple mean prediciton
y_h_mean=mean(train_set$net_starts)

RMSE(test_set$net_starts,y_h_mean)


# step it up to linear regression

linfit<-lm(net_starts~.,data=train_set)

y_hat_lin<-predict(linfit,test_set)

RMSE(test_set$net_starts,y_hat_lin)

summary(linfit)%>%.$coefficients%>%head()


####### let's redefine the net starts, !!!!!!

new<-cycleframe_plus  %>%
  mutate(day=weekdays(as.Date(started_at)),
         date=as.Date(started_at),
         time=format(as.POSIXct(started_at), format = "%H:%M:%S"),
         daytype=as.factor(c('weekend', 'weekday')
                           [(weekdays(as.Date(started_at)) %in% 
                               workdays)+1L]),
         hour=hour(hms(as.character(time))),
         month= (month(date)),
         year=year(date))

starts<-new%>%group_by(as.factor(start_station_id),month,daytype,hour,year,day,start_station_latitude,start_station_longitude)%>%
  summarise(starts=(n()),
            elevation=mean(start_station_elevation)) %>%
  `colnames<-`(c('station_id','month','daytype','hour','year','day','starts','elevation','station_latitude','station_longitude'))

ends<-new%>%group_by(as.factor(end_station_id),month,daytype,hour,year,day) %>% 
  summarise(ends=n()) %>%
  `colnames<-`(c('station_id','month','daytype','hour','year','day','ends')) %>% ungroup()


net<-inner_join(starts,ends,by=c('station_id','hour','month','daytype','year','day')) %>%
  mutate(net_starts=starts-ends)


net<-subset(net,select=-c(station_id,starts,ends))


net_s<-net[sample(nrow(net),size=40000),]


ti <- createDataPartition(y=net_s$net_starts, times = 1, p = 0.5, list = FALSE)


test_set <- net_s[ti, ]
train_set <- net_s[-ti, ]

# try linear fit again, with the reorganised package
linfit<-lm(net_starts~.,data=train_set)
summary(linfit)

y_hat_lin<-predict(linfit,test_set)

RMSE(test_set$net_starts,y_hat_lin)

# now for some more advanced machine learning

library(gbm)

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated two times
  repeats = 2)

train_gbm <- train(net_starts ~ ., data = train_set, 
                   method = "gbm", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)


ggplot(train_gbm, highlight = TRUE)

y_hat_gbm<-predict(train_gbm,test_set)  

# get the final RMSE, achieved from gbm
RMSE(test_set$net_starts,y_hat_gbm)



