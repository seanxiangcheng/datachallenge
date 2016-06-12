setwd("/home/xcheng0907/GoogleDrive/Career/Applications/IntuitiveSurgical/datachallenge")
library(lubridate)

data_dir = "./data/"
f_activity = paste0(data_dir, "Activities.csv")
f_event1 = paste0(data_dir, "Events_1.csv")
f_event2 = paste0(data_dir, "Events_2.csv")

df.Act = read.csv(f_activity)
df.E1 = read.csv(f_event1)
df.E2 = read.csv(f_event2)

df.E1 = transform(df.E1, datetime.utc = ymd_hms(Timestamp))
df.E1 = transform(df.E1, weekday.utc = weekdays(datetime.utc), hour.uct=hour(datetime.utc))
df.E1 = subset(df.E1, select=-Timestamp)
df.E1 = transform(df.E1, datetime.edt = with_tz(datetime.utc, tzone="America/New_York"))
df.E1 = transform(df.E1, hour.edt = hour(datetime.edt), weekday.edt=weekdays(datetime.edt))


df.E2 = transform(df.E2, datetime.utc = ymd_hms(Timestamp))
df.E2 = transform(df.E2, weekday.utc = weekdays(datetime.utc), hour.uct=hour(datetime.utc))
df.E2 = subset(df.E2, select=-Timestamp)
df.E2 = transform(df.E2, datetime.edt = with_tz(datetime.utc, tzone="America/New_York"))
df.E2 = transform(df.E2, hour.edt = hour(datetime.edt), weekday.edt=weekdays(datetime.edt))

df.Act = transform(df.Act, Timestamp=paste(Date, Start))
df.Act = transform(df.Act, datetime.edt=mdy_hms(Timestamp, tz="America/New_York"))
df.Act = transform(df.Act, weekday.edt=weekdays(datetime.edt), hour.edt=hour(datetime.edt), gmt_ts=as.numeric(datetime.edt))
df.Act = subset(df.Act, select=-c(Date, Start, Time.Zone, Timestamp))

# generate relative gmt_ts:
min_gmt_ts = min(min(df.Act$gmt_ts), min(df.Act$gmt_ts), min(df.Act$gmt_ts))
df.Act = transform(df.Act, relative_gmt_ts=gmt_ts-min_gmt_ts)
df.E1 = transform(df.E1, relative_gmt_ts=gmt_ts-min_gmt_ts)
df.E2 = transform(df.E2, relative_gmt_ts=gmt_ts-min_gmt_ts)

boxplot( relative_gmt_ts ~ ActivityCodes, df.Act)
boxplot( relative_gmt_ts ~ EventCodes, df.E1)
boxplot( relative_gmt_ts ~ EventCodes, df.E2)


meansd.act = aggregate(relative_gmt_ts~ActivityCodes, df.Act, FUN=function(x)c(ave=mean(x), std=sd(x), len=length(x)))
meansd.act= meansd.act[order(meansd.act$relative_gmt_ts[, 1]), ]


meansd.e1 = aggregate(relative_gmt_ts~EventCodes, df.E1, FUN=function(x)c(ave=mean(x), std=sd(x), len=length(x)))
meansd.e1 = meansd.e1[order(meansd.e1$relative_gmt_ts[, 1]), ]

meansd.e2 = aggregate(relative_gmt_ts~EventCodes, df.E2, FUN=function(x)c(ave=mean(x), std=sd(x), len=length(x)))
meansd.e2 = meansd.e2[order(meansd.e2$relative_gmt_ts[, 1]), ]



df.E = rbind(df.E1, df.E2)[ ,c('relative_gmt_ts', 'EventCodes')]
df.A = df.Act[, c('relative_gmt_ts','ActivityCodes')]

i=1
ind=which.min(abs(df.A[i,1] - df.E[,1]))
as.character(df.E[i,2])

find_closest_event <- function(rel_ts){
  ind=which.min(abs(rel_ts - df.E[,1]))
  as.character(df.E[ind,2])
}

Act_events = rep('', dim(df.A)[1])
for(i in 1:dim(df.A)[1]){
  Act_events[i] = find_closest_event(df.A[i, 1])
}
df.A$Act_events = Act_events
df.A = df.A[order(df.A$ActivityCodes), ]

library(class)

df.A$Prediction_1NN = knn(as.matrix(df.E$relative_gmt_ts), as.matrix(df.A$relative_gmt_ts), df.E$EventCodes, k=1)
df.A$Prediction_5NN = knn(as.matrix(df.E$relative_gmt_ts), as.matrix(df.A$relative_gmt_ts), df.E$EventCodes, k=5)
df.A$Prediction_10NN = knn(as.matrix(df.E$relative_gmt_ts), as.matrix(df.A$relative_gmt_ts), df.E$EventCodes, k=10)
df.A$Prediction_15NN = knn(as.matrix(df.E$relative_gmt_ts), as.matrix(df.A$relative_gmt_ts), df.E$EventCodes, k=15)
df.A$Prediction_20NN = knn(as.matrix(df.E$relative_gmt_ts), as.matrix(df.A$relative_gmt_ts), df.E$EventCodes, k=20)
df.A = df.A[order(df.A$ActivityCodes),-1]


# final prediction
activities = sort(unique(df.A$ActivityCodes))
prediction = rep('', length(activities))
for(i in 1:length(activities)){
  pred = df.A$Prediction_5NN[ df.A$ActivityCodes==activities[i] ]
  pred = as.character(names(which.max(table(pred))))
  prediction[i] = pred
}
df.Final_Prediction = data.frame(Activites=activities, Match_Prediction=prediction)
