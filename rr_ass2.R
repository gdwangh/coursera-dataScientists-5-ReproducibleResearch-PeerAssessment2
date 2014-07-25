#setwd('D:/doc/study/dataScientists/5-Reproducible Research/RepData_PeerAssessment2')
setwd("D:/workspace/dataScientists/5-Reproducible Research")
Sys.setlocale('LC_ALL', 'English')

# download and unzip the data file
if (!file.exists("StormData.csv.bz2")) {
  fileURL<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL,destfile="StormData.csv.bz2")
  list.files("./StormData.csv.bz2")
  dateDownload<-date()
  dateDownload
}

library(data.table)
rawdata<-as.data.table(read.csv("StormData.csv.bz2",header=TRUE, nrows=5))
to_read = c("NULL", NA,rep("NULL",4),NA,NA,rep("NULL",14),rep(NA,6),rep("NULL",8),NA)
sdata<-as.data.table(read.csv("StormData.csv.bz2",colClasses=to_read,stringsAsFactors=FALSE))
sdata[, year:=year(as.Date(strptime(BGN_DATE, "%m/%d/%Y %H:%M:%S")))]
sdata$STATE = as.factor(sdata$STATE)
summary(sdata)

# read valid event type and sort them by length
#event_type<-as.data.table(read.table("event_Type.txt", header=FALSE,stringsAsFactors=FALSE,sep="/n",col.names=c("typename")))
event_type<-data.table(typename=c("Avalanche",
  "Blizzard",
  "Coastal Flood",
  "Cold/Wind Chill",
  "Debris Flow",
  "Dense Fog",
  "Dense Smoke",
  "Drought",
  "Dust Devil",
  "Dust Storm",
  "Excessive Heat",
  "Extreme Cold/Wind Chill",
  "Flash Flood",
  "Flood",
  "Frost/Freeze",
  "Funnel Cloud",
  "Freezing Fog",
  "Hail",
  "Heat",
  "Heavy Rain",
  "Heavy Snow",
  "High Surf",
  "High Wind",
  "Hurricane",
  "Ice Storm",
  "Lake-Effect Snow",
  "Lakeshore Flood",
  "Lightning",
  "Marine Hail",
  "Marine High Wind",
  "Marine Strong Wind",
  "Marine Thunderstorm Wind",
  "Rip Current",
  "Seiche",
  "Sleet",
  "Storm Surge/Tide",
  "Strong Wind",
  "Thunderstorm Wind",
  "Tornado",
  "Tropical Depression",
  "Tropical Storm",
  "Tsunami",
  "Volcanic Ash",
  "Waterspout",
  "Wildfire",
  "Winter Storm",
  "Winter Weather"))
event_type[,len:=nchar(typename)]
sort_evtype<-event_type[order(event_type$len, decreasing=TRUE)]

# clean data,  
# get the record from year 2005 and get rid of record with human +economic costs are zero
cost_data<-subset(sdata,FATALITIES+INJURIES+PROPDMG+CROPDMG>0)

# replace substring in evtype string:  src-->desc 
# TSTM --> Thunderstorm
# Landslide --> Debris Flow.
# STORM SURGE --> STORM SURGE/Tide
# ASTRONOMICAL HIGH TIDE --> High Surf
# Extreme Cold --> Frost/Freeze
# WILD/FOREST FIRE --> Wildfire
# URBAN/SML STREAM FLD --> Heavy Rain
# LIGHT SNOW --> Winter Storm
# DRY MICROBURST --> Thunderstorm Wind
#  FREEZING RAIN --> Winter Weather
trans_src_desc<-matrix(c("TSTM", "Thunderstorm",
                      "Landslide", "Debris Flow",
                      "STORM SURGE", "STORM SURGE/Tide",
                      "ASTRONOMICAL HIGH TIDE", "High Surf",
                      "Extreme Cold", "Frost/Freeze",
                      "WILD/FOREST FIRE", "Wildfire",
                      "URBAN/SML STREAM FLD", "Heavy Rain",
                      "LIGHT SNOW", "Winter Storm",
                      "DRY MICROBURST", "Thunderstorm Wind",
                      "FREEZING RAIN", "Winter Weather",
                      "GUSTY WINDS","Strong Wind",
                      "SNOW SQUALL","Winter Weather",
                      "EXTREME WINDCHILL","Extreme Cold/Wind Chill",
                      "GUSTY WIND", "Strong Wind"), 
                      ncol = 2, byrow=TRUE
                    )
  
for (i in 1:nrow(trans_src_desc))
  cost_data[,EVTYPE:=gsub(trans_src_desc[i,1],trans_src_desc[i,2],EVTYPE,ignore.case=TRUE)]


# get the valid event type by greping the event_type name from long to short
cost_data[,valid_type:=""]
for (et in sort_evtype$typename) {
   cost_data[(valid_type=="") & grepl(et, cost_data$EVTYPE, ignore.case=TRUE), valid_type:=et]
}

# recheck the left event type
ldata<-cost_data[valid_type=="", .N, by=EVTYPE]
ldata[order(ldata$N, decreasing=TRUE)]
ldata[ldata$N>10,]

# check the event type appear in each year
us<-unique(subset(cost_data,valid_type!="",select=c("year","valid_type")))
us[,.N,by=year]

# so get the record after 1992
cldata<-subset(cost_data, valid_type!="" & year>"1992")
cldata$valid_type<-as.factor(cldata$valid_type)

# Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?
cldata_popu<-subset(cldata, FATALITIES+INJURIES>0)
cldata_popu[,harm_popu:=FATALITIES+INJURIES]
popu_sum<-cldata_popu[,sum(harm_popu),by=valid_type]
setnames(popu_sum, "V1", "sum")
sorted_popu_sum<-popu_sum[order(popu_sum$sum, decreasing=TRUE)]
popu_sum[which(sum==max(popu_sum$sum))]
topN_harm_popu<-sorted_popu_sum[1:3,]$valid_type

#2. Across the United States, which types of events have the greatest economic consequences?
# get rid of the invalid unit and trans the cost unit same
cldata_cost<-subset(cldata, PROPDMG+CROPDMG>0)

cldata_cost[,sum(PROPDMG),by=PROPDMGEXP]
cldata_cost[,.N,by=PROPDMGEXP]
subset(cldata_cost, PROPDMGEXP %in% c("K","k","M","m","B","b",""))

cldata2<-subset(cldata_cost, PROPDMGEXP %in% c("K","k","M","m","B","b","")& CROPDMGEXP  %in% c("K","k","M","m","B","b",""))
cldata2[PROPDMGEXP %in% c(""),pro_cost:=PROPDMG]
cldata2[PROPDMGEXP %in% c("K","k"),pro_cost:=PROPDMG*10^3]
cldata2[PROPDMGEXP %in% c("M","m"),pro_cost:=PROPDMG*10^6]
cldata2[PROPDMGEXP %in% c("B","b"),pro_cost:=PROPDMG*10^9]

cldata2[CROPDMGEXP %in% c(""),crop_cost:=CROPDMG]
cldata2[CROPDMGEXP %in% c("K","k"),crop_cost:=CROPDMG*10^3]
cldata2[CROPDMGEXP %in% c("M","m"),crop_cost:=CROPDMG*10^6]
cldata2[CROPDMGEXP %in% c("B","b"),crop_cost:=CROPDMG*10^9]

cldata2[, cost_sum:=pro_cost+crop_cost]
cost_sum<-cldata2[,sum(cost_sum),by=valid_type]
setnames(cost_sum, "V1", "sum")
sorted_cost_sum<-cost_sum[order(cost_sum$sum, decreasing=TRUE)]
cost_sum[which(sum==max(cost_sum$sum))]
topN_cost<-sorted_cost_sum[1:3,]$valid_type
  
par(mfrow = c(1, 2))
pie(sorted_popu_sum$sum, labels=sorted_popu_sum$valid_type,col=rainbow(length(sorted_popu_sum$valid_type)), main="FATALITIES & INJURIES")
pie(sorted_cost_sum$sum, labels=sorted_cost_sum$valid_type,col=rainbow(length(sorted_cost_sum$valid_type)), main="PROPDMG & CROPDMG")

# harm with population for top 3 
require(reshape2)
pmelt<-melt(subset(cldata_popu, valid_type %in% topN_harm_popu), id.vars=c("valid_type", "STATE"),measure.vars=c("harm_popu"))
popu_state_sum<-dcast(pmelt,STATE+valid_type~variable, fun=sum)

cmelt<-melt(subset(cldata2, valid_type %in% topN_cost), id.vars=c("valid_type", "STATE"),measure.vars=c("cost_sum"))
cost_state_sum<-dcast(cmelt,STATE+valid_type~variable, fun=sum)


# draw plots
library(lattice)
library(latticeExtra)
library(maps)
library(mapproj)
library(choroplethr)
data(state.names)
smap<-map("state",plot = FALSE, fill = TRUE)

df1<-merge(popu_state_sum, subset(state.names,!abb %in% c("HI","AK")),by.x="STATE",by.y="abb")
mapplot(name~harm_popu|valid_type, data = df1, xlab="",layout = c(1,3),map = map("state",plot = FALSE, fill = TRUE), colramp = colorRampPalette(c("green", "blue")))

df2<-merge(cost_state_sum, subset(state.names,!abb %in% c("HI","AK")),by.x="STATE",by.y="abb")
mapplot(name~cost_sum|valid_type, data = df2, xlab="",layout = c(1, 3),map = map("state",plot = FALSE, fill = TRUE), colramp = colorRampPalette(c("green", "blue")))

library(ggplot2)
pmelt$level=as.factor(cut(pmelt$value, breaks=c(0,50, 100,500,1000,1500), labels=FALSE))
ggplot(pmelt, aes(valid_type,STATE)) + geom_tile(aes(fill = level),
        colour = "white")  +   scale_fill_manual(values=rainbow (5))

