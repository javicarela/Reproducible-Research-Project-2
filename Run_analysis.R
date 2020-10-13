library(lubridate)
#########
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","storms.csv.bz2")
#
Storm_data<-read.csv('repdata_data_StormData.csv.bz2', stringsAsFactors = FALSE)
#
str(Storm_data)
#
Storm_data$BGN_DATE <- as.Date(Storm_data$BGN_DATE,format="%m/%d/%Y")
#
Storm_data_sub <- Storm_data[year(Storm_data$BGN_DATE)>=1980,]
vars <- c( "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm <- Storm_data_sub[, vars]
dim(storm)
#
sort(table(storm$PROPDMGEXP), decreasing = TRUE)[1:10]
sort(table(storm$CROPDMGEXP), decreasing = TRUE)[1:10]
#
library(dplyr)
summarise_all(storm, funs(sum(is.na(.))))
# 
storm$EVTYPE[grep("BLIZZARD|WINTER",storm$EVTYPE)] <- "BLIZZARD"
storm$EVTYPE[grep("FLOOD|FLDG|FLD|URBAN|FLOOO",storm$EVTYPE)]<-"FLOOD"
storm$EVTYPE[grep("FREEZING RAIN|RAIN",storm$EVTYPE)] <- "RAIN"
storm$EVTYPE[grep("ICE|ICY",storm$EVTYPE)]<- "ICE"
storm$EVTYPE[grep("HURRICANE|TYPHOON",storm$EVTYPE)] <- "HURRICANE"
storm$EVTYPE[grep("THUNDER|TSTM",storm$EVTYPE)] <- "THUNDER STORM"
storm$EVTYPE[grep("LIGHTN|LIGHTI|LIGNT",storm$EVTYPE)] <- "LIGHTNING"
storm$EVTYPE[grep("WIND|WND",storm$EVTYPE)] <- "WIND"
storm$EVTYPE[grep("TORN|FUNNEL",storm$EVTYPE)] <- "TORNADO"
storm$EVTYPE[grep("HEAT|HOT|HIGH TEMP",storm$EVTYPE)] <- "HEATWAVE"
storm$EVTYPE[grep("SWELL|SEA|TIDE",storm$EVTYPE)] <- "SEAS"
storm$EVTYPE<-as.factor(storm$EVTYPE)
# 
storm$PROPDMGEXP <- as.character(storm$PROPDMGEXP)
storm$PROPDMGEXP[is.na(storm$PROPDMGEXP)] <- 0 # NA's considered as dollars
storm$PROPDMGEXP[!grepl("K|M|B", storm$PROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B
storm$PROPDMGEXP[grep("K", storm$PROPDMGEXP, ignore.case = TRUE)] <- "3"
storm$PROPDMGEXP[grep("M", storm$PROPDMGEXP, ignore.case = TRUE)] <- "6"
storm$PROPDMGEXP[grep("B", storm$PROPDMGEXP, ignore.case = TRUE)] <- "9"
storm$PROPDMGEXP <- as.numeric(as.character(storm$PROPDMGEXP))
storm$property.damage <- storm$PROPDMG * 10^storm$PROPDMGEXP

storm$CROPDMGEXP <- as.character(storm$CROPDMGEXP)
storm$CROPDMGEXP[is.na(storm$CROPDMGEXP)] <- 0 # NA's considered as dollars
storm$CROPDMGEXP[!grepl("K|M|B", storm$CROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
storm$CROPDMGEXP[grep("K", storm$CROPDMGEXP, ignore.case = TRUE)] <- "3"
storm$CROPDMGEXP[grep("M", storm$CROPDMGEXP, ignore.case = TRUE)] <- "6"
storm$CROPDMGEXP[grep("B", storm$CROPDMGEXP, ignore.case = TRUE)] <- "9"
storm$CROPDMGEXP <- as.numeric(as.character(storm$CROPDMGEXP))
storm$crop.damage <- storm$CROPDMG * 10^storm$CROPDMGEXP
#
sort(table(storm$property.damage), decreasing = TRUE)[1:5]
sort(table(storm$crop.damage), decreasing = TRUE)[1:5]
#
library(dplyr)
library(plyr)
# aggregate FATALITIES and INJURIES by type of EVTYPE
agg.fatalites.and.injuries <- ddply(storm, .(EVTYPE), summarize, Total = sum(FATALITIES + INJURIES,  na.rm = TRUE))
agg.fatalites.and.injuries$type <- "fatalities and injuries"

# aggregate FATALITIES by type of EVTYPE
agg.fatalities <- ddply(storm, .(EVTYPE), summarize, Total = sum(FATALITIES, na.rm = TRUE))
agg.fatalities$type <- "fatalities"

# aggregate INJURIES by type of EVTYPE
agg.injuries <- ddply(storm, .(EVTYPE), summarize, Total = sum(INJURIES, na.rm = TRUE))
agg.injuries$type <- "injuries"

# combine all
agg.health <- rbind(agg.fatalities, agg.injuries)
agg.health_top<-head(agg.health[order(agg.health$Total, decreasing = TRUE),],n =20L)

health.by.EVTYPE <- join (agg.fatalities, agg.injuries, by="EVTYPE", type="inner")
health_top<-head(health.by.EVTYPE[order(health.by.EVTYPE$Total, decreasing = TRUE),], n =20L)
health_top
#
# aggregate PropDamage and CropDamage by type of EVTYPE
agg.propdmg.and.cropdmg <- ddply(storm, .(EVTYPE), summarize, Total = sum(property.damage + crop.damage,  na.rm = TRUE))
agg.propdmg.and.cropdmg$type <- "property and crop damage"

# aggregate PropDamage by type of EVTYPE
agg.prop <- ddply(storm, .(EVTYPE), summarize, Total = sum(property.damage, na.rm = TRUE))
agg.prop$type <- "property"

# aggregate INJURIES by type of EVTYPE
agg.crop <- ddply(storm, .(EVTYPE), summarize, Total = sum(crop.damage, na.rm = TRUE))
agg.crop$type <- "crop"

# combine all
agg.economic <- rbind(agg.prop, agg.crop)
agg.economic_top<-head(agg.economic[order(agg.economic$Total, decreasing = TRUE),],n =20L)


economic.by.EVTYPE <- join (agg.prop, agg.crop, by="EVTYPE", type="inner")
economic_top<-head(economic.by.EVTYPE[order(economic.by.EVTYPE$Total, decreasing = TRUE),],n =20L)
economic_top
library(ggplot2)
library(ggthemes)
health.plot <- ggplot(agg.health_top, aes(x = EVTYPE, y = Total, fill = type)) + geom_bar(stat = "identity") + coord_flip() + xlab("Event Type") + ylab("Number of health impact") +
  ggtitle("Weather event types impact") + scale_fill_manual(values=c("red","black")) + theme_economist()

print(health.plot)  
#
economic.plot <- ggplot(agg.economic_top, aes(x = EVTYPE, y = Total, fill = type)) + geom_bar(stat = "identity") + coord_flip() + xlab("Event Type") + ylab("Number of economic impact") +
  ggtitle("Weather event types impact") + scale_fill_manual(values=c("red","black")) + theme_economist()

print(economic.plot) 
