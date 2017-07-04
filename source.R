library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
setwd("C:/Users/razba/Desktop/Final_project")

graph_pre_condition <- function(data,start_year) {
  data[,SUMPERYEAR := sum(VALUE), by=paste0(YEAR)]
  data = unique(PRCP.station[,.(YEAR,SUMPERYEAR)])
  #we divided the dataset into n groups
  n = 15
  p = data.table("PERIOD"=c(1:n), "MEAN"=1:n)
  period = length(data$YEAR)/n
  for (i in 1:n){
    p[i,1] = start_year+(period*i)
    p[i,2] = data[YEAR > (as.integer(p[i,1])-period) & YEAR <= as.integer(p[i,1]),mean(SUMPERYEAR)]
  }
  return(p);
}

###############################################################################################################
# These file is a especial parser for the excel file that contains the kinneret data
# Step1
# The excel file contains 173 pages(sheets) and needed to be parsed into one long page

total <- read_excel("1966-2017-MiflasKinneretExcelParsed.xlsx",na = "empty" ,sheet = "Page 1",col_names = FALSE)
total <- total[2:length(total),]

for (page in 2:173){
  addme <- read_excel("1966-2017-MiflasKinneretExcelParsed.xlsx",na = "empty" ,sheet = paste0("Page"," ",page),col_names = FALSE)
  total <- rbind(total,addme)
}
total = total[ ,1:3]
total = na.omit(total) #remove NA rows

names(total) = c("DailyDelta","SeaLevel","Date")
#write.csv(total, file = "/Users/eran/Downloads/ParsedKinneret1966-2017.csv",row.names = FALSE)
######################################################################################################
#Step 2

data.kinneret = as.data.frame(total,row.names(NULL),header=FALSE);
datetxt <- data.kinneret$Date;
datetxt <- as.Date(datetxt,"%d/%m/%Y")

#parse the date column into 3 new columns, day,month,year
df <- data.frame(date = datetxt,
                 day = as.numeric(format(datetxt, format = "%d")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 year = as.numeric(format(datetxt, format = "%Y")))

data.kinneret$Day = df$day;            
data.kinneret$Month = df$month;
data.kinneret$Year = df$year;
data.kinneret = as.data.table(data.kinneret,row.names(NULL))
data.kinneret[,"rn"]=NULL
data.kinneret[,SeaLevel:=as.numeric(SeaLevel)] #convert  column to numeric column
data.kinneret[,SeaMean:=mean(SeaLevel), by=paste0(Month,Year)] #calc mean of seaLevel
data.kinneret[,"Date"]=NULL
data.kinneret[,"SeaLevel"]=NULL
data.kinneret[,"DailyDelta"]=NULL
data.kinneret = unique(data.kinneret[,.(Month,Year,SeaMean)])

#plotting all months through all years
plot.year.months  = ggplot(data.kinneret,aes(x=Year,y=SeaMean,color=SeaMean))+ geom_point(shape=1) +geom_smooth(size=1,color="red",span=0.05)+  ggtitle("Kinneret Water Level") + ylab("Sea Level") + xlab("Year") + scale_color_gradient(name="Sea Level",low="blue",high="lightblue") + theme_bw()
plot.year.months  = plot.year.months + facet_wrap(~Month,ncol=3,shrink = TRUE) 
plot(plot.year.months)

#plotting february and august monthes
data.kinneret_2_8 =  data.kinneret[Month==2 | Month==8,] #make smaller table contains only 2 months
data.kinneret_2_8_plot = ggplot(data.kinneret_2_8,aes(x=Year,y=SeaMean,color=SeaMean))+ geom_point(shape=1) +geom_smooth(size=1,color="red",span=0.05)+  ggtitle("Kinneret Water Level in Fab and Aug") + ylab("Sea Level") + xlab("Year") + scale_color_gradient(name="Sea Level",low="blue",high="lightblue") + theme_bw()
data.kinneret_2_8_plot = data.kinneret_2_8_plot + facet_wrap(~Month,ncol=1,shrink = TRUE) 

plot(data.kinneret_2_8_plot+ scale_y_continuous(limit = c(-214,-208))) #for the y axle changing the default limits

##################################################################################
##################################################################################
# This part manufacture plots to help us identify reach a conclusion     
stations.file = fread("stations.csv") #reading the station file to get data on specific station later
table.prcp.world= fread("PRCP.csv")   #reading PRCP dataset for all the world
table.prcp.israel =  table.prcp.world[substr(STATION, 1, 6) == "IS0000",] #filter only stations is israel!
table.prcp.israel[,"NDAYS"]=NULL

##############################################
#split the big table into 5 stations is israel
stations.israel = unique(table.prcp.israel$STATION) #vector of stations ID in israel

#for each station we need to plot months & year one by one.
for(station in stations.israel){
  PRCP.station =  table.prcp.israel[STATION==station,]
  one.station.station.file = stations.file[STATION==station,]
  title.str = paste0(one.station.station.file$NAME," (",one.station.station.file$STATION,"), Lat=",one.station.station.file$LATITUDE,", Long=",one.station.station.file$LONGITUDE,", Elevation=",one.station.station.file$ELEVATION);
  sp <- ggplot(PRCP.station,aes(x=YEAR,y=VALUE,color=VALUE)) + geom_point(shape=1) +geom_smooth(size=1,color="blue",span=0.1)
  plot(sp + facet_wrap(~MONTH,ncol=3,shrink = TRUE) +  ggtitle(title.str) + ylab("PRCP") + xlab("Year") + scale_color_gradient(name="PRCP",low="red",high="pink") + theme_bw())
  
  p = graph_pre_condition(PRCP.station,1951)
  plot(p$PERIOD,p$MEAN, type = "l",col = "red",xlab = "Period", ylab = "Mean rainfall (mm)",main = one.station.station.file$NAME)
}

##################################################################################
##################################################################################
# Supporting data
#Step 1
#read the data from the file(the file is already at the structure we want)
IL_mean_rainfall = read_excel("pr5_1900_2012.xls",na = "empty")
IL_mean_rainfall = as.data.table(IL_mean_rainfall[,1:3])
names(IL_mean_rainfall) = c("Rainfall","Year","Month")

IL_mean_rainfall = IL_mean_rainfall[Year >1947,] #Israel is a free country from 1948! 
IL_mean_rainfall[,Year:=as.numeric(Year)] #convert  column to numeric column

#######################################################################
#Step 2
#plotting all months through the years
IL_mean_rainfall_plot  = ggplot(IL_mean_rainfall,aes(x=Year,y=Rainfall,color=Rainfall))  + geom_smooth(size=1,color="red",span=0.05)+ geom_point(shape=11) +  ggtitle("Rainfall in Israel") + ylab("PRCP") + xlab("Year") + scale_color_gradient(name="PRCP",low="blue",high="grey60") + theme_bw()
IL_mean_rainfall_plot  = IL_mean_rainfall_plot + facet_wrap(~Month,ncol=3) 
plot(IL_mean_rainfall_plot)

IL_mean_rainfall_2_8 =  IL_mean_rainfall[Month==2 | Month==8,] #make a smaller table
#plotting monthes february and august
IL_mean_rainfall_plot_2_8 = ggplot(IL_mean_rainfall_2_8,aes(x=Year,y=Rainfall,color=Rainfall))+ geom_point(shape=1) +geom_smooth(size=1,color="red",span=0.05)+  ggtitle("Rainfall in Israel in Fab and Aug") + ylab("PRCP") + xlab("Year") + scale_color_gradient(name="PRCP",low="blue",high="lightblue") + theme_bw()
IL_mean_rainfall_plot_2_8 = IL_mean_rainfall_plot_2_8 + facet_wrap(~Month,ncol=1,shrink = TRUE) 
plot(IL_mean_rainfall_plot_2_8)

#######################################################################
#Step 3
setnames(IL_mean_rainfall, "Year", "YEAR") #rename to adapt the function
setnames(IL_mean_rainfall, "Rainfall", "VALUE")
p = graph_pre_condition(IL_mean_rainfall,1948)
plot(p$PERIOD,p$MEAN, type = "l",col = "red",xlab = "Period", ylab = "Mean rainfall (mm)",main = "Mean rainfall in Israel")

