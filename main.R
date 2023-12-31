##Vital Sign Visualization##


##Libraries/Variables##
library(tidyverse)
library(readxl)
library(tictoc)
library(conflicted)
library(plotly)
library(jsonlite)
library(reshape2)
library(ggtext)
library(lubridate)
wd<-getwd()
BeginDate<-as.POSIXct(Sys.time()-days(5), tz = 'America/New_York')
EndDate<-as.POSIXct(Sys.time(), tz = 'America/New_York')

tic("Read in data and coerce.")

##Read in manual blood pressure and heart rate data.##

BPData<-read_xlsx("BloodPressureDevice.xlsx")
BPDataMelt<-melt(BPData, id.vars="DateTimeGroup")


##Handle FitBit Data.##
FitBitZip<-grep("takeout", list.files(file.path(wd,"FitBit")), value=TRUE)
FitBitZipPath<-file.path(wd,"FitBit",FitBitZip)
zipped_heart_rate <- grep(paste0("Takeout/","FitBit/","Global Export Data/",'heart_rate.'), unzip(file.path(wd,"FitBit",FitBitZip), list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)
unzip(FitBitZipPath, files=zipped_heart_rate, exdir = file.path(wd, "FitBit","FitBitHeartRate"), overwrite = TRUE)
HeartRateList<-list.files(path=wd, pattern = "heart_rate", recursive = TRUE)

for(i in 1:length(HeartRateList)){
  if(i==1){
    HeartRateDF<-fromJSON(txt=HeartRateList[i], flatten = TRUE)
  }else{
    HeartRateAdd<-fromJSON(txt=HeartRateList[i], flatten=TRUE)
    HeartRateDF<-bind_rows(HeartRateDF,HeartRateAdd)
  }
  
}
##Filtering based on research that higher confidence values are better. 0 and 1##
##may be unreliable.##
##https://towardsdatascience.com/when-your-fitbit-says-your-heart-is-exploding-should-you-care-4e47aa5bf452##
HeartRateDF<-dplyr::filter(HeartRateDF, value.confidence==2|value.confidence==3)
##Coercing DTG to POSIXct, then changing the value to local time, then back to POSIXct##
HeartRateDF$dateTime<-as.POSIXct(HeartRateDF$dateTime, format="%m/%d/%y %H:%M:%S", tz="UTC")
HeartRateDF$dateTime<-format(HeartRateDF$dateTime,tz="America/New_York", usetz = TRUE)
HeartRateDF$dateTime<-as.POSIXct(HeartRateDF$dateTime, tz="America/New_York")
HeartRateDF<-dplyr::filter(HeartRateDF,dateTime>=BeginDate&dateTime<=EndDate)
##Creating a separate DF to create a box plot of non-active heart (<130 BPM)##
##that spans the entire data set.##
HeartRateDFBox<-HeartRateDF
HeartRateDFBox<-dplyr::filter(HeartRateDFBox,value.bpm<=130)

toc()


tic("Plotting")

##Manual Blood Pressure Data.#

BPPlot<-ggplot(BPDataMelt,aes(x = DateTimeGroup, y = value, color = variable)) + 
  geom_point()+
  labs(x="Date and Time (Local)", y=expression(Value *" "* (mmHG*", " * BPM *", "* or * " %" * SpO[2])), title = "Manual Vitals Data")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(color='Vital Sign')
BPPlot

BPPlotly<-ggplot(BPDataMelt,aes(x = DateTimeGroup, y = value, color = variable)) + 
  geom_point()+
  labs(x="Date and Time (Local)", y="Value (mmHG, BPM, or %SPO2)", title = "Manual Vitals Data")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(color='Vital Sign')

BPPlotly<-ggplotly(BPPlotly)
BPPlotly

##FitBit Heart Rate Plot.##
HeartRatePlot<-ggplot(HeartRateDF, aes(x=dateTime, y=value.bpm))+
  geom_point(size = 1, color = "orange2", alpha = 0.5) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 10, bs = "cs"),
              fill = "orange", color = "orange4", linetype = 2)+
  labs(x="Date and Time (Local)", y="Heartbeats Per Minute", title = "FitBit Heartbeat Data (Last Five Days)")+
  theme(plot.title = element_text(hjust = 0.5))
  

HeartRatePlot

HeartRatePlot<-ggplotly(HeartRatePlot)
HeartRatePlot

##Box Plot for FitBit Heart Rate Data.##

HeartRateBoxPlot <- ggplot(HeartRateDFBox, aes(y=value.bpm)) + 
  geom_boxplot()+ 
  scale_x_discrete( ) +
  labs(title = paste0("FitBit Inactive Heart Rate Boxplot (Last Five Days) N= ", nrow(HeartRateDFBox), " Samples"),
       y = "Heart Rate (BPM)")+
  theme(plot.title = element_text(hjust = 0.5))
HeartRateBoxPlot
HeartRateBoxPlot<-ggplotly(HeartRateBoxPlot)
HeartRateBoxPlot

toc()
