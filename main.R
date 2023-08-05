##Vital Sign Visualization##


##Libraries/Variables##
library(tidyverse)
library(readxl)
library(tictoc)
library(conflicted)
library(plotly)
library(jsonlite)
wd<-getwd()
BeginDate<-as.POSIXct('2023-07-01 00:00', tz = 'America/New_York')
EndDate<-as.POSIXct('2023-08-04 23:59', tz = 'America/New_York')

tic("Read in data and coerce.")
FitBitZip<-grep("takeout", list.files(file.path(wd,"FitBit")), value=TRUE)
FitBitZipPath<-file.path(wd,"FitBit",FitBitZip)
##Read in manual blood pressure and heart rate data.##

BPData<-read_xlsx("BloodPressureDevice.xlsx")


##Handle FitBit Data.##
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
toc()


tic("Plotting")
HeartRatePlot<-ggplot(HeartRateDF, aes(x=dateTime, y=value.bpm))+
  geom_point(size = 1, color = "orange2", alpha = 0.5) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 10, bs = "cs"),
              fill = "orange", color = "orange4", linetype = 2)

HeartRatePlot

HeartRatePlot<-ggplotly(HeartRatePlot)
HeartRatePlot
toc()
