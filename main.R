##Vital Sign Visualization##


##Libraries/Variables##
library(tidyverse)
library(readxl)
library(tictoc)
library(conflicted)
library(plotly)
library(jsonlite)
library(TAF)
wd<-getwd()

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

HeartRateDF$dateTime<-as.POSIXct(HeartRateDF$dateTime, format="%m/%d/%y %H:%M:%S", tz="UTC")
HeartRateDF$dateTime<-format(HeartRateDF$dateTime,tz="America/New_York", usetz = TRUE)
HeartRateDF$dateTime<-as.POSIXct(HeartRateDF$dateTime, tz="America/New_York")
toc()


tic("Plotting")
HeartRatePlot<-ggplot(HeartRateDF, aes(x=dateTime, y=value.bpm))+
  geom_point(size = 0.1, color = "orange2", alpha = 0.5) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 10, bs = "cs"),
              fill = "orange", color = "orange4", linetype = 2)



HeartRatePlot<-ggplotly(HeartRatePlot)
HeartRatePlot
