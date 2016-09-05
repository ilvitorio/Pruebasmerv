#Function Definitions and library requirements

#Libraries required

require("BMA")
require("PerformanceAnalytics")
require("ggplot2")
require("quantmod")
require("ROCR")

#User defined Functions

initialTransform <- function(data){
        #The function makes the initial data transformation from character to numeric
        numVar<-dim(data)
        data[,1]<-as.Date(data[,1],format="%d/%m/%Y")
        data[,-1]<-apply(data[,-1],2,as.numeric)
        return(data)
}

percentileRank <- function(vectorData){
        #The function ranks a generic ranking
        aux<-round(rank(vectorData)/length(vectorData),1)
        return(aux)
}

DFRank <- function(DFxts){
        #The function ranks the data frame and returns a DF of Ranking
        burn_date<-'1999-12'
        initial_index<-paste("/",burn_date,sep="")
        indexer<-paste(burn_date,"/",sep="")
        index_dates<- index(DFxts[indexer])[-1]
        DFRanked<-DFxts
        DFRanked[initial_index,] <- apply(DFxts[initial_index],2,percentileRank)
        
        for (i in index_dates) { 
                #Computes the roll over in the XTS object for taking into account the 
                #date up to the time
                newIndex<-paste("/",as.Date(i),sep="")
                DFtemp<-DFxts[newIndex]
                newDecile<-tail(apply(DFtemp,2,percentileRank),n=1)
                DFRanked[as.Date(i),]<-newDecile
        }
        
        return(DFRanked)
}


trendExtract <- function(tsUni){
                #Perform a STL decomposition and extrats the trending time series
                frequency<-12
                STLObj<-stl(ts(tsUni,frequency=frequency),s.window="periodic")
                TObj<-STLObj$time.series[,2]
                return(TObj)
  
}

DFTrend <- function(DFxts){
  #The function performs a Seasonal-Trend Decomposition on each column 
  #and returns the DF of the trending times series in a ts object
  frequency<-12
  DFTrendObj <- apply(DFxts,2,trendExtract)
  startDate<-as.Date(start(DFxts))
  Year<-as.numeric(format(startDate,"%Y"))
  Month<-as.numeric(format(startDate,"%m"))
  DFTrendObj <- ts(DFTrendObj,start=c(Year,Month),frequency=frequency)
  return(DFTrendObj)
}

#Begins the coding

## Charge the data and make the initial data transformation
Data_Month_Level_R <- initialTransform(read.csv("~/General Documents/FED Study/FED-master/Datasets/Data_Month_Level_R.csv",stringsAsFactors=FALSE))
Data_Month_Perc_R <- initialTransform(read.csv("~/General Documents/FED Study/FED-master/Datasets/Data_Month_Perc_R.csv",stringsAsFactors=FALSE))
Data_Month_PercCh_R <- initialTransform(read.csv("~/General Documents/FED Study/FED-master/Datasets/Data_Month_PercCh_R.csv",stringsAsFactors=FALSE))
Data_Quarter_Level_R <- initialTransform(read.csv("~/General Documents/FED Study/FED-master/Datasets/Data_Month_PercCh_R.csv",stringsAsFactors=FALSE))

## Transform the variables into XTS DataFrames
Data_Month_Level_ts <- xts(Data_Month_Level_R[,-1] , order.by=Data_Month_Level_R[,1])
Data_Month_Perc_ts <- xts(Data_Month_Perc_R[,-1] , order.by=Data_Month_Perc_R[,1],frequency=12)
Data_Month_PercCh_ts <- xts(Data_Month_PercCh_R[,-1] , order.by=Data_Month_PercCh_R[,1])
Data_Quarter_Level_ts <- xts(Data_Quarter_Level_R[,-1] , order.by=Data_Quarter_Level_R[,1])

## Filter all the cases to work with
Data_Month_Level_clear   <- Data_Month_Level_ts[complete.cases(Data_Month_Level_ts)]
Data_Month_Perc_clear    <- Data_Month_Perc_ts[complete.cases(Data_Month_Perc_ts)]
Data_Month_PercCh_clear  <- Data_Month_PercCh_ts[complete.cases(Data_Month_PercCh_ts)]
Data_Quarter_Level_clear <- Data_Quarter_Level_ts[complete.cases(Data_Quarter_Level_ts)]

## Apply logarithms
Data_Month_Level_log   <- log(Data_Month_Level_clear)
#Data_Quarter_Level_log <- log(Data_Quarter_Level_clear)

## Returns of the data 1-Month, 3-Month ,6-Month and 12-Month from Log
Data_Month_Level_R1 <- diff(Data_Month_Level_log,1)
Data_Month_Level_R3 <- diff(Data_Month_Level_log,3)
Data_Month_Level_R6 <- diff(Data_Month_Level_log,6)
Data_Month_Level_R12 <- diff(Data_Month_Level_log,12)

## Returns of the data 1-Month, 3-Month ,6-Month and 12-Month from Perc
Data_Month_Perc_R1 <- diff(Data_Month_Perc_clear,1)
Data_Month_Perc_R3 <- diff(Data_Month_Perc_clear,3)
Data_Month_Perc_R6 <- diff(Data_Month_Perc_clear,6)
Data_Month_Perc_R12 <- diff(Data_Month_Perc_clear,12)

## Filter again the cases to work with
Data_Month_Level_R1  <-Data_Month_Level_R1[complete.cases(Data_Month_Level_R1)]
Data_Month_Level_R3  <-Data_Month_Level_R3[complete.cases(Data_Month_Level_R3)]
Data_Month_Level_R6  <-Data_Month_Level_R6[complete.cases(Data_Month_Level_R6)]
Data_Month_Level_R12 <-Data_Month_Level_R12[complete.cases(Data_Month_Level_R12)]


Data_Month_Perc_R1  <-Data_Month_Perc_R1[complete.cases(Data_Month_Perc_R1)]
Data_Month_Perc_R3  <-Data_Month_Perc_R3[complete.cases(Data_Month_Perc_R3)]
Data_Month_Perc_R6  <-Data_Month_Perc_R6[complete.cases(Data_Month_Perc_R6)]
Data_Month_Perc_R12  <-Data_Month_Perc_R12[complete.cases(Data_Month_Perc_R12)]



## Rank the data by deciles

#Returns by Deciles
Data_Month_Level_R1_Dec <- DFRank(Data_Month_Level_R1)
Data_Month_Level_R3_Dec <- DFRank(Data_Month_Level_R3)
Data_Month_Level_R6_Dec <- DFRank(Data_Month_Level_R6)
Data_Month_Level_R12_Dec<- DFRank(Data_Month_Level_R12)

Data_Month_Perc_R1_Dec <- DFRank(Data_Month_Perc_R1)
Data_Month_Perc_R3_Dec <- DFRank(Data_Month_Perc_R3)
Data_Month_Perc_R6_Dec <- DFRank(Data_Month_Perc_R6)
Data_Month_Perc_R12_Dec <- DFRank(Data_Month_Perc_R12)

#Percentage Variables by Deciles
Data_Month_Perc_Dec    <- DFRank(Data_Month_Perc_clear)
Data_Month_PercCh_Dec  <- DFRank(Data_Month_PercCh_clear)

#Seasonal Trend Decomposition to find local minima and maxima
Data_Month_Perc_Trend <- DFTrend(Data_Month_Perc_Dec)



a<-ts(as.vector(Data_Month_Perc_Dec$ROUTGAP),start=as.Date(start(Data_Month_Perc_Dec$ROUTGAP)),frequency=12)
stl(a,s.window="periodic")



