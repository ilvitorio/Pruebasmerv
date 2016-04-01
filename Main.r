###### Empieza el scrip para replicar el paper de Carr


##### Data Load #####

#Trabajo con los 32

spot <- read.delim("~/General Documents/Maqui/online_spot.txt", header=FALSE)
options <- read.delim("~/General Documents/Maqui/online_option.txt", header=FALSE)
#Cargo el dictionario spot-options
source("Dictionary.R")

##### List of Arrays for each ticker ####



#Ticker Definition
ticker <- levels(as.factor(options[["V14"]]))

#List of arrays declaration
optionBigMatrix<- vector("list",length(dictionary))
names(optionBigMatrix) <- names(dictionary)

#List of arrays for Static replication 1
staticQu<-optionBigMatrix

for (tickerName in names(optionBigMatrix)) {
  
  #Error debbuger
  #print(tickerName)
  
  #Ticker table
  tickerOption<-options[(tickerName==options["V14"]) & ("Call"==options["V15"]),]
  
  ##### Augmented Option Matrix and the Static Qu Matrix #####
  
  #Strike definition
  tickerStrike<-levels(as.factor(tickerOption[["V16"]]))
  tickerStrikeAum <- c(0,tickerStrike)
  
  #Maturity definition
  tickerMaturity <- levels(as.factor(options[["V17"]]))
  tickerMaturityAum <- c(0,tickerMaturity)
  
  #Spot retrieve
  sb<-rep(as.numeric(as.character(spot[spot[["V1"]]==dictionary[[tickerName]],5])), length(tickerMaturityAum))
  sa<-rep(as.numeric(as.character(spot[spot[["V1"]]==dictionary[[tickerName]],6])), length(tickerMaturityAum))
  
  #Allocate the 3D Array
  tickerAugmentedMatrix <- array( , dim=c(length(tickerMaturityAum),length(tickerStrikeAum),2) , dimnames = list( Maturity = tickerMaturityAum ,Strike = tickerStrikeAum , quote = c("bid","ask") ) )
  
  
  #Allocate the 2D Q Matrix
  tickerQuMatrix <- array( , dim=c(length(tickerMaturityAum),length(tickerStrikeAum)) , dimnames = list( Maturity = tickerMaturityAum ,Strike = tickerStrikeAum ) )
  
  
  #Initial Values Augmented Option Matrix
  tickerAugmentedMatrix["0",,"bid"]<- (as.numeric(dimnames(tickerAugmentedMatrix)$Strike)-sb[1])*( (as.numeric(dimnames(tickerAugmentedMatrix)$Strike)-sb[1])>0 )
  tickerAugmentedMatrix["0",,"ask"]<- (as.numeric(dimnames(tickerAugmentedMatrix)$Strike)-sa[1])*( (as.numeric(dimnames(tickerAugmentedMatrix)$Strike)-sa[1])>0 )
  tickerAugmentedMatrix[,"0","bid"]<-rep(sb[1],length(tickerMaturityAum))
  tickerAugmentedMatrix[,"0","ask"]<-rep(sa[1],length(tickerMaturityAum))
  
  #Initial Values Static Q Matrix
  tickerQuMatrix[,"0"]<-rep(1,length(tickerMaturityAum))
  
  
  
  
  #Fill the dates 
  for (i in levels(tickerOption[["V17"]])) {
    #There is an additional filter for the strikes that are elegible for each maturity
    tickerAugmentedMatrix[i, dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ,] <- as.matrix(tickerOption[(tickerOption[["V17"]]==i), 5:6]) 
  
    #Calculate the Static Q
      tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ] <- (tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"][-length(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"])]  - tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"][-1] ) / (as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"]))[-1] - as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"]))[-length(as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"])))]    )
    
    #Error Debugger
    #print(i)
    
  }
  
  #Asign the Augmented Matrix in a list position
  optionBigMatrix[[tickerName]]<-tickerAugmentedMatrix
  
  #Asign the Static ticker Matrix in a list position
  staticQu[[tickerName]]<-tickerQuMatrix
  

  
}
