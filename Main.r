###### Empieza el scrip para replicar el paper de Carr

#### Set warnings as errors ####
#options(warn=2)

#Librería con utilidades
require("taRifx")
require("data.table")
##### Data Load #####

#Trabajo con los 32

#Para esperar el command prompt
#myArgs = commandArgs(trailingOnly=TRUE)

#Carga Lab
spot <- read.delim("~/General Documents/Maqui/online_spot.txt", header=FALSE)
options <- read.delim("~/General Documents/Maqui/online_option.txt", header=FALSE)

#Carga desde la casa
#spot <- read.delim("~/Maquinola/Live/Pruebasmerv-master/online_spot.txt", header=FALSE)
#options <- read.delim("~/Maquinola/Live/Pruebasmerv-master/online_option.txt", header=FALSE)

#Carga para el Backtesting
#spot <- read.delim(myArgs[1], header=FALSE)
#options <- read.delim(myArgs[2], header=FALSE)


#Transformo numeros del dataframe leído

spot[,4]<-as.numeric(as.character(spot[,4]))
spot[,5]<-as.numeric(as.character(spot[,5]))
spot[,6]<-as.numeric(as.character(spot[,6]))
spot[,7]<-as.numeric(as.character(spot[,7]))

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

#List of arrays for the Butterfly run
Butterfly<-optionBigMatrix

#Initial vector of filling for the book
intialOrderNum<-rep(0,length(dictionary)*20)

intialOrderChar<-rep("0",length(dictionary)*20)

#Initialize the OrderBook
bigOrderBookQu<-data.table(type=      intialOrderChar,
                         name=      intialOrderChar,
                         order1=    intialOrderChar,
                         ticker1=   intialOrderChar,
                         quantity1= as.integer(intialOrderNum),
                         price1=    intialOrderNum,
                         order2=    intialOrderChar,
                         ticker2=   intialOrderChar,
                         quantity2= as.integer(intialOrderNum),
                         price2=    intialOrderNum)

#Initialize the OrderBook
bigOrderBookBs<-data.table(type=      intialOrderChar,
                         name=      intialOrderChar,
                         order1=    intialOrderChar,
                         ticker1=   intialOrderChar,
                         quantity1= as.integer(intialOrderNum),
                         price1=    intialOrderNum,
                         order2=    intialOrderChar,
                         ticker2=   intialOrderChar,
                         quantity2= as.integer(intialOrderNum),
                         price2=    intialOrderNum,
                         order3=    intialOrderChar,
                         ticker3=   intialOrderChar,
                         quantity3= as.integer(intialOrderNum),
                         price3=    intialOrderNum)





#Initialize OrderCount
orderCountQu<-1L
orderCountBs<-1L

for (tickerName in names(optionBigMatrix)) {
  
  #Error debbuger
  #print(tickerName)
  
  #Ticker table
  tickerOption<-options[(tickerName==options["V14"]) & ("Call"==options["V15"]),]
  
  if(!(dim(tickerOption)[1]==0))
  {
    
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
    
    
    #Allocate the 2D Q Matrix and Butterfly Matrix (This is declare as an array, if you want to gain speed redo everything with a data.table (10x faster))
    tickerQuMatrix <- array( , dim=c(length(tickerMaturityAum),length(tickerStrikeAum)) , 
                            dimnames = list( Maturity = tickerMaturityAum ,Strike = tickerStrikeAum ) )
    
    #Allocate the memory for the data table of buttreflies
    tickerBsMatrix <- data.table(maturity = tickerMaturityAum)
    
    if( length(tickerStrikeAum)>=3 )
    {
      #Set the dimension for all the data table of butterflies
      set( tickerBsMatrix, 1:length(tickerMaturityAum) ,tickerStrikeAum[-c(1,length(tickerStrikeAum))], 
           as.numeric(rep(NA,length(tickerMaturityAum))) )
      
      #Set the key for searchs in the butterflies data table
      setkey(tickerBsMatrix,maturity)
    }
    
    #Initial Values Augmented Option Matrix
    tickerAugmentedMatrix["0",,"bid"]<- (sb[1]-as.numeric(dimnames(tickerAugmentedMatrix)$Strike))*( (sb[1]-as.numeric(dimnames(tickerAugmentedMatrix)$Strike))>0 )
    tickerAugmentedMatrix["0",,"ask"]<- (sa[1]-as.numeric(dimnames(tickerAugmentedMatrix)$Strike))*( (sa[1]-as.numeric(dimnames(tickerAugmentedMatrix)$Strike))>0 )
    tickerAugmentedMatrix[,"0","bid"]<-rep(sb[1],length(tickerMaturityAum))
    tickerAugmentedMatrix[,"0","ask"]<-rep(sa[1],length(tickerMaturityAum))
    
    #Initial Values Static Q Matrix
    tickerQuMatrix[,"0"]<-rep(1,length(tickerMaturityAum))
    
    
    
    #Fill the dates 
    for (i in levels(tickerOption[["V17"]])) {
      #There is an additional filter for the strikes that are elegible for each maturity
      tickerAugmentedMatrix[i, dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ,] <- as.matrix(tickerOption[(tickerOption[["V17"]]==i), 5:6]) 
      
      
      
      
      ##### Calculate the Static Q Matrix ####
      tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ] <- (tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"][-length(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"])]  - tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"][-1] ) / (as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"]))[-1] - as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"]))[-length(as.numeric(names(tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"])))]    )
      
      #Retrieve the order information
      
      #The shorts for that Maturity
      
      #The names part filters the strike for that given maturity then it takes the proper strikes for the shorts
      tickerMaturityLong<- names(tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ])[taRifx::shift(tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ]<0)]
      
      #The names part filters the strike for that given maturity then it takes the proper strikes for the longs
      tickerMaturityShort<- names(tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ])[tickerQuMatrix[i,dimnames(tickerAugmentedMatrix)[["Strike"]] %in% tickerOption[(tickerOption[["V17"]]==i), 16] ]<0]
      
      #Order Information Filtering
      tickerQuOrderPanelLong<-na.omit(tickerOption[  (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerMaturityLong)  ,])
      tickerQuOrderPanelShort<-na.omit(tickerOption[ (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerMaturityShort) ,])
      tickerQuOrderNumber<-as.integer(dim(tickerQuOrderPanelLong)[1])
      
      
      
      
      ##Test for ticker names
      #print(tickerName)
      #print(tickerQuOrderNumbe)
      
      
      #Assign orders to the Book if the orders exist
      if(!(tickerQuOrderNumber==0L)) {
        
        #Order construction
        tickerOrderQuMatrix<-cbind( rep("A",tickerQuOrderNumber),
                                  rep("V",tickerQuOrderNumber), 
                                  rep("B",tickerQuOrderNumber), tickerQuOrderPanelLong [,c(1,7,6)],
                                  rep("S",tickerQuOrderNumber), tickerQuOrderPanelShort[,c(1,4,5)] )
        #Order Booking
        set(bigOrderBookQu, (orderCountQu):(orderCountQu+tickerQuOrderNumber-1) , 1L:10L , tickerOrderQuMatrix)
        
        #Update the Order Index
        orderCountQu<-orderCountQu + tickerQuOrderNumber
        
      }
      
      
      ##### Calculate the Butterfly Matrix #####
      
      if( length(na.omit(tickerAugmentedMatrix[i,,"bid"]))>=3 )
      {
        
        tickerButterfly<- tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"][-c(length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))) ,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"])))-1)] +                                                               #Takes the previous call value
          (
            as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-(1:2)]) -
              
              as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))) ,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"])))-1)])
          )/ #Calculates the first numerator
          (
            as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-(1:2)]) -
              
              as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))))])
          )* #Calculates the first denominator 
          
          tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"bid"]),"bid"][-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"bid"]))) )]+ #Takes the actual call value                                                                                                                      
          (
            as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))))])-  
              as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))) ,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"])))-1)])
          )/
          
          (
            as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-(1:2)])-
              as.numeric(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))))])
          )*
          tickerAugmentedMatrix[i, !is.na(tickerAugmentedMatrix[i,,"ask"]),"ask"][-(1:2)]
        
        
        
        set(tickerBsMatrix,                          #The data table
            tickerBsMatrix[,.I[maturity==i]],        #The maturity filter
            names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))))] , #The Strike Filter
            as.list(tickerButterfly) ) #Value assignment (Has to be as a list)
        
        #Getting the positions from the Butterflies
        
        ##Long positions##
        
        #Lagged Positions
        tickerLeftBsLong<- names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(length(names(na.omit(tickerAugmentedMatrix[i,,"ask"])))-1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))) )][na.omit((tickerBsMatrix[i]<0)[-1])]
        #Posterior Positions
        tickerRightBsLong<- names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,2)][na.omit((tickerBsMatrix[i]<0)[-1])]
        
        ##Short positions##
        #Middle positions
        tickerMiddleBsShort<- names(na.omit(tickerAugmentedMatrix[i,,"ask"]))[-c(1,length(names(na.omit(tickerAugmentedMatrix[i,,"ask"]))) )][na.omit((tickerBsMatrix[i]<0)[-1])]
        
        
        ##Panel Order retrieval##
        ##Implementation of a 0 Strike
        if(tickerLeftBsLong[1]==0){
          tickerOrderPanelBsLeftLong   <-na.omit(tickerOption[  (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerLeftBsLong)  ,])  
          tickerOrderPanelBsLeftLong   <-rbind(dictionary[[tickerName]],rep(0,2),as.numeric(as.character(unlist(spot[spot$V1==dictionary[[tickerName]],c(4,5,6,7)],use.name=F))),
                                               rep(0,10),
                                               data.frame(tickerOrderPanelBsLeftLong))
        }else{
          tickerOrderPanelBsLeftLong   <-na.omit(tickerOption[  (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerLeftBsLong)  ,])  
        }
        
        #Retrieval of the order for the Butterfly
        tickerOrderPanelBsRightLong  <- na.omit(tickerOption[  (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerRightBsLong)  ,])
        tickerOrderPanelBsMiddleShort <- na.omit(tickerOption[  (tickerOption["V17"]==i)  & (tickerOption[["V16"]] %in% tickerMiddleBsShort)  ,])
       
        tickerOrderNumberBs<-as.integer(dim(tickerOrderPanelBsMiddleShort)[1])
        
        
        
        #Assign orders to the Book if the orders exist
        if(!(tickerOrderNumberBs==0L)) {
          
          #Order construction
          tickerOrderBsMatrix<-cbind( rep("A",tickerOrderNumberBs),
                                    rep("BS",tickerOrderNumberBs), 
                                    rep("B",tickerOrderNumberBs), cbind(tickerOrderPanelBsLeftLong[,1],as.numeric(tickerOrderPanelBsLeftLong[,6]),as.numeric(tickerOrderPanelBsLeftLong[,7])), 
                                    rep("B",tickerOrderNumberBs), tickerOrderPanelBsRightLong    [,c(1,7,6)],
                                    rep("S",tickerOrderNumberBs), tickerOrderPanelBsMiddleShort  [,c(1,4,5)])
          #Order Booking
          set(bigOrderBookBs, (orderCountBs):(orderCountBs+tickerOrderNumberBs-1) , 1L:14L , tickerOrderBsMatrix)
          
          #Update the Order Index
          orderCountBs<-orderCountBs + tickerOrderNumberBs
          
        }
        
        
        
        
        
        
      }
      
      #Error Debugger
      #print(i)
      
    }
    
    #Asign the Augmented Matrix in a list position
    optionBigMatrix[[tickerName]]<-tickerAugmentedMatrix
    
    #Asign the Static ticker Matrix in a list position
    staticQu[[tickerName]]<-tickerQuMatrix
    
    #Asign the BS Matrix in the list position
    Butterfly[[tickerName]]<-tickerBsMatrix
    
  }
  
  
}
