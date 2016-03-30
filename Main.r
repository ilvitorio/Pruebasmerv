###### Empieza el scrip para replicar el paper de Carr


##### Data Load #####

spot <- read.delim("~/General Documents/Maqui/online_spot.txt", header=FALSE)
options <- read.delim("~/General Documents/Maqui/online_option.txt", header=FALSE)

##### Augmented Matrix #####

petro <- options[132:142,]
petro$V14<- c(18.4,22.4,26.4,30.4,34.4,38.4,42.4,46.4,50.4,54.4,58.4) # para ejercicio
petro$V15<- as.factor(rep("Junio",length(petro$V14))) # para ejercicio
dictionary <- c("PETR","PAMP")
names(dictionary) <- c("PBR","PAM")

sb<-rep(as.numeric(as.character(spot[spot$V1=="PETR",5])), dim(petro)[1]+1)
sa<-rep(as.numeric(as.character(spot[spot$V1=="PETR",6])), dim(petro)[1]+1)
strike <- c(18.4,22.4,26.4,30.4,34.4,38.4,42.4,46.4,50.4,54.4,58.4)
strikeAum <- c(0,strike)

maturity <- c("Abril", "Mayo", "Junio", "Julio", "Agosto")
maturityAum <- c(0,maturity)


# First filling

augmentedMatrix = array( , dim=c(length(maturityAum),length(strikeAum),2) , dimnames = list( Maturity = maturityAum ,Strike = strikeAum , quote = c("bid","ask") ) )
augmentedMatrix["0",,"bid"]<- (as.numeric(dimnames(augmentedMatrix)$Strike)-sb[1])*( (as.numeric(dimnames(augmentedMatrix)$Strike)-sb[1])>0 )
augmentedMatrix["0",,"ask"]<- (as.numeric(dimnames(augmentedMatrix)$Strike)-sa[1])*( (as.numeric(dimnames(augmentedMatrix)$Strike)-sa[1])>0 )
augmentedMatrix[,"0","bid"]<-rep(sb[1],length(maturityAum))
augmentedMatrix[,"0","ask"]<-rep(sa[1],length(maturityAum))

#Attempt of filling

for (i in levels(petro$V15)) {
  
  augmentedMatrix[i,,] <- as.matrix(rbind(c(sb[1],sa[1]),petro[(petro$V15==i), 5:6]))
  
  
}

#augmentedMatrix[]
show(augmentedMatrix)
