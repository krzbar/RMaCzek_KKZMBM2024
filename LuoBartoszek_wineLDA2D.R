## This file generates Figure 3 of the manuscript
## Luo, Bartoszek, Fuzzy clustering in Czekanowski's diagram

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


library(ContaminatedMixt)
data(wine,package="ContaminatedMixt") ## this data is also in the candisc package

library(MASS)
library(candisc)
library(multigroup)
library(RMaCzek)


modellda<-MASS::lda(Type~.,data=wine) #Type: cultivar, i.e., kind of plant, of wine
vcol<-rep("blue",nrow(wine))
vcol[which(wine$Type=="Barolo")]<-"green"
vcol[which(wine$Type=="Barbera")]<-"red"
png("Wine2DLDA.png");plot(modellda,col=vcol);dev.off()



