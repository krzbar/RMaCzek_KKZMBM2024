## This file accompanies the manuscript: 
## Bartoszek, Luo Fuzzy clustering in Czekanowski's diagram

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

library(RMaCzek)
library(RCurl)
library(readr)
## description of data is at https://users.stat.ufl.edu/~winner/data/armada.txt
## colums are Battle; Year;  #Portuguese ships; #Dutch ships; #English ships; Ratio of Portuguese to Dutch/British ships; Spanish Involvement (1=Yes, 0=No); Portuguese outcome  (-1=Defeat, 0=Draw, 1=Victory)
## data is sourced from sourced from Armando da Silva Saturnino Monteiro (2001). Decline and Fall of Portuguese Seapower 1583-1663, Journal of Military History Vol. 65, #1, pp9-20.
#armada_file <- getURL("https://users.stat.ufl.edu/~winner/data/armada.dat")
##armada<-read.table(textConnection(armada_file),sep=" ")
col_nums <- fwf_cols(V1 = c(1,16), V2 = c(30, 34), V3 = c(40, 42), V4 = c(47, 50), V5 = c(56, 58), V6 = c(61, 66), V7=c(73,74), V8=c(80,82))
armada<-as.data.frame(read_fwf("https://users.stat.ufl.edu/~winner/data/armada.dat",col_positions = col_nums))
df_armada<-armada[,c(3,4,5,6,7,8)]
armada[23,1]<-"Dunas_1"
armada[24,1]<-"Dunas_2"
rownames(df_armada)<-paste(armada[,1],armada[,2],sep="_")
png("Portugese_seabattles.png");plot(RMaCzek::czek_matrix(df_armada,cluster = TRUE, cluster_type = "exact", num_cluster = 3, min.size = 2),plot_title="");dev.off()
