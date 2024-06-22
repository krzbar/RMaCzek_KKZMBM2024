## This file contains the code for the analysis (except Fig 3) in Section 4 of the manuscript
## Luo, Bartoszek, Fuzzy clustering in Czekanowski's diagram

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this code or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


library("RMaCzek") ## version 1.6.0 or later is required
library(e1071)
library(ggplot2)
library(ggfortify)
library(caret)


# Accuracy Calculation
acc <- function(true, n, res) {
  if (length(true) != length(res)) {
    stop("The lengths do not match!")
  }
  
  labels <- unique(res)
  acc_res <- 0
  all <- permutations(n)
  
  for (i in 1:nrow(all)) {
    temp <- res
    for (j in 1:n) {
      temp[res == labels[j]] <- all[i, j]
    }
    temp_acc <- sum(temp == true) / length(true)
    
    if (temp_acc > acc_res) {
      acc_res <- temp_acc
      final_result <- temp
    }
  }
  
  return(list(acc_res = acc_res, final_result = final_result))
}


# Data Processing
data(wine,package="HDclassif")
data = scale(wine[,-1])
original_lables = wine$class
lables = unique(original_lables)
num_cluster = length(lables)

true = c()
for (i in 1:nrow(wine)) {
  temp = which(lables == original_lables[i])
  true = c(true, temp)
}

res_fuzzy_fig4 = czek_matrix(data, order = 'GW_ward', cluster = TRUE, cluster_type = "fuzzy", num_cluster = 3, min.size = 2, eps = c(0.02,0.05), pts = c(1,5))
cluster_res = attr(res_exact, "cluster_res")
acc_res = acc(true, num_cluster, cluster_res)

sink(paste0("wine_GW_ward_fuzzy_fig4_accuracy.txt"))
print(acc_res$acc_res)
sink()


## PCA analysis of wine data
## Code for Figure 4
data(wine, package = "HDclassif")
wine$class <- as.factor(wine$class)
wine$prediction <- as.factor(acc_res$final_result)

columns_to_exclude <- c('class', 'prediction')
selected_columns <- setdiff(names(wine), columns_to_exclude)

wine.pca <- prcomp(wine[, selected_columns], scale = TRUE)
wine <- cbind(wine, wine.pca$x)


p1 <- autoplot(wine.pca, data = wine, colour = 'class', shape = 'class', 
               loadings = FALSE, frame = TRUE, frame.type = 'norm', size = 4) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  scale_fill_manual(values=c("red","blue","green")) +
  scale_color_manual(values=c("red","blue","green")) 
  
  
png("Wine_Dataset_true_class.png")
print(p1)
dev.off()

p2 <- autoplot(wine.pca, data = wine, colour = 'prediction', shape = 'prediction', 
               loadings = FALSE, frame = TRUE, frame.type = 'norm', size = 4) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  scale_fill_manual(values=c("red","blue","green")) +
  scale_color_manual(values=c("red","blue","green")) 
png("Wine_Dataset_predicted_class.png")
print(p2)
dev.off()

## ==============================================================

## Comparison with exact Czakanwoskis clustering
## Code for Tables 2,3,4

res_exact = czek_matrix(data, order = 'GW_ward', cluster = TRUE, cluster_type = "exact", num_cluster = 3, min.size = 2, eps = NULL, pts =  NULL)
cluster_res = attr(res_exact, "cluster_res")
acc_res_exact = acc(true, num_cluster, cluster_res)

sink("wine_GW_ward_exact_Tabs234_confusion.txt")
print("Barolo: class 1, Grignolino: class 2, Barbera: class 3")
print(caret::confusionMatrix(as.factor(acc_res_exact$final_result),wine$class))
sink()

## ==============================================================

## RMaCzek plots
## plots for Figure 5
## the resulting figures can differ from those in the manuscript as
## the fuzzy clustering methods in RMaCzek::czek_matrix() have a random component

l_Fig5params<-list(
    list(c_suffix="exact",c_clustertype="exact",eps=NULL,pts=NULL),
    list(c_suffix="01",c_clustertype="fuzzy",eps=0.01,pts=c(1,5)),	
    list(c_suffix="02",c_clustertype="fuzzy",eps=0.01,pts=c(5,15)),
    list(c_suffix="03",c_clustertype="fuzzy",eps=c(0.01,0.02),pts=c(1)),
    list(c_suffix="04",c_clustertype="fuzzy",eps=c(0.01,0.02),pts=c(1,5)),
    list(c_suffix="05",c_clustertype="fuzzy",eps=c(0.01,0.05),pts=c(1,5)),
    list(c_suffix="06",c_clustertype="fuzzy",eps=c(0.01,0.05),pts=c(5,15)),
    list(c_suffix="07",c_clustertype="fuzzy",eps=c(0.01,0.01),pts=c(1,15)),
    list(c_suffix="08",c_clustertype="fuzzy",eps=c(0.02,0.05),pts=c(1,5)),
    list(c_suffix="X",c_clustertype="fuzzy",eps=c(0.01,0.05),pts=c(1,10))
)

sapply(l_Fig5params,function(x,data,num_cluster){
#    res = czek_matrix(data, order = 'GW_ward', cluster = TRUE, cluster_type = "fuzzy", num_cluster = 3, min.size = 2, eps = c(0.01, 0.05), pts = c(1, 10))
    res = czek_matrix(data, order = 'GW_ward', cluster = TRUE, cluster_type = x$c_clustertype, num_cluster = 3, min.size = 2, eps = x$eps, pts = x$pts)
    cluster_res = attr(res, "cluster_res")
    acc_res = acc(true, num_cluster, cluster_res)
    sink(paste0("wine_GW_ward_",x$c_suffix,"_accuracy.txt"))
    print(acc_res$acc_res)
    sink()
    png(paste0("wine_GW_ward_",x$c_suffix,".png"))
    plot(res,plot_title="") ##plot_title = "Wine [GW_ward, eps = c(0.02, 0.05), pts = c(5, 15)]")
    dev.off()
    NA
},data=data,num_cluster=num_cluster,simplify=FALSE)
## =======================================================================================


