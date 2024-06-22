library("RMaCzek")
library(e1071)

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


# RMaCzek
res = czek_matrix(data, order = 'GW_ward', cluster = TRUE, cluster_type = "fuzzy", num_cluster = 3, min.size = 2, eps = c(0.01, 0.05), pts = c(1, 10))
cluster_res = attr(res, "cluster_res")
acc_res = acc(true, num_cluster, cluster_res)
acc_res$acc_res
plot(res, plot_title = "Wine [GW_ward, eps = c(0.02, 0.05), pts = c(5, 15)]")




# PCA
library(ggplot2)
library(ggfortify)

data(wine, package = "HDclassif")
wine$class <- as.factor(wine$class)
wine$prediction <- as.factor(acc_res$final_result)

columns_to_exclude <- c('class', 'prediction')
selected_columns <- setdiff(names(wine), columns_to_exclude)

wine.pca <- prcomp(wine[, selected_columns], scale = TRUE)
wine <- cbind(wine, wine.pca$x)


p1 <- autoplot(wine.pca, data = wine, colour = 'class', shape = 'class', 
               loadings = FALSE, frame = TRUE, frame.type = 'norm', size = 8) +
  theme_minimal() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        legend.position = "none")
p1

p2 <- autoplot(wine.pca, data = wine, colour = 'prediction', shape = 'prediction', 
               loadings = FALSE, frame = TRUE, frame.type = 'norm', size = 8) +
  theme_minimal() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        legend.position = "none")
p2