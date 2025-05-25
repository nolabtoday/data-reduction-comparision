# set up ----
set.seed(584)
library(devtools)
library(ggbiplot)
library(Rtsne)
library(car)
library(psych)
library(FNN)
library(Hotelling)
library(umap)
library(scatterplot3d)
library(vegan)


# importing and setting up datasets ----
  sub1test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub1test1.csv", header=FALSE)
  sub1test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub1test2.csv", header=FALSE)
  sub1test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub1test3.csv", header=FALSE)
  sub2test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub2test1.csv", header=FALSE)
  sub2test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub2test2.csv", header=FALSE)
  sub2test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub2test3.csv", header=FALSE)
  sub3test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub3test1.csv", header=FALSE)
  sub3test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub3test2.csv", header=FALSE)
  sub3test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub3test3.csv", header=FALSE)
  sub4test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub4test1.csv", header=FALSE)
  sub4test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub4test2.csv", header=FALSE)
  sub4test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub4test3.csv", header=FALSE)
  sub5test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub5test1.csv", header=FALSE)
  sub5test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub5test2.csv", header=FALSE)
  sub5test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub5test3.csv", header=FALSE)
  sub6test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub6test1.csv", header=FALSE)
  sub6test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub6test2.csv", header=FALSE)
  sub6test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub6test3.csv", header=FALSE)
  sub7test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub7test1.csv", header=FALSE)
  sub7test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub7test2.csv", header=FALSE)
  sub7test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub7test3.csv", header=FALSE)
  sub8test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub8test1.csv", header=FALSE)
  sub8test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub8test2.csv", header=FALSE)
  sub8test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub8test3.csv", header=FALSE)
  sub9test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub9test1.csv", header=FALSE)
  sub9test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub9test2.csv", header=FALSE)
  sub9test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub9test3.csv", header=FALSE)
  sub10test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub10test1.csv", header=FALSE)
  sub10test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub10test2.csv", header=FALSE)
  sub10test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub10test3.csv", header=FALSE)
  sub11test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub11test1.csv", header=FALSE)
  sub11test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub11test2.csv", header=FALSE)
  sub11test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub11test3.csv", header=FALSE)
  sub12test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub12test1.csv", header=FALSE)
  sub12test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub12test2.csv", header=FALSE)
  sub12test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub12test3.csv", header=FALSE)
  sub13test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub13test1.csv", header=FALSE)
  sub13test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub13test2.csv", header=FALSE)
  sub13test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub13test3.csv", header=FALSE)
  sub14test1 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub14test1.csv", header=FALSE)
  sub14test2 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub14test2.csv", header=FALSE)
  sub14test3 <- read.csv("~/Documents/2025/WIL2/DR project/HIV/HIV reordered/sub14test3.csv", header=FALSE)

  givecolname <- function(dataset, labnam = "sample", labnum) {
    names(dataset)[1] <- "Forward Scatter"
    names(dataset)[2] <- "Side Scatter"
    names(dataset)[3] <- "Colour 1"
    names(dataset)[4] <- "Colour 2"
    names(dataset)[5] <- "Colour 3"
    datan <- paste(labnum, collapse = "")
    assign(paste0(labnam, datan), dataset, enviroment <- .GlobalEnv)
  }

  givecolname(sub1test1,labnum = "1a")
  givecolname(sub1test2,labnum = "1b")
  givecolname(sub1test3,labnum = "1c")
  givecolname(sub2test1,labnum = "2a")
  givecolname(sub2test2,labnum = "2b")
  givecolname(sub2test3,labnum = "2c")
  givecolname(sub3test1,labnum = "3a")
  givecolname(sub3test2,labnum = "3b")
  givecolname(sub3test3,labnum = "3c")
  givecolname(sub4test1,labnum = "4a")
  givecolname(sub4test2,labnum = "4b")
  givecolname(sub4test3,labnum = "4c")
  givecolname(sub5test1,labnum = "5a")
  givecolname(sub5test2,labnum = "5b")
  givecolname(sub5test3,labnum = "5c")
  givecolname(sub6test1,labnum = "6a")
  givecolname(sub6test2,labnum = "6b")
  givecolname(sub6test3,labnum = "6c")
  givecolname(sub7test1,labnum = "7a")
  givecolname(sub7test2,labnum = "7b")
  givecolname(sub7test3,labnum = "7c")
  givecolname(sub8test1,labnum = "8a")
  givecolname(sub8test2,labnum = "8b")
  givecolname(sub8test3,labnum = "8c")
  givecolname(sub9test1,labnum = "9a")
  givecolname(sub9test2,labnum = "9b")
  givecolname(sub9test3,labnum = "9c")
  givecolname(sub10test1,labnum = "10a")
  givecolname(sub10test2,labnum = "10b")
  givecolname(sub10test3,labnum = "10c")
  givecolname(sub11test1,labnum = "11a")
  givecolname(sub11test2,labnum = "11b")
  givecolname(sub11test3,labnum = "11c")
  givecolname(sub12test1,labnum = "12a")
  givecolname(sub12test2,labnum = "12b")
  givecolname(sub12test3,labnum = "12c")
  givecolname(sub13test1,labnum = "13a")
  givecolname(sub13test2,labnum = "13b")
  givecolname(sub13test3,labnum = "13c")
  givecolname(sub14test1,labnum = "14a")
  givecolname(sub14test2,labnum = "14b")
  givecolname(sub14test3,labnum = "14c")
  
  sample14a <- sample14a[1:10000, ]
  sample14b <- sample14b[1:10000, ]
  sample14c <- sample14c[1:10000, ]
  sample4c <- sample4c[1:10000, ]
  
  
  
  sample1a <- scale(sample1a)
  sample1b <- scale(sample1b)
  sample1c <- scale(sample1c)
  sample2a <- scale(sample2a)
  sample2b <- scale(sample2b)
  sample2c <- scale(sample2c)
  sample3a <- scale(sample3a)
  sample3b <- scale(sample3b)
  sample3c <- scale(sample3c)
  sample4a <- scale(sample4a)
  sample4b <- scale(sample4b)
  sample4c <- scale(sample4c)
  sample5a <- scale(sample5a)
  sample5b <- scale(sample5b)
  sample5c <- scale(sample5c)
  sample6a <- scale(sample6a)
  sample6b <- scale(sample6b)
  sample6c <- scale(sample6c)
  sample7a <- scale(sample7a)
  sample7b <- scale(sample7b)
  sample7c <- scale(sample7c)
  sample8a <- scale(sample8a)
  sample8b <- scale(sample8b)
  sample8c <- scale(sample8c)
  sample9a <- scale(sample9a)
  sample9b <- scale(sample9b)
  sample9c <- scale(sample9c)
  sample10a <- scale(sample10a)
  sample10b <- scale(sample10b)
  sample10c <- scale(sample10c)
  sample11a <- scale(sample11a)
  sample11b <- scale(sample11b)
  sample11c <- scale(sample11c)
  sample12a <- scale(sample12a)
  sample12b <- scale(sample12b)
  sample12c <- scale(sample12c)
  sample13a <- scale(sample13a)
  sample13b <- scale(sample13b)
  sample13c <- scale(sample13c)
  sample14a <- scale(sample14a)
  sample14b <- scale(sample14b)
  sample14c <- scale(sample14c)
  
# exploratory data analysis ----
  
  s1apairs <- pairs(sample4a)
  s1acor <- cor(sample3a, method = "pearson")
  s1acor
  cortest.bartlett(s1acor, n = nrow(sample3a))
  det(s1acor)
  
# Dimension Reduction ----
  
  dimension = 3
  
  PCA1a <- prcomp(sample1a, center = TRUE, scale = TRUE)
  tSNE1a <- Rtsne(sample1a, perplexity = 25, dims = dimension)$Y
  
  PCA2a <- prcomp(sample2a, center = TRUE, scale = TRUE)
  tSNE2a <- Rtsne(sample2a, perplexity = 25, dims = dimension)$Y
  
  PCA3a <- prcomp(sample3a, center = TRUE, scale = TRUE)
  tSNE3a <- Rtsne(sample3a, perplexity = 25, dims = dimension)$Y
  
  PCA4a <- prcomp(sample4a, center = TRUE, scale = TRUE)
  tSNE4a <- Rtsne(sample4a, perplexity = 25, dims = dimension)$Y
  
  PCA5a <- prcomp(sample5a, center = TRUE, scale = TRUE)
  tSNE5a <- Rtsne(sample5a, perplexity = 25, dims = dimension)$Y
  
  PCA6a <- prcomp(sample6a, center = TRUE, scale = TRUE)
  tSNE6a <- Rtsne(sample6a, perplexity = 25, dims = dimension)$Y
  
  PCA7a <- prcomp(sample7a, center = TRUE, scale = TRUE)
  tSNE7a <- Rtsne(sample7a, perplexity = 25, dims = dimension)$Y
  
  PCA8a <- prcomp(sample8a, center = TRUE, scale = TRUE)
  tSNE8a <- Rtsne(sample8a, perplexity = 25, dims = dimension)$Y
  
  PCA9a <- prcomp(sample9a, center = TRUE, scale = TRUE)
  tSNE9a <- Rtsne(sample9a, perplexity = 25, dims = dimension)$Y
  
  PCA10a <- prcomp(sample10a, center = TRUE, scale = TRUE)
  tSNE10a <- Rtsne(sample10a, perplexity = 25, dims = dimension)$Y
  
  PCA11a <- prcomp(sample11a, center = TRUE, scale = TRUE)
  tSNE11a <- Rtsne(sample11a, perplexity = 25, dims = dimension)$Y
  
  PCA12a <- prcomp(sample12a, center = TRUE, scale = TRUE)
  tSNE12a <- Rtsne(sample12a, perplexity = 25, dims = dimension)$Y
  
  PCA13a <- prcomp(sample13a, center = TRUE, scale = TRUE)
  tSNE13a <- Rtsne(sample13a, perplexity = 25, dims = dimension)$Y
  
  PCA14a <- prcomp(sample14a, center = TRUE, scale = TRUE)
  tSNE14a <- Rtsne(sample14a, perplexity = 25, dims = dimension)$Y
  
# DR evaluation ----
  
  summary(PCA3a)
  
  knn_retention <- function(HDdata, LDdata, k=30) {
    HDknn <- get.knn(HDdata, k=k)$nn.index
    LDknn <- get.knn(LDdata, k=k)$nn.index
    knnret <- sapply(1:nrow(HDdata), function(i) {
      length(intersect(HDknn[i, ], LDknn[i, ])) / k
    })
    return(mean(knnret))
  }
  
  sample_names <- ls(pattern = "^sample\\d+a$")
  
  pca_names <- data.frame(ls(pattern = "^PCA\\d+a$"), sample_names)
  tsne_names <- data.frame(ls(pattern = "^tSNE\\d+a$"), sample_names)
 
  pcaknn <- do.call(rbind, lapply(1:nrow(pca_names), function(i) {
    pcadata <- get(pca_names[i, 1])
    sampledata <- get(pca_names[i, 2])
    knn_retention(sampledata, pcadata$x[, 1:3])
  }))
  
  tsneknn <- do.call(rbind, lapply(1:nrow(tsne_names), function(i) {
    pcadata <- get(tsne_names[i, 1])
    sampledata <- get(tsne_names[i, 2])
    knn_retention(sampledata, pcadata)
  }))
  
  Name <- c(10,11,12,13,14,1,2,3,4,5,6,7,8,9)
  
  knnscores <- data.frame(tsneknn, pcaknn, Name)
  
  knnscores <- knnscores[order(knnscores$Name), ]
  
  knnscores
  
  colMeans(knnscores)
  
    # Reconstruction Error  
  
  ReconEr <- function(HDdata, LDdata, d = 3) {
    recdata <- LDdata$x[, 1:d] %*% t(LDdata$rotation[, 1:d])
    MNrecdata <- scale(recdata, center = -colMeans(HDdata), scale = FALSE)
    RecEr <- mean((HDdata - MNrecdata)^2)
    return(RecEr)
  }
  
  pcareconer <- do.call(rbind, lapply(1:nrow(pca_names), function(i) {
    pcadata <- get(pca_names[i, 1])
    sampledata <- get(pca_names[i, 2])
    ReconEr(sampledata, pcadata)
  }))
  
  Reconerror <- data.frame(pcareconer, Name)
  Reconerror <- Reconerror[order(Reconerror$Name), ]
  
  Reconerror
  
  colMeans(Reconerror)
  
# DR Grouping ----
    
  # PCA
  
    pca_names <- ls(pattern = "^PCA\\d+a$")
  
  pca_summary_3D <- do.call(rbind, lapply(pca_names, function(name) {
    dataset <- get(name)
    colMeans(dataset$x[, 1:3])
  }))
  
  rownames(pca_summary_3D) <- pca_names
  colnames(pca_summary_3D) <- c("PC1", "PC2", "PC3")
  
    # hierarchy
  hc <- hclust(dist(pca_summary_3D))
  plot(hc, main = "Clustering PCA datasets (3 Dimensions)")
  rect.hclust(hc, k = 2, border = "red")
  
  clusters_3D <- cutree(hc, k = 2)
  clusters_3D
  
    # k means
  kmeans_result <- kmeans(pca_summary, centers = 2)
  kmeans_clusters <- kmeans_result$cluster
  kmeans_clusters
  
  # t-SNE
  
  tsne_names <- ls(pattern = "^tSNE\\d+a$")
  
  tsne_summary_3D <- do.call(rbind, lapply(tsne_names, 
                                           function(name) {
                                             dataset <- get(name)
                                             colMeans(dataset)
                                           }))
  
  rownames(tsne_summary_3D) <- tsne_names
  
    # hierarchy
  hc <- hclust(dist(tsne_summary_3D))
  plot(hc, main = "Clustering t-SNE datasets (3 Dimensions)")
  rect.hclust(hc, k = 2, border = "red")
  
  clusters_3D <- cutree(hc, k = 2)
  clusters_3D
  
    # k means
  kmeans_result <- kmeans(tsne_summary_3D, centers = 2)
  kmeans_clusters <- kmeans_result$cluster
  kmeans_clusters
  
  # Groupings
  
  # k-NN classifier 
  
  # t-SNE
  
  tsneknng1 <- c(1,2,3,4,5,6,8,10,12,13)
  
  tsneknng2 <- c(7,9,11,14)
  
  # PCA
  
  pcaknng1 <- c(1,2,4,5,6,7,8,9,10,11,12,13,14)

  pcaknng2 <- c(3)
  
  # Hierarchical Clustering
  
  # t-SNE
  
  tsnehcg1 <- c(1,2,3,4,5,8,10,12,13)
  
  tsnehcg2 <- c(6,7,9,11,14)
  
  # PCA
  
  tsnehcg1 <- c(6,8,10,12,13)
  
  tsnehcg2 <- c(1,2,3,4,5,7,9,11,14)
  
# DR Graphing ----
  
  pca_name <- data.frame(ls(pattern = "^PCA\\d+a$"), Name)
  tsne_name <- data.frame(ls(pattern = "^tSNE\\d+a$"), Name)
  
  pca_list <- pca_list[order(pca_name$Name), ]
  tsne_list <- tsne_list[order(tsne_name$Name), ]
  
  tsneknn <- do.call(rbind, lapply(1:nrow(tsne_list), function(i) {
    tsne <- get(tsne_list[i, 1])
    par(mfrow = c(2,2))
    scatterplot3d(tsne)
    scatterplot3d(tsne, angle = 135)
    scatterplot3d(tsne, angle = 225)
    scatterplot3d(tsne, angle = 315)
    mtext(paste("t-SNE for Subject", i), side = 3, line = -1, outer = TRUE)
    par(mfrow = c(1,1))
    plot(tsne, main = paste("t-SNE Overhead for Subject", i))
  }))
  
  pcaknn <- do.call(rbind, lapply(1:nrow(pca_list), function(i) {
    pc <- get(pca_list[i, 1])
    par(mfrow = c(2,2))
    scatterplot3d(pc$x[, 1:3])
    scatterplot3d(pc$x[, 1:3], angle = 135)
    scatterplot3d(pc$x[, 1:3], angle = 225)
    scatterplot3d(pc$x[, 1:3], angle = 315)
    mtext(paste("PCA for Subject", i), side = 3, line = -1, outer = TRUE)
    par(mfrow = c(1,1))
    plot(pc$x, main = paste("PCA Overhead for Subject", i))
  }))
  
  par(mfrow = c(2,2))
  scatterplot3d(PCA1a$x[, 1:3], main = "Subject 1", angle = 315)
  scatterplot3d(PCA2a$x[, 1:3], main = "Subject 2", angle = 315)
  scatterplot3d(PCA7a$x[, 1:3], main = "Subject 7", angle = 315)
  scatterplot3d(PCA9a$x[, 1:3], main = "Subject 9", angle = 315)
  mtext(paste("PCA angle 315"), side = 3, line = -1, outer = TRUE)
  par(mfrow = c(1,1))
  
  plot(pc$x, main = paste("PCA Overhead for Subject", i))
    
  par(mfrow = c(2,2))
  scatterplot3d(PCA7a$x[, 1:3], angle = 45)
  scatterplot3d(PCA7a$x[, 1:3], angle = 135)
  scatterplot3d(PCA7a$x[, 1:3], angle = 225)
  scatterplot3d(PCA7a$x[, 1:3], angle = 315)
  mtext(paste("PCA Subject 7"), side = 3, line = -1, outer = TRUE)
  par(mfrow = c(1,1))
  
  par(mfrow = c(2,2))
  scatterplot3d(tSNE1a, angle = 45)
  scatterplot3d(tSNE1a, angle = 135)
  scatterplot3d(tSNE1a, angle = 225)
  scatterplot3d(tSNE1a, angle = 315)
  mtext(paste("t-SNE Subject 1"), side = 3, line = -1, outer = TRUE)
  par(mfrow = c(1,1))
  
  par(mfrow = c(2,2))
  scatterplot3d(tSNE7a, angle = 45)
  scatterplot3d(tSNE7a, angle = 135)
  scatterplot3d(tSNE7a, angle = 225)
  scatterplot3d(tSNE7a, angle = 315)
  mtext(paste("t-SNE Subject 7"), side = 3, line = -1, outer = TRUE)
  par(mfrow = c(1,1))
  
  plot(PCA7a$x, main = paste("PCA Overhead for Subject 7"))
  
  scatterplot3d(tSNE14a, angle = 135, 
                main = "Group 1; Subject 14 t-SNE Reduction")
  scatterplot3d(PCA14a$x[, 1:3], angle = 135, main = "Group 1; Subject 14 PCA")
  
  scatterplot3d(tSNE11a, angle = 135, 
                main = "Group 1; Subject 11 t-SNE Reduction")
  scatterplot3d(PCA11a$x[, 1:3], angle = 135, main = "Group 1; Subject 11 PCA")
  
  scatterplot3d(tSNE13a, angle = 135, 
                main = "Group 2; Subject 13 t-SNE Reduction")
  scatterplot3d(PCA13a$x[, 1:3], angle = 135, main = "Group 2; Subject 13 PCA")
  
  scatterplot3d(tSNE12a, angle = 135, 
                main = "Group 2; Subject 12 t-SNE Reduction")
  scatterplot3d(PCA12a$x[, 1:3], angle = 135, main = "Group 2; Subject 12 PCA")
  
# HD graphing example ----
  a1_300 <- sample1a[sample(nrow(sample1a), 300), ]
  
  point_sizes <- scales::rescale(a1_300[ ,4], to = c(0.5, 2))
  
  color_gradient <- colorRampPalette(c("blue", "green", "red"))
  num_colors <- 100
  colors <- color_gradient(num_colors)
  colour3_scaled <- as.numeric(cut(a1_300[ ,5], breaks = num_colors))
  point_colours <- colors[colour3_scaled]
  
  scatterplot3d(x = a1_300[ ,1],
                y = a1_300[ ,2],
                z = a1_300[ ,3],
                color = point_colours,
                cex.symbols = point_sizes,
                main = '5d scatterplot',
                xlab = 'Forward Scatter',
                ylab = 'Side Scatter',
                zlab = 'Color 1')
  
  tSNE1a_300 <- Rtsne(a1_300, perplexity = 10, dims = 2)$Y
  
  plot(
    x = tSNE1a_300[ ,1],
    y = tSNE1a_300[ ,2],
    xlab = "t-SNE 1",
    ylab = "t-SNE 2",
    main = "2D Scatterplot of 5d data after t-SNE")
  