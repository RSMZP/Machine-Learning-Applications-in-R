# Hide warnings globally
options(warn=-1) 

# Load packages
library(tidyverse)
library(ggplot2)
library(cluster)
library(GGally) # Visualising the correlations
library(grid) # Plotting the grid 
library(gridExtra) # Plotting the grid

# Set random seed
set.seed(123)

redWine <- read.csv("red_wine.csv", header = T,row.names = 1)
whiteWine <- read.csv("white_wine.csv", header = T, row.names = 1)

# Red wines
# YOUR CODE HERE
dim(redWine)

# White wines
# YOUR CODE HERE
dim(whiteWine)

wine <- rbind(cbind(redWine,colour = "red"),
              cbind(whiteWine,colour = "white"))
wine

options(repr.plot.width=16, repr.plot.height=10)

wine %>% 
    gather(1:12, key = "variables", value = "result") %>%
    ggplot(aes(result, fill = colour)) +
    geom_density(alpha = 0.5)+
    theme_classic()+
    facet_wrap(.~variables, scale = "free")

options(repr.plot.width=20, repr.plot.height=16)

ggpairs(wine, 
        mapping = ggplot2::aes(color = colour),
        lower = list(continuous = wrap("points", alpha = 0.3), combo ='dot_no_facet'),
        upper = list(combo = 'box_no_facet'))

options(repr.plot.width=20, repr.plot.height=16)

p1 = wine  %>% 
    ggplot(aes(x = quality, y = fixed.acidity))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p2 = wine  %>% 
    ggplot(aes(x = quality, y = volatile.acidity))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p3 = wine  %>% 
    ggplot(aes(x = quality, y = citric.acid))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p4 = wine  %>% 
    ggplot(aes(x = quality, y = residual.sugar))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p5 = wine  %>% 
    ggplot(aes(x = quality, y = chlorides))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p6 = wine  %>% 
    ggplot(aes(x = quality, y = free.sulfur.dioxide))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p7 = wine  %>% 
    ggplot(aes(x = quality, y = total.sulfur.dioxide))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p8 = wine  %>% 
    ggplot(aes(x = quality, y = density))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p9 = wine  %>% 
    ggplot(aes(x = quality, y = pH))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p10 = wine  %>% 
    ggplot(aes(x = quality, y = sulphates))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

p11 = wine  %>% 
    ggplot(aes(x = quality, y = alcohol))+
    geom_jitter(aes(col=colour),size =3, alpha =0.5)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,
             nrow = 4,
             ncol = 3)

wine  %>% 
    ggplot(aes(x = total.sulfur.dioxide,
               y = alcohol,
               col = as.factor(quality),
               shape = colour))+
    geom_point(size = 4,alpha = 0.5)

wine_scaled  <- wine[,c(1:12)]  %>% 
    scale(center = TRUE, scale = TRUE)  %>% 
    as.data.frame()

# YOUR CODE HERE
silhouette_vec <- vector()
k <- vector()
distances <- dist(wine[,1:12])

# YOUR CODE HERE
for(i in 2:20) {
  
  model <- kmeans(wine_scaled, i)
  ss <- silhouette(model$cluster, distances)
  silhouette_vec[i] <- mean(ss[,3])
  k[i] <- i
}

# YOUR CODE HERE
model_statistics <- data.frame(
  cluster_number = k[2:20],
  silhouette_score = silhouette_vec[2:20]
)

# YOUR CODE HERE
ggplot(model_statistics, aes(x = cluster_number, y = silhouette_score)) +
  geom_point() + geom_line()

# YOUR CODE HERE
for(i in 2) {
  
  kmeans_model_2 <- kmeans(wine_scaled, i)
  ss <- silhouette(model$cluster, distances)
  silhouette_vec[i] <- mean(ss[,3])
  k[i] <- i
}

wine  %>% 
    mutate(cluster = as.factor(kmeans_model_2$cluster),
           colour = wine$colour)  %>% 
    ggplot(aes(x = total.sulfur.dioxide,
               y = alcohol,
               col = cluster,
               shape = colour)) +
    geom_point(size = 4,alpha = 0.5)


# YOUR CODE HERE
for(i in 3) {
  
  kmeans_model_3 <- kmeans(wine_scaled, i)
  ss <- silhouette(model$cluster, distances)
  silhouette_vec[i] <- mean(ss[,3])
  k[i] <- i
}

# YOUR CODE HERE
wine  %>% 
    mutate(cluster = as.factor(kmeans_model_3$cluster),
           colour = wine$colour)  %>% 
    ggplot(aes(x = total.sulfur.dioxide,
               y = alcohol,
               col = cluster,
               shape = colour)) +
    geom_point(size = 4,alpha = 0.5)

wine[,1:12]  %>% 
    mutate(clusters = kmeans_model_3$cluster) %>% 
    aggregate(by = list(.$clusters),
             FUN = mean)

# YOUR CODE HERE
hc <- hclust(distances) 

# YOUR CODE HERE
hcluster_2 <- cutree(hc, 2)
rsf_5 <- wine_scaled[,]%>%
          mutate(cluster =  as.factor(hcluster_2))

ggplot(rsf_5) + geom_point(aes(x = total.sulfur.dioxide, y = alcohol, color = cluster))


# Plot the clusters
wine  %>% 
    mutate(cluster = as.factor(hcluster_2),
           colour = wine$colour)  %>% 
    ggplot(aes(x = total.sulfur.dioxide,
               y = alcohol,
               col = cluster,
               shape = colour)) +
    geom_point(size = 4,alpha = 0.5)

# YOUR CODE HERE
hcluster_3 <- cutree(hc, 3)
rsf_6 <- wine_scaled[,]%>%
          mutate(cluster =  as.factor(hcluster_3))

ggplot(rsf_6) + geom_point(aes(x = total.sulfur.dioxide, y = alcohol, color = cluster))
