library(tidyverse)
library(ggplot2)
library(cluster)

# Load the movies data set
movies <- read.csv("movies.csv", header = T)
# Load the ratings data set
ratings <- read.csv("ratings.csv", header = T)

# Number of movies
dim(movies)

# Number of ratings
dim(ratings)

length(unique(ratings$userId))

ratings_per_user <- ratings %>% 
                      group_by(userId) %>% 
                      count() 
summary(ratings_per_user$n)

movies_and_ratings <- merge(ratings,
                            movies,
                            by.x = "movieId",
                            by.y = "movieId", 
                            all.x = TRUE)

head(movies_and_ratings)

rsf  <- movies_and_ratings %>% 
        group_by(userId) %>% 
        summarise(Drama = mean(rating[grepl("Drama", genres)]),
                  Comedy = mean(rating[grepl("Comedy", genres)]))

ggplot(rsf) + geom_point(aes(x = Drama, y = Comedy))

rsf_2 <- rsf %>% drop_na() %>% select(-userId)

kmeans_model_1 <- kmeans(rsf_2, 2)

rsf_3 <- rsf_2 %>% 
         mutate(cluster =  as.factor(kmeans_model_1$cluster))

ggplot(rsf_3) + geom_point(aes(x = Drama, y = Comedy, color = cluster))

rsf_3 %>% 
  group_by(cluster) %>%
  summarise(min(Drama),
            mean(Drama),
            max(Drama),
            min(Comedy),
            mean(Comedy),
            max(Comedy))

kmeans_model_2 <- kmeans(rsf_2, 3)
# Put the estimate back
rsf_4 <- rsf_2 %>% 
          mutate(cluster =  as.factor(kmeans_model_2$cluster))
# Plot
ggplot(rsf_4) + geom_point(aes(x = Drama, y = Comedy, color = cluster))

silhouette_vec <- vector()
k <- vector()
distances <- dist(rsf_2)

for(i in 2:20) {
  
  model <- kmeans(rsf_2, i)
  ss <- silhouette(model$cluster, distances)
  silhouette_vec[i] <- mean(ss[,3])
  k[i] <- i
}

model_statistics <- data.frame(
  cluster_number = k[2:20],
  silhouette_score = silhouette_vec[2:20]
)

ggplot(model_statistics, aes(x = cluster_number, y = silhouette_score)) +
  geom_point() + geom_line()

# apply hierarchical clustering
hc <- hclust(distances) 
# plot the dendrogram
plot(hc)

hcluster_2 <- cutree(hc, 2)
rsf_5 <- rsf_2[,]%>%
          mutate(cluster =  as.factor(hcluster_2))

ggplot(rsf_5) + geom_point(aes(x = Drama, y = Comedy, color = cluster))


hcluster_3 <- cutree(hc, 3)
rsf_6 <- rsf_2[,]%>%
          mutate(cluster =  as.factor(hcluster_3))

ggplot(rsf_6) + geom_point(aes(x = Drama, y = Comedy, color = cluster))


