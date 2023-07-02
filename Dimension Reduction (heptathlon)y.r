library("tidyverse") 
library("HSAUR") 
library("GGally") 
library("factoextra") 

data(heptathlon)

heptathlon

options(repr.plot.width=14, repr.plot.height=10)

gather(heptathlon, "Event", "Event_scores")  %>% 
    ggplot(aes(x=Event, y=Event_scores)) + 
    geom_boxplot() +
    facet_wrap(~Event, scale="free")


options(repr.plot.width=16, repr.plot.height=10)

# Correlation plot 
my_fn <- function(data, mapping, ...){ 
    p <- ggplot(data = data, mapping = mapping) +
         geom_point() + 
         geom_smooth(formula = 'y ~ x', method=loess, fill="Black", color="red", ...)
    p
} 
ggpairs(heptathlon, lower = list(continuous = my_fn ))


# Create a training set and reduce using PCA
# YOUR CODE HERE
heptathlon_pca=prcomp(heptathlon[,-c(8)], scale = TRUE, center = TRUE)

# Review the output
# YOUR CODE HERE
summary(heptathlon_pca)

# Plot the cumulative impact of variance
# YOUR CODE HERE
plot(cumsum(heptathlon_pca$sdev^2)/sum(heptathlon_pca$sdev^2)*100,
     type="l",
     main='Cumulative impact of variance',
     xlab="Number of PC",
     ylab="Proportion of explained variance")

# Loadings on first principal component
heptathlon_pca$rotation[,1]

# Biplot 
fviz_pca_biplot(heptathlon_pca, 
                repel = TRUE,
                col.var = "red",
                title ="Heptathlon PCA Biplot")


# Correlation between score and first PC
cor(heptathlon$score, heptathlon_pca$x[,1])

# Scatterplot
cbind(score = heptathlon$score, PC1 = heptathlon_pca$x[,1])  %>% 
    as.data.frame()  %>% 
        ggplot(aes(x=score, y=PC1)) +
            geom_point() + 
            geom_text(label=names(heptathlon_pca$x[,1])) +
            labs(title="Score vs First Principal Component")+ 
            theme_minimal()








