library(data.table)
library(ggplot2)

# Generate and plot a random ellipse.
x_coord=rnorm(500,1,0.4)
y_coord=rnorm(500,2,0.1)
direction=pi/4
DT=data.table(x=x_coord*cos(direction)+y_coord*sin(direction),
              y=-x_coord*sin(direction)+y_coord*cos(direction))

# Plot the data.
plot(DT,asp=1)

# First PCA.
PCA1 = prcomp(DT,center = T,scale. = F)

# Review the output.
summary(PCA1)

# Calculate the center of the data frame.
Mx=mean(DT$x)
My=mean(DT$y)

# Plot the data set.
plot(DT,asp=1)
# Plot the principle components using arrows with principal directions.
arrows(x0= c(Mx,Mx),y0= c(My,My),x1=Mx+c(PCA1$rotation[1,]),y1=My+c(PCA1$rotation[2,]),lwd=5,col="red")

# Load the data.
mnist_data=fread('mnist.csv')

# Plot observations.
par(mfrow=c(4,4),mar=c(2,2,2,2))
for (i in 1:16)
{
    ## Changing i-th row to matrix.
    mat=matrix(as.numeric(mnist_data[i,785:2,with=F]),nrow = 28,ncol=28,byrow = F)
    
    ## Inverting row order.
    mat=mat[nrow(mat):1,]
    
    ## Plot.
    image(mat,main=paste0('This is a ',mnist_data[i,1,with=F]))
}

# Identify the constant pixels.

# You would typically experiment with this code in your analysis phase, # but would remove the output, as it serves as input for the preprocessing in the next cell.

# This is not executed as it produces text output. 
# which(mnist_data[,sapply(.SD,FUN = function(x){min(x)==max(x)}),.SDcols=2:ncol(mnist_data)])

# Pre-processing: removes cells closer than three cells from the border.
mnist_data=mnist_data[,-(which((1:784)%%28<=3|(1:784)%%28>=25|1:784%/%28<=3|1:784%/%28>=25)+1),with=F]

# Create a training set and reduce using PCA.
PCA1=prcomp(mnist_data[,(2:ncol(mnist_data)),with=F],
            center = T, scale. = F)

summary(PCA1)

# Plot cumulative impact of variance.
plot(cumsum(PCA1$sdev^2)/sum(PCA1$sdev^2)*100, type="l",
     main='Cumulative impact of variance',xlab="Number of PC", ylab="Proportion of explained variance")

# Project the reduced components on the original data.
projected=scale(mnist_data[,(2:ncol(mnist_data)),with=F], PCA1$center, PCA1$scale) %*% PCA1$rotation

# Review the summary.
projectedsummary <- projected[seq(0,100,5),]
projectedsummary

# Keep 2 main dimensions.
n_dim=2

# Project data using a specified number of principal components.
coord_x=data.table(mnist_data$label,projected[,1:n_dim]%*%t(PCA1$rotation)[1:n_dim,])

# Plot.
par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:16)
{
    mat=matrix(as.numeric(coord_x[i,441:1,with=F]),
               nrow = 21,
               ncol=21,
               byrow = F)
    mat=mat[nrow(mat):1,]
    image(mat)
}

# Keep 10 main dimensions.
n_dim=10

# Project data using a specified number of principal components.
coord_x=data.table(mnist_data$label,projected[,1:n_dim]%*%t(PCA1$rotation)[1:n_dim,])

# Plot.
par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:16)
{
    mat=matrix(as.numeric(coord_x[i,441:1,with=F]),
               nrow = 21,
               ncol=21,
               byrow = F)
    mat=mat[nrow(mat):1,]
    image(mat)
}

# Keep 40 main dimensions.
n_dim=40

# Project data using a specified number of principal components.
coord_x=data.table(mnist_data$label,projected[,1:n_dim]%*%t(PCA1$rotation)[1:n_dim,])

# Plot.
par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:16)
{
    mat=matrix(as.numeric(coord_x[i,441:1,with=F]),
               nrow = 21,
               ncol=21,
               byrow = F)
    mat=mat[nrow(mat):1,]
    image(mat)
}
