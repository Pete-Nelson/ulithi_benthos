# Peter Nelson
# Institute for Marine Science
# University of California Santa Cruz
# created: 14 March 2023
# purpose: cluster analysis with Ulithi benthic data
# requires: data from 'data manipulation.R'
# modified: 25 March 2023

library(vegan)
library(cluster)

# cluster analysis ----
# based on https://www.davidzeleny.net/anadat-r/doku.php/en:hier-agglom_examples

# coral morphs ----
## hclust function -----
s_coral # dataframe of summed coral morphs from 'data manipulation.R'
dis <- vegdist(sqrt(s_coral[,-1]), method = "bray") # updated approach

sites <- s_coral$sitecode

cluster.single <- hclust (d = dis, method = 'single')
cluster.complete <- hclust (dis, 'complete')
cluster.average <- hclust (dis, 'average')

par(mfrow = c(1,3)) # will draw all dendrograms into one figure

plot(cluster.single, main = NULL, sub = 'Single linkage', labels = sites, xlab = NULL)
plot(cluster.complete, main = "Ulithi Coral Communities", sub = 'Complete linkage', labels = sites, xlab = NULL)
plot(cluster.average, main = NULL, sub = 'Average linkage', labels = sites, xlab = NULL)

par(mfrow = c(1, 1))

plot(cluster.average, main = 'Average linkage', labels = sites)
rect.hclust(cluster.average, k = 4, border = 'blue')

clusters <- cutree(cluster.average, k = 6)
clusters

## agnes function ----
# Note: In function agnes the setting beta parameter is, however, not so simple--you need to set up one to four parameters (see ?agnes for details). The simplest options is to assign to the argument par.method only one value, so called alpha, while it applies: beta = 1 - 2*alpha, the value for alpha can be thus calculated as alpha = (1-beta)/2. If, for example, I want to calculate beta flexible method with beta = -0.25 (which is said to optimally represent distances among samples), than alpha = (1-(-0.25))/2 = 1.25/2 = 0.625.

cluster.flexible <- agnes(x = dis, method = "flexible", par.method = 0.625)

cluster.flexible.hclust <- as.hclust (cluster.flexible)
plot(cluster.flexible.hclust, labels = sites)

## Ward's algorithm -----
s_coral_t <- log1p(s_coral[,-1]) # log-transform "spp" data
dis2 <- vegdist(s_coral_t, method = "bray")
print(dis2, diag = TRUE)

# Ward's requires distances to metric; Bray-Curtis is not metric, so use the square-root transformation of the BC matrix
clust <- agnes(sqrt(dis2), method = "ward") 
plot(clust, which.plot = 2, labels = sites)

# To use functions requiring objects of class hclust, convert the object 'clust' by function as.hclust and then plot it:
clust.hclust <- as.hclust(clust)
plot(clust.hclust, labels = sites) # looks identical to previous plot but w different labels

# To cut the tree into a few clusters, use the function cutree again...
groups <- cutree(clust, k = 5) # pretty haphazard choice!
groups

# To know which of these samples belong to which cluster...
group.order <- groups[clust.hclust$order]
group.order

# Now we're going to apply the function unique to get a vector of samples in the order they're displayed on the ordination diagram and later use it to color the groups in the dendrogram.
group.in.cluster <- unique(group.order)
group.in.cluster

plot(clust.hclust, 
     labels = sites, 
     main = "Ulithi Coral Communities", sub = "coral morphotype frequency",
     xlab = "Agglomerative Coefficient = 0.76")
rect.hclust(clust.hclust, border = group.in.cluster, k = 5)
legend('topleft', legend = paste('Cluster', 1:5), pch = 22, col = 1:5, bty = 'n')

## needs work ----
# Link (map) these samples to islands from which they came

## nmds w Bray-Curtis distances ----
# The result of cluster analysis can be displayed also on the ordination diagram of unconstrained ordination, to assess how well are individual groups compositionally separated from each other. Here we will use two contrasting unconstrained ordination analyses: one which is based on the same distance measure as is the cluster anlaysis (NMDS with Bray-Curtis distance), one which is not (DCA, whose mother method, CA, is based on chi-square distance). The point of this selection (NMDS vs DCA) is to show that to evaluate whether cluster analysis results into well defined clusters, one needs to use the ordination method which is based on the same distance metric as the cluster analysis (Bray-Curtis distance in this example).

nmds <- metaMDS(vegdist(s_coral_t)) # not converging!
par(mfrow = c(1,2)) # I want to plot both plots into one figure, with two panels in one row
ordiplot(nmds, type = 'n') # spp scores not available...why?
points(nmds, pch = groups, col = groups)
legend('topright', pch = 1:5, col = 1:5, legend = 1:5, bty = 'n')

## DCA ordination ----
# (uses chi-square distance)
dca <- decorana(s_coral_t)
ordiplot(dca, type = 'n', display = 'si')
points(dca, pch = groups, col = groups)
legend('topright', pch = 1:5, col = 1:5, legend = 1:5, bty = 'n')
par(mfrow = c(1, 1))

# SCRATCH ####################################
# all cover types ----
## hclust function -----
cover # dataframe from 'data manipulation.R'
# transformed as sqrt(1+x) to handle empty rows
dis <- vegdist(sqrt(1 + cover[,8:12]), method = "bray")
cluster.single <- hclust (d = dis, method = 'single')
cluster.complete <- hclust (dis, 'complete')
cluster.average <- hclust (dis, 'average')

par(mfrow = c(1,3)) # will draw all dendrograms into one figure

plot(cluster.single, main = 'Single linkage')
plot(cluster.complete, main = 'Complete linkage')
plot(cluster.average, main = 'Average linkage')

par(mfrow = c(1, 1))
plot(cluster.average, main = 'Average linkage')
rect.hclust (cluster.average, k = 4)
rect.hclust (cluster.average, k = 6, border = 'blue') # argument border specifies the colour of the rectangle

clusters <- cutree(cluster.average, k = 6)
clusters

## agnes function ----
cluster.flexible <- agnes(x = dis, method = "flexible", par.method = 0.625)

cluster.flexible.hclust <- as.hclust (cluster.flexible)
plot (cluster.flexible.hclust)

## Ward's algorithm -----
tdat <- log1p(1 + cover[,8:12]) # log-transform "spp + 1" data
dis2 <- vegdist(tdat, method = "bray")
print(dis2, diag = TRUE)

clust <- agnes(sqrt(dis2), method = "ward") 
plot(clust, which.plot = 2)

# To use functions requiring objects of class hclust, convert the object 'clust' by function as.hclust and then plot it:
clust.hclust <- as.hclust(clust)
plot(clust.hclust) # looks identical to previous plot but w different labels

# To cut the tree into a few clusters, use the function cutree again...
groups <- cutree(clust, k = 5) # feels like a good number
groups

# To know which of these samples belong to which cluster...
group.order <- groups[clust.hclust$order]
group.order

# Now we're going to apply the function unique to get a vector of samples in the order they're displayed on the ordination diagram and later use it to color the groups in the dendrogram.
group.in.cluster <- unique(group.order)
group.in.cluster

plot(clust.hclust)
rect.hclust(clust.hclust, border = group.in.cluster, k = 5)
legend('topleft', legend = paste('Cluster', 1:5), pch = 22, col = 1:5, bty = 'n')

## nmds w Bray-Curtis distances ----
nmds <- metaMDS(vegdist(tdat)) # not converging!
par(mfrow = c(1,2)) # I want to plot both plots into one figure, with two panels in one row
ordiplot(nmds, type = 'n') # spp scores not available...why?
points(nmds, pch = groups, col = groups)
legend('topright', pch = 1:5, col = 1:5, legend = 1:5, bty = 'n')

## DCA ordination ----
# (uses chi-square distance)
dca <- decorana(tdat)
ordiplot(dca, type = 'n', display = 'si')
points(dca, pch = groups, col = groups)
legend('topright', pch = 1:5, col = 1:5, legend = 1:5, bty = 'n')
par(mfrow = c(1, 1))
