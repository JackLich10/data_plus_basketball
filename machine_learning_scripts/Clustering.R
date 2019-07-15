#Read in CSV
to_be_clustered <- read_csv("data/new_shots.csv")

#Threshold for Velocity
to_be_clustered <- to_be_clustered %>%
  filter(velocity <= 30)

to_be_clustered <- to_be_clustered %>%
  mutate(value = case_when(
    shot_dist > 20.75 ~ 3,
    shot_dist <= 20.75 ~ 2))

#Remove Shot Number and Result
clustered_new <- to_be_clustered
clustered_new <- clustered_new %>%
  dplyr::select(-X1)

to_be_clustered <- to_be_clustered %>%
  dplyr::select(-X1)

#Normalization (subtracting mean, dividing by standard deviation) (must be quantitative data)
means <- apply(clustered_new[, 3:16], 2, mean)
stdevs <- apply(clustered_new[, 3:16], 2, sd)
clustered_new[, 3:16] <- scale(clustered_new[, 3:16], means, stdevs)

standardized <- clustered_new[, 3:17]

z <- standardized %>%
  dplyr::select(shot_dist, angle_closest_def, offense_hull, angle_second_def)

#Calculating Euclidean distance
distance <- dist(z)

#Cluster Dendrogram with Complete Linkage
hierarchical_clust_c <- hclust(distance)
plot(hierarchical_clust_c, hang = 1)

rect.hclust(hierarchical_clust_c, k = 2, border = "grey")
rect.hclust(hierarchical_clust_c, k = 3, border = "blue")

#Cluster Means (if small variation between clusters in a variable - it has a small role in determining clusters)

#Silhouette Plot
library(cluster)
plot(silhouette(cutree(hierarchical_clust_c, 5), distance))

#Scree Plot
within_group_sum_squares <- (nrow(clustered_new) - 1) * sum(apply(clustered_new, 2, var))
for (i in 2:20) {
  within_group_sum_squares[i] <- sum(kmeans(clustered_new, centers = i)$withinss)
}
plot(1:20, within_group_sum_squares, type = "b", xlab = "Number of Clusters", ylab = "Within Group SS")

#K-Means Clustering
k_means_c <- kmeans(z, 6)
plot(z, col = k_means_c$cluster)
k_means_c

clusters <- cutree(hierarchical_clust_c, 5)
plot(z, col = clusters)


clustered_new <- clustered_new %>%
  dplyr::mutate(cluster = k_means_c$cluster)

to_be_clustered <- to_be_clustered %>%
  dplyr::mutate(cluster = k_means_c$cluster)

a %>%
  dplyr::count(cluster)


to_be_clustered %>%
  mutate(cluster = clusters) %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(dist_ten_sec = mean(distance_ten_seconds),
            dist_game = mean(distance_game),
            vel = mean(velocity),
            dist_def = mean(distance_closest_def),
            angle_def = mean(angle_closest_def),
            dist_shot = mean(shot_dist),
            angle_shot = mean(shot_angle),
            catch_shoot = mean(catch_shoot),
            off_hull = mean(offense_hull),
            def_hull = mean(defense_hull))

write_csv(clustered_new, path = "data/clustered_shots_standardized.csv")
write_csv(to_be_clustered, path = "data/clustered_shots.csv")
