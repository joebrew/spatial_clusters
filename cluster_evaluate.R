## This function is going to sum the  
## distance between the points in a cluster and the that cluster's centroid
## from the clusters created by the fuction cluster_optimize

cluster_evaluate<-function(clusters){
        
        clusters <-clusters %>%
                group_by(cluster) %>%
                mutate(centroid_lat=mean(lat),
                       centroid_long=mean(long)) %>%
                ungroup
        # Add a distance colum
        clusters$distance <- spDists(x=cbind(clusters$long,clusters$lat),
                                     y=cbind(clusters$centroid_long,
                                             clusters$centroid_lat),
                                     diagonal = TRUE,
                                     longlat = TRUE)
        total_distance<-sum(clusters$distance)
        return(total_distance)
        
        
}