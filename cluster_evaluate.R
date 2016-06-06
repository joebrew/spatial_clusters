## This function is going to sum the  
## distance between the points in a cluster and the that cluster's centroid
## from the clusters created by the fuction cluster_optimize

cluster_evaluate<-function(clusters){
        
        clusters <-clusters %>%
                group_by(cluster) %>%
                mutate(centroid_lat=mean(lat),
                       centroid_lon=mean(lon)) %>%
                ungroup
        # Add a distance colum
        clusters$distance <- spDists(x=cbind(clusters$lon,clusters$lat),
                                     y=cbind(clusters$centroid_lon,
                                             clusters$centroid_lat),
                                     diagonal = TRUE,
                                     longlat = TRUE)
        total_distance<-sum(clusters$distance)
        return(total_distance)
        
        
}