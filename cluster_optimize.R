#' Optimization of cluster formation
#' 
#' Use the brute force method to assign households to clusters in a way that minimizes inter-cluster distance.
#' @param times The number of times to repeat the cluster assignation
#' @param cluster_size The size of each cluster
#' @param plot_map Whether to plot a map after each cluster formation (slows down operations significantly)
#' @param locations a SpatialPointsDataFrame containing 3 columns: id, lat, and lon
#' @param sleep How many seconds to sleep between plots
#' @param best_only Whether to only return the best simulation
#' @return A dataframe of size \code{times} times the number of points in \code{locations}, with columns indicating each \code{simulation_number}
#' @export


# Define our algorithm
cluster_optimize <- function(times = 2,
                             cluster_size = 10,
                             plot_map = FALSE,
                             sleep = 0.3,
                             best_only = TRUE,
                             locations){
  
  require(dplyr)
  
  # Create a placeholder list to store results
  results_list <- list()
  
  # Repeat [times] times the search
  for (time in 1:times){
    
    message(paste0('time ', time))
    
    # Create a fresh copy of locations
    locations_fresh <- locations
    # Specify that none of the points have yet been selected
    locations_fresh$selected <- FALSE
    # Create a column for clusters
    locations_fresh$cluster <- NA
    # Create column for simulations
    locations_fresh$simulation_number <- time
    # Create column for indication of whether full sized cluster or not
    locations_fresh$complete_cluster <- NA
    
    # Pick a start point
    # (the point which is furthest from all other points)
    possibles <- spDists(x = locations_fresh[!locations_fresh$selected,],
                         longlat = TRUE)
    start_index <- locations_fresh$id[!locations_fresh$selected][which.max(rowSums(possibles))][1]  
    
    # Start the clster counter 
    cluster <- 1
    
    # Go until all the points are filled
    while(length(which(!locations_fresh$selected)) > 0){
      message(paste0('making cluster number ', cluster,'\n',
                     length(which(!locations_fresh$selected)),
                     ' points remaining'))
      # Use the start index to get a start point
      start_point <- locations_fresh[start_index,]
      # Remove that start point from the list of eligibles
      locations_fresh$selected[start_index] <- TRUE
      # Assign the cluster to the start point
      locations_fresh$cluster[start_index] <- TRUE
      
      # Get the distance of all remaining points from the start_point
      all_distances <- spDistsN1(pts = locations, 
                                 pt = start_point,
                                 longlat = TRUE)
      all_distances <- data.frame(index = 1:nrow(locations),
                                  distance = all_distances)
      # Remove those rows which are ineligible (already selected/start_point)
      all_distances <- 
        all_distances[! all_distances$index %in% which(locations_fresh$selected),]
      
      # Order by distance
      all_distances <- all_distances[order(all_distances$distance),]
      
      # Get the cluster_size nearest points
      # (or fewer, if not enough eligible points still remain)
      incomplete_cluster <- (nrow(all_distances) + 1) < cluster_size
      if(incomplete_cluster){
        # Just get whatever is left
        nearest <- all_distances
      } else {
        nearest <- all_distances[1:(cluster_size - 1),]
      }
      
      # Mark those nearest points as part of the same cluster
      locations_fresh$cluster[nearest$index] <- cluster
      # And mark them as selected
      locations_fresh$selected[nearest$index] <- TRUE
      # Mark if it's a full size cluster or not
      locations_fresh$complete_cluster[nearest$index] <- !incomplete_cluster
      
      # Get the start_point for the next round 
      # (the point which is furthest to all the others)
      possibles <- spDists(x = locations_fresh[!locations_fresh$selected,],
                           longlat = TRUE)
      start_index <- locations_fresh$id[!locations_fresh$selected][which.max(rowSums(possibles))][1]  
      
      # Move the cluster counter up
      cluster <- cluster + 1
      
      # Plot if necessary
      if(plot_map){
        colors <- ifelse(locations_fresh$selected, 'red', 'grey')
        plot(man)
        points(locations_fresh, col = colors, pch = 3)
        points(locations_fresh[nearest$index,], col = 'blue', pch = 1)
        legend('topleft',
               legend = c('This cluster',
                          'Already selected',
                          'Not selected yet'),
               pch = c(1, 3, 3),
               col = c('blue', 'red', 'grey'),
               border = FALSE,
               bty = 'n')
        title(main = paste0('Simulation number ', time, '\n',
                            'Cluster number ', cluster))
        Sys.sleep(sleep)
      }
    } # all locations have now been selected
    
    # Put results into the list
    results_list[[time]] <- data.frame(locations_fresh)
  }
  # Combine all of the results of the results list into one
  results <- do.call('rbind', results_list)
  
  # Get best only if applicable
  if(best_only){
    # Get a dataframe for aggregate results
    results_agg <- results %>%
      group_by(simulation_number, cluster) %>%
      summarise(avg_distance = mean(spDists(x = cbind(x, y),
                                            longlat = TRUE))) %>%
      # group just by simulation number and get average
      ungroup %>%
      group_by(simulation_number) %>%
      summarise(avg_distance = mean(avg_distance)) %>%
      # Keep only the best
      filter(avg_distance == min(avg_distance))
    # Keep only one row if tie
    results_agg <- results_agg[1,]
    
    # Overwrite results with just the best one
    results <- results %>%
      filter(simulation_number == results_agg$simulation_number)
  } 
  
  # Return the results
  return(results)
}