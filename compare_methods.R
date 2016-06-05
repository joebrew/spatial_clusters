# Prepare data
source('prepare_data.R')

results_list <- list()

counter <- 1

# How many times it has to be run
# (how_many * 3) + 4

how_many <- 1000
total <- (how_many * 5) +4

for (start in c('far', 'close', 'random')){
        for(rest in c('far', 'close', 'random')){
                
                # Figure out how many times to go
                # Create a "real_times" which is how many times it needs to be run
                if(start != 'random' &
                   rest != 'random'){
                        real_times <- 1
                } else {
                        real_times <- how_many
                }
                
                for(n in 1:real_times){
                        message(paste0('starting counter: ', counter, '\n'))
                        start_time <- Sys.time()
                        # run the function once
                        x = cluster_optimize(cluster_size = 12,
                                             sleep = 0,
                                             plot_map = FALSE,
                                             locations = locations,
                                             shp=mag, 
                                             start=start, 
                                             rest=rest,
                                             messaging = FALSE)

                        # Create dataframe to store results
                        results_df <- data.frame(performance =  
                                                   cluster_evaluate(clusters = x),
                                                 start = start,
                                                 rest = rest,
                                                 n = n)
                        
                        # Stick the results into the list
                        results_list[[counter]] <- results_df
                        end_time <- Sys.time()
                        total_time <- end_time - start_time
                        # Message
                        message(paste0('just finished: ', counter, ' of ',total, '\n',
                                       'start = ', start, '\n',
                                       'rest = ', rest, '\n',
                                       'n = ', n, '\n', 
                                       'counter = ', counter, '\n',
                                       'that took = ', total_time, ' seconds\n',
                                       '\n---\n'))
                        counter <- counter + 1
                        
                }
                
        }
}

# Bind results_list
results <- do.call('rbind', results_list)
head(results)
x <-
results %>%
        group_by(start, rest) %>%
        summarise(avg_performance = mean(performance)) %>%
        ungroup %>%
        arrange(avg_performance) %>%
        mutate(strategy = paste0(start, '-', rest))
x$strategy <- factor(x$strategy, levels = x$strategy)
ggplot(data = x,
       aes(x = strategy,
           y = avg_performance)) +
        geom_bar(stat = 'identity')
