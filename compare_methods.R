library(ggplot2)
library(readr)

## Prepare data
source('prepare_africa_data.R')
# source('prepare_magude_data.R')
africa_df <- prepare_africa_data()
# x = prepare_magude_data()

# Source our functions for running and evaluating the algorithm
source('cluster_optimize.R')
source('cluster_evaluate.R')

results_list <- list()

counter <- 1

# How many times it has to be run
countries <- sort(unique(africa_df$COUNTRY))
how_many <- 10
total <- ((how_many * 5 ) + 4) * length(countries)
for (country in countries){
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
      
      # Subset the data for only this country
      this_country <- africa_df[africa_df@data$COUNTRY == country,]
      
      for(n in 1:real_times){
        message(paste0('starting counter: ', counter, '\n'))
        start_time <- Sys.time()
        # run the function once
        x = cluster_optimize(cluster_size = 8,
                             sleep = 0,
                             plot_map = FALSE,
                             locations = this_country,
                             start=start, 
                             rest=rest,
                             messaging = FALSE)
        
        # Create dataframe to store results
        results_df <- data.frame(performance =  
                                   cluster_evaluate(clusters = x),
                                 start = start,
                                 rest = rest,
                                 n = n,
                                 country = country)
        
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
}

# Bind results_list
results <- do.call('rbind', results_list)
x <-
  results %>%
  group_by(start, rest, country) %>%
  summarise(avg_performance = mean(performance)) %>%
  ungroup %>%
  group_by(country) %>%
  mutate(adj_performance = avg_performance / mean(avg_performance)) %>%
  ungroup %>%
  arrange(adj_performance) %>%
  mutate(strategy = as.character(paste0(start, '-', rest)))

x$strategy <- factor(x$strategy)

# Get rid of the garbage strategies
x <- x %>%
  filter(!strategy %in% c('close-random',
                          'far-random'))

# Plot
ggplot(data = x,
       aes(x = strategy,
           y = adj_performance)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~country) +
  geom_hline(yintercept = 1, color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Write a csv of results
write_csv(results, '~/Desktop/results.csv')