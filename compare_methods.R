library(ggplot2)
library(readr)
library(dplyr)
library(ggthemes)
# Theme for plotting
source('theme.R')

## Prepare data
source('prepare_africa_data.R')
africa_df <- prepare_africa_data()

# Source our functions for running and evaluating the algorithm
source('cluster_optimize.R')
source('cluster_evaluate.R')

# How many iterations per strategy per country
how_many <- 2

# How many times it has to be run
countries <- as.character(sort(unique(africa_df$COUNTRY)))
total <- ((how_many * 5 ) + 4) * length(countries)


# Create a results dataframe for placing the 
# results of each iteration
nas <- rep(NA, total)
results <- data.frame(performance =  as.numeric(nas),
                      start = as.character(nas),
                      rest = as.character(nas),
                      n = as.numeric(nas),
                      country = as.character(nas),
                      stringsAsFactors = FALSE)

# Start a counter so as to keep track of progress
counter <- 1

# Begin loop of testing each strategy
# how_many times for each map
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
                                 country = country,
                                 stringsAsFactors = FALSE)
        
        # Stick the results into the list
        # results_list[[counter]] <- results_df
        results[counter,] <- results_df
        end_time <- Sys.time()
        total_time <- end_time - start_time
        # Message
        message(paste0('just finished: ', counter, ' of ',total, '\n',
                       'country = ', country, '\n',
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

save.image('evaluation/results_', how_many, '_simulations.RData')

# Add strategy to results
results$strategy <- paste0(results$start, '-', results$rest)

# Get adjusted performance
results <- 
  results %>% 
  group_by(country) %>%
  mutate(adj_performance = performance / mean(performance)) %>%
  ungroup

# Get everything grouped together
z <- results %>%
  group_by(strategy) %>%
  summarise(avg_performance = mean(performance)) %>%
  ungroup %>%
  arrange(avg_performance)


# Apply the levels of z (the ordered average performance) to results
results$strategy <- factor(results$strategy, levels = z$strategy)
  
# Get a breakdown of strategy by country
by_country <- results %>%
  group_by(strategy, country) %>%
  summarise(avg_performance = mean(performance)) %>%
  ungroup %>%
  arrange(avg_performance) %>%
  # get the average performance by country
  group_by(country) %>%
  mutate(adj_performance = avg_performance / mean(avg_performance))
by_country$strategy <- factor(by_country$strategy, levels = z$strategy)


# Make a violin plot of all results
pdf(file = 'evaluation/aggregate_results.pdf')
ggplot(data = results,
       aes(x = strategy,
           y = adj_performance)) +
  geom_jitter(alpha = 0.5, size = 0.3) +
  geom_violin(alpha = 0.6, fill = 'darkgreen') +
  theme_brew(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  xlab('Strategy') +
  ylab('Performance (1 is average, lower is better)')  +
  geom_hline(yintercept = 1, color = 'red', alpha = 0.5) +
  labs(title = 'Performance breakdown',
       subtitle = '1,000 simulations for each strategy for each 56 African countries')
dev.off()

pdf(file = 'evaluation/all_countries.pdf')
# Do the same for each country
for (i in 1:length(countries)){
  this_country <- as.character(countries[i])
  
  g <- ggplot(data = results %>% filter(country == this_country),
         aes(x = strategy,
             y = adj_performance)) +
    geom_jitter(alpha = 0.5, size = 0.6) +
    geom_violin(alpha = 0.6, fill = 'darkgreen') +
    theme_brew(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
    xlab('Strategy') +
    ylab('Performance (1 is average, lower is better)')  +
    geom_hline(yintercept = 1, color = 'black', alpha = 0.5, lty = 3) +
    labs(title = this_country,
         subtitle = '1000 simulations for all strategies involving randomness') +
    geom_line(data = by_country %>%
               filter(country == this_country),
             aes(x = strategy, y = adj_performance, group = 1),
             # stat = 'identity',
             alpha = 0.5) +
    geom_point(data = by_country %>%
                filter(country == this_country),
              aes(x = strategy, y = adj_performance, group = 1),
              # stat = 'identity',
              alpha = 0.6, 
              col = 'red')
  print(g)
    
}
dev.off()
# # Prepare for aggregated plotting
# x <-
#   results %>%
#   group_by(start, rest, country) %>%
#   summarise(avg_performance = mean(performance)) %>%
#   ungroup %>%
#   group_by(country) %>%
#   mutate(adj_performance = avg_performance / mean(avg_performance)) %>%
#   ungroup %>%
#   arrange(adj_performance) %>%
#   mutate(strategy = as.character(paste0(start, '-', rest)))
# 
# x$strategy <- factor(x$strategy)
# 
# # # Get rid of the garbage strategies
# # x <- x %>%
# #   filter(!strategy %in% c('close-random',
# #                           'far-random'))
# 
# # Get all countries aggregated together
# y <- x %>%
#   group_by(strategy) %>%
#   summarise(adj_performance = mean(adj_performance)) %>%
#   ungroup %>%
#   arrange(adj_performance)
# 
# # Order the factors by overall
# x$strategy <- factor(x$strategy, levels = y$strategy)
# 
# # Plot all countries
# ggplot(data = x,
#        aes(x = strategy,
#            y = adj_performance)) +
#   geom_bar(stat = 'identity') +
#   facet_wrap(~country) +
#   geom_hline(yintercept = 1, color = 'red') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ggplot(data = x,
#        aes(x = strategy,
#            y = adj_performance)) +
#   geom_jitter(alpha = 0.6) +
#   geom_violin(alpha = 0.6) +
#   theme_brew(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
#   xlab('Strategy') +
#   ylab('Performance (1 is average, lower is better)')  +
#   geom_hline(yintercept = 1, color = 'red', alpha = 0.5)