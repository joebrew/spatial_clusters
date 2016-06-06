# This script needs work
library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)

# Read in results generated in compare_methods.R
results <- read_csv('~/Desktop/results.csv')

# Make a column combining start and rest
results$strategy <- paste0(results$start,'-', results$rest)

# Get the BEST previous run for each strategy
results$best <- NA
for (i in 1:nrow(results)){
  sub_data <- results[1:i,]
  results$best[i] <-
    min(sub_data$performance[sub_data$strategy ==
                             sub_data$strategy[i]])
}

# Add rows for those which don't have multiple runs
more_rows <- 
  results %>%
  filter(start != 'random' &
           rest != 'random') %>%
  # Artifically set to 1000
  mutate(n = 1000)
results <- rbind(results, more_rows)


# Make a plot
ggplot(data = results %>%
         # get rid of the garbage
         filter(!strategy %in% c('far-random', 
                'close-random')),
       aes(x = n,
           y = best,
           group = strategy,
           color = strategy)) +
  geom_line(size = 2)


# Reshuffle to try to estimate the curve better
strategies <- sort(unique(results$strategy))
results_df <- data.frame(n = 1:1000)
for(j in 1:length(strategies)){
  results_df[,j+1] <- rep(NA, 1000)
  names(results_df)[j+1] <- strategies[j]
}

for (j in 1:length(strategies)){
  message(paste0('Strategy: ', strategies[j]))
  # Define the strategy
  strategy <- strategies[j]
  # Get the original results for that strategy
  original <- results$performance[results$strategy == strategy]
  # If only length of 2 or less (ie, no randomness)
  # then just make a value like that
  if(length(original) <= 2){
    results_df[,strategies[j]] <- original[1]
  } else {
    # Reorder 1000 times and get the average
    results_matrix <- matrix(rep(NA, 1000 * 1000),
                             ncol = 1000,
                             nrow = 1000)
    for (i in 1:1000){
      new_original <- sample(original, length(original))
      results_matrix[i,] <- cummin(new_original)
    }
    # Get the average for each round
    results_df[,strategies[j]] <- apply(results_matrix, 2, mean)
  }
}

# gather the data
results_df <- tidyr::gather(results_df, key, value, `close-close`:`random-random`)

cols <- brewer.pal(n = length(unique(results_df$key)),
                   'Set1')

ggplot(data = results_df,
       aes(x = n, 
           y = value,
           group = key,
           color = key)) +
  geom_line(size = 1) +
  theme_fivethirtyeight() +
  scale_color_manual(name = 'Strategy',
                     values = cols) +
  ylim(0, 5000)
