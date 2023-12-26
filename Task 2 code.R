library(tidyverse)
library(factoextra)

df <- read_csv('D:/Internships/Prodigy/Task 2 (Clustering)/Data2.csv',
               show_col_types = F)


# Looking at the data:
glimpse(df)

# checking for NA values in the data:
df %>% is.na() %>% sum()

# Summary measurements:
summary(df)

#========================================================================
# Univariate analysis:

df %>% count(Gender) %>% 
  ggplot(aes(x = Gender, y = n)) + 
  geom_col(fill = 'violet', colour = 'blue',
           width = 0.3) + theme_minimal() +
  labs(y = 'Frequency\n', x = '\nGender')


hist_plot <- function(text){
  df %>% ggplot(aes({{text}})) + geom_histogram(fill = 'lightyellow',
                                           colour = 'red',
                    aes(y = after_stat(density))) +
    geom_density(colour = 'blue', lty = 2, lwd = 1) +
    theme_minimal() + labs(y = 'Frequency density\n')
}

attach(df)
hist_plot(Age)
hist_plot(`Annual Income (k$)`)
hist_plot(`Spending Score (1-100)`)




# Bivariate analysis:

cont_scat_plot <- function(var1, var2){
  df %>% ggplot(aes(x = {{var1}}, y = {{var2}})) +
    geom_point(colour = 'red') + 
    geom_density_2d(aes(color = ..level..), bins = 15) +
    scale_color_viridis_c() + 
    theme_minimal()
}

cont_scat_plot(Age, `Annual Income (k$)`)
cont_scat_plot(Age, `Spending Score (1-100)`)
cont_scat_plot(`Spending Score (1-100)`, `Annual Income (k$)`)


#==============================================================
## Clustering

df %>% select(-c(CustomerID, Gender)) %>% 
  scale() %>% as_tibble() -> df_active


centers <- 1:10
r <- 0

for(i in 1:length(centers)){
  set.seed(42)
  k <- kmeans(df_active, centers[i])
  r[i] <- k$tot.withinss/k$totss
}

ggplot(NULL, aes(x = centers, y = r)) +
  geom_line(colour = 'red') + geom_point() +
  geom_vline(xintercept = 5, lty = 2, colour = 'blue') +
  theme_minimal() +
  labs(x = '\nNumber of centroids', y = 'SS(within)/SS(total)\n') +
  scale_x_continuous(n.breaks = 10)


set.seed(42)
K <- kmeans(df_active, centers = 6)
df %>% mutate('Clusters' = K$cluster) -> df


# visualizing clusters:
fviz_cluster(K, data = df_active, 
             pointsize = 1, labelsize = 8,
             ellipse.alpha = 0.1) +
  theme_minimal()







