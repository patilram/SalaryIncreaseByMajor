##Salary Increase By Major dataset analysis
##Ramesh K Patil

# Load or install relevant packages required for our analysis
# Note: this process could take a couple of minutes
#Make sure we get all required packages else get those installed
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

#-------------------------------------------------------------------------------------------------------------------------

#Web Scraping
# We will be using Undergraduate major that pay you back dataset out of Salary Increase By Major Set from The Wall Street Journal

url <- "http://online.wsj.com/public/resources/documents/info-Degrees_that_Pay_you_Back-sort.html?mod=article_inline#top"

pre_data <- read_html(url)

nodes <- pre_data %>% html_nodes('table')

# locate table of interest from nodes "xml_nodeset"
majortab <- nodes[[7]]

# create a raw data we scraped from WTJ page
raw_dataset <- html_table(majortab)

# Take a Look at few rows of this data
head(raw_dataset)

# Take a look at summary of this data
summary(raw_dataset)

#Remove the unwanted variables from the environment
rm(pre_data, nodes, majortab, url)

#-------------------------------------------------------------------------------------------------------------------------

#Data Wrangling

#As we see while exploring the dataset that column names are marked as X1 to X8 and not much informative
#Row one of dataset is contains info of stored data in respective column

colnames(raw_dataset) <- c("Undergraduate_Major", "Starting_Median_Salary", "Mid_Career_Median_Salary", "Career_Percent_Growth", "Percentile_10", "Percentile_25", "Percentile_75", "Percentile_90" )

raw_dataset <- raw_dataset[-1,]

rownames(raw_dataset) <- 1:nrow(raw_dataset)

# Since values are in currency format, we will clean it and also converting Career Percent growth to floating point
degrees <- raw_dataset %>% 
  mutate_at(vars(Starting_Median_Salary: Percentile_90), function(x) as.numeric(gsub('[\\$,]',"",x))) %>%
  mutate(Career_Percent_Growth = Career_Percent_Growth / 100)

#Take a look a look at modified data
head(degrees)

# Remove the not needed object
rm(raw_dataset)

#-------------------------------------------------------------------------------------------------------------------------

#Data Analysis:

# Elbow method
# select and scale the relevant features and store as k_means_data

k_means_data <- degrees %>%
  select(Starting_Median_Salary, Mid_Career_Median_Salary, Percentile_10, Percentile_90) %>% 
  scale()

# we will use the fviz_nbclust function with selected data and method "wss"
# wss = total within-cluster sume of square

elbow_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "wss")

elbow_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of clusters k') + 
  ylab('Total Within Sum of Square\n')

# Silhouette method

# Use the fviz_nbclust function with the method "silhouette" 
silhouette_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "silhouette")

silhouette_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of clusters k') + 
  ylab('Average silhouette width\n')

# Gap Statistic method

# We will use the clusGap function to apply the Gap Statistic Method

gap_stat <- clusGap(k_means_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# use the fviz_gap_stat function to vizualize the results

gap_stat_method <- fviz_gap_stat(gap_stat)

gap_stat_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of clusters k') + 
  ylab('Gap Statistic (k)\n')


# K-means algorithm

# set a random seed
suppressWarnings(set.seed(111, sample.kind = 'Rounding'))

# set k equal to the optimal number of clusters
num_clusters <- 3

# run the k-means algorithm 
k_means <- kmeans(k_means_data, centers = num_clusters, iter.max = 15, nstart = 25)

# label the clusters of major to show which cluster major belongs to
degrees_labeled <- degrees %>%
  mutate(clusters = k_means$cluster)

# Visualizing the clusters

# plot the clusters by Starting and Mid Career Median Salaries
career_growth <- ggplot(degrees_labeled, aes(x = Starting_Median_Salary, y = Mid_Career_Median_Salary, color=factor(clusters))) + 
  geom_point(alpha = 4/5, size = 7) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_color_manual(name = "Clusters", values = c("#EC2C73", "#29AEC7", "#FFDD30"))

career_growth + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nStarting Median Salary') + 
  ylab('Mid Career Median Salary\n')


# Deep Dive into the clusters

# NOw use the gather function to reshape degrees and use mutate() to reorder the new percentile column
degrees_perc <- degrees_labeled %>%
  select(Undergraduate_Major, Percentile_10, Percentile_25, Mid_Career_Median_Salary, Percentile_75, Percentile_90, clusters) %>%
  gather(key=percentile, value=salary, -c(Undergraduate_Major, clusters)) %>%
  mutate(percentile = factor(percentile, levels = c("Percentile_10", "Percentile_25", "Mid_Career_Median_Salary", "Percentile_75", "Percentile_90")))

# The liberal arts cluster

# Plot the majors of Cluster I by percentile
cluster_I <-  ggplot(degrees_perc %>% filter(clusters == 1), aes(x=percentile, y=salary, group=Undergraduate_Major, color=Undergraduate_Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)

cluster_I + theme_fivethirtyeight() + labs(color = "Undergraduate Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salary\n')

# The goldilocks cluster

# Plot the majors of Cluster II by percentile
cluster_II <-  ggplot(degrees_perc %>% filter(clusters == 2), aes(x=percentile, y=salary, group=Undergraduate_Major, color=Undergraduate_Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)

cluster_II + theme_fivethirtyeight() + labs(color = "Undergraduate Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salary\n')

# The over achiever cluster

# Plot the majors of Cluster III by percentile
cluster_III <-  ggplot(degrees_perc %>% filter(clusters == 3), aes(x=percentile, y=salary, group=Undergraduate_Major, color=Undergraduate_Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)

cluster_III + theme_fivethirtyeight() + labs(color = "Undergraduate Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salary\n')

# Finally, we will sort majors/degrees by Career_Percent_Growth
degrees_sorted <- degrees_labeled %>% arrange(desc(Career_Percent_Growth))
degrees_sorted %>% as_tibble()

