horror_data <- read.csv(
  "C:/Users/pc/Documents/bluesky_horror_data_updated.csv",
  stringsAsFactors = FALSE
)

print(head(horror_data))
print(dim(horror_data))
library(dplyr)
library(tidytext)
library(ggplot2)

words <- horror_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 15)

ggplot(words, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most Frequent Horror Movie Words",
       x = "Word", y = "Frequency") +
  theme_minimal()
ggsave("C:/Users/pc/Documents/horror_word_frequency.png",
       width = 8, height = 6, dpi = 300)
library(tidytext)
library(dplyr)

horror_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(cluster, word, sort = TRUE) %>%
  group_by(cluster) %>%
  slice_max(n, n = 5) %>%
  print(n = 15)
library(ggplot2)

ggplot(horror_data, aes(x = factor(cluster))) +
  geom_bar() +
  labs(
    title = "Horror Movie Post Clusters",
    x = "Cluster",
    y = "Number of Posts"
  ) +
  theme_minimal()
ggsave("C:/Users/pc/Documents/horror_clusters.png",
       width = 8, height = 6, dpi = 300)
library(ggplot2)

set.seed(123)
x <- matrix(rnorm(477 * 2), ncol = 2)

ggplot(data.frame(x, cluster = factor(horror_data$cluster)),
       aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Horror Movie Post Clusters",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
library(dplyr)

horror_data <- horror_data %>%
  mutate(engagement = likes + reposts + replies)

psychological <- horror_data %>%
  filter(search_term == "psychological horror") %>%
  pull(engagement)

supernatural <- horror_data %>%
  filter(search_term == "supernatural horror") %>%
  pull(engagement)

wilcox.test(psychological, supernatural)
install.packages("igraph")
library(dplyr)
library(tidytext)
library(igraph)

words <- horror_data %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  distinct(id, word)

edges <- inner_join(words, words, by = "id") %>%
  filter(word.x < word.y) %>%
  count(from = word.x, to = word.y, name = "weight") %>%
  filter(weight >= 2)

net <- graph_from_data_frame(edges, directed = FALSE)

plot(net, vertex.size = 5, vertex.label.cex = 0.6)
plot(net,
     vertex.size = 6,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     edge.width = 1,
     main = "Horror Movie Word Network")
head(sort(degree(net), decreasing = TRUE), 10)
png("horror_network.png", width = 1200, height = 900)

plot(net,
     vertex.size = 6,
     vertex.label.cex = 0.7,
     main = "Horror Movie Word Network")

dev.off()
degree(net)
betweenness(net)

cat("Nodes:", vcount(net), "\n")
cat("Edges:", ecount(net), "\n")

