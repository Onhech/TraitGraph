# --- Psychological Similarity Network Graph ---
# This script demonstrates how to create a network graph that visualizes the
# psychological similarity between individuals in a group.

# 1. Load Necessary Libraries
# You may need to install these first: install.packages(c("dplyr", "igraph", "ggraph"))
library(dplyr)
library(igraph)
library(ggraph)

# 2. Create Example Data
# This tibble simulates data for 10 people.
group_data <- tibble::tribble(
  ~names,    ~HonestyHumility, ~Emotionality, ~Extroversion, ~Agreeableness, ~Conscientiousness, ~Openness, ~BIS_Anxiety, ~BAS_FunSeeking, ~BAS_Drive, ~BAS_Reward, ~Agency, ~Mastery, ~Belonging, ~Moral_Care, ~Moral_Fairness, ~Moral_Loyalty, ~Moral_Authority, ~Moral_Purity,
  "Alice",   75,               40,            80,            85,             90,                 70,        30,           75,              90,         85,          90,      85,       80,          85,          90,              70,           65,               60,
  "Bob",     80,               45,            75,            90,             85,                 65,        35,           70,              85,         80,          85,      80,       85,          90,          85,              75,           70,               65,
  "Frank",   70,               50,            85,            80,             95,                 75,        25,           80,              95,         90,          95,      90,       75,          80,          80,              65,           60,               55,
  "Eve",     85,               70,            75,            80,             40,                 95,        40,           95,              60,         90,          85,      70,       90,          95,          70,              50,           30,               40,
  "Grace",   90,               65,            80,            75,             35,                 90,        45,           90,              65,         85,          80,      65,       95,          90,          75,              55,           35,               45,
  "Heidi",   80,               75,            70,            85,             45,                 85,        35,           85,              55,         95,          90,      75,       85,          85,          65,              45,           25,               35,
  "Charlie", 40,               80,            30,            50,             60,                 45,        90,           40,              50,         40,          50,      60,       55,          70,          60,              85,           90,               80,
  "David",   45,               75,            35,            55,             65,                 50,        85,           45,              55,         45,          55,      65,       60,          75,          65,              80,           85,               75,
  "Ivan",    35,               85,            25,            45,             55,                 40,        95,           35,              45,         35,          45,      55,       50,          65,          55,              90,           95,               85,
  "Judy",    50,               70,            40,            60,             70,                 55,        80,           50,              60,         50,          60,      70,       65,          80,          70,              75,           80,               70
) %>%
  mutate(initials = substr(names, 1, 1))

# 3. Prepare the Data for Analysis
similarity_data <- group_data %>%
  select(where(is.numeric))

# 4. Scale the Data
scaled_data <- scale(similarity_data)
rownames(scaled_data) <- group_data$initials

# 5. NEW: Calculate the Correlation Matrix
# We transpose the data so we're correlating people, not traits.
# The result is a matrix where each cell is the correlation between two people.
correlation_matrix <- cor(t(scaled_data))
diag(correlation_matrix) <- 0 # A person has 0 correlation with themselves for this graph

# 6. Create the Network Graph Object
# We use the correlation matrix directly as our adjacency matrix.
graph <- graph_from_adjacency_matrix(correlation_matrix, weighted = TRUE, mode = "undirected")

# Calculate Node Centrality (using the absolute correlation value for strength)
V(graph)$centrality <- strength(graph, weights = abs(E(graph)$weight))

# 7. Plot the Network with ggraph
# NEW: Tell the layout algorithm to use the ABSOLUTE value of the weights for positioning
ggraph(graph, layout = 'fr', weights = abs(E(graph)$weight)) +
  # Draw the edges and color them by their weight (correlation)
  geom_edge_link(aes(width = abs(weight), color = weight), alpha = 0.7) +
  scale_edge_width(range = c(0.2, 3), name = "Correlation Strength", guide = "none") +
  
  # Use a three-color gradient for the edges (Positive-Neutral-Negative)
  scale_edge_color_gradient2(
    low = "tomato", 
    mid = "lightgray", 
    high = "dodgerblue", 
    midpoint = 0, # Set the neutral point to 0 for correlation
    name = "Correlation"
  ) +
  
  # Draw the nodes and SIZE them by their centrality score
  geom_node_point(aes(size = centrality), color = "skyblue") +
  scale_size_continuous(range = c(8, 20), name = "Centrality", guide = "none") +
  
  # Add the initials inside the nodes
  geom_node_text(aes(label = name), size = 6, color = "white") +
  
  # Use a clean, minimal theme and adjust legend
  theme_graph(base_family = 'sans') +
  theme(
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add a more informative title and subtitle
  labs(
    title = "Psychological Correlation Network",
    subtitle = "Blue lines connect similar profiles, red lines connect opposite profiles. Larger circles are more central."
  )
