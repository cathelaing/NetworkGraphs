## Updated 4/10/2024

# Load data generated from the matrix_script_global_network file in the PhonologicalNetworksPrep repo

Lily_actual_saved <- read.table("Data/Lily_example_actual.txt",header=TRUE,row.names=1)
Lily_target_saved <- read.table("Data/Lily_example_target.txt",header=TRUE,row.names=1)

Lily_actual <- as.matrix(Lily_actual_saved)
Lily_target <- as.matrix(Lily_target_saved)

# standardize values

Q1_actual <- quantile(Lily_actual[Lily_actual > 0], 0.25)

Lily_actual <- ifelse(Lily_actual>Q1_actual, NA, Lily_actual)

Q1_target <- quantile(Lily_target[Lily_target > 0], 0.25)

Lily_target <- ifelse(Lily_target>Q1_target, NA, Lily_target)

# Transform it in a network graph format
network_actual <- graph_from_adjacency_matrix(Lily_actual, weighted = TRUE, diag=F)
network_target <- graph_from_adjacency_matrix(Lily_target, weighted = TRUE, diag=F)

# Remove edges with NA weights
network_actual <- delete_edges(network_actual, E(network_actual)[is.na(E(network_actual)$weight)])
network_target <- delete_edges(network_target, E(network_target)[is.na(E(network_target)$weight)])

flip <- function(x) max(x)-x+1  # function to flip the values so that wider edges = closer connection

# Make the graph
ggraph(network_actual) +
  geom_edge_link(aes(edge_width=(edge_width=flip(weight))),
                    edge_colour="grey", edge_alpha=0.1) +
  geom_node_point(color="#69b3a2", size=8) +
  geom_node_text(aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  ggtitle("Actual network") +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  )

ggraph(network_target) +
  geom_edge_link(aes(edge_width=flip(weight)), edge_colour="grey", edge_alpha=0.1) +
  geom_node_point(color="#69b3a2", size=8) +
  geom_node_text(aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  ggtitle("Target network") +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  )

IPA_table_Lily <- read_csv("Data/FULLsample_withIPA.csv") %>%
  filter(Speaker == "Lily" & Age %in% c("01;01.17", "01;02.02", "01;02.13", "01;02.28")) %>%
  distinct(Gloss, IPAtarget, IPAactual) %>%
  arrange(Gloss) %>%
  group_by(Gloss) %>%
  slice(1) %>%
  rename("Target" = IPAtarget,
         "Actual" = IPAactual)
