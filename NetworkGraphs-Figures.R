## Updated 21/10/2024

### Network size by month

monthly_types <- comparison_data %>%
  group_by(Speaker) %>%
  distinct(Gloss, .keep_all=T) %>%
  ungroup() %>%
  group_by(Speaker, corpus, age) %>%
  tally() %>% 
  ungroup() %>%
  group_by(Speaker) %>%
  mutate(csum = cumsum(n)) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = age, values_from = csum) %>%
  dplyr::select(Speaker, corpus, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`, `29`, `30`) %>%
  arrange(desc(corpus)) %>%
  rename(Corpus = corpus) %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

### Example networks

# Load data generated from the matrix_script_global_network file in the PhonologicalNetworksPrep repo

Lily_actual_saved <- read.table("Data/Lily_example_actual.txt",header=TRUE,row.names=1)
Lily_target_saved <- read.table("Data/Lily_example_target.txt",header=TRUE,row.names=1)

Lily_actual <- as.matrix(Lily_actual_saved)
Lily_target <- as.matrix(Lily_target_saved)

# standardize values

Q1_actual <- quantile(Lily_actual[Lily_actual > 0], 0.25)

Lily_actual <- ifelse(Lily_actual>Q1_actual, NA, Lily_actual)
Lily_actual <- ifelse(Lily_actual==0, 0.1, Lily_actual)

Q1_target <- quantile(Lily_target[Lily_target > 0], 0.25)

Lily_target <- ifelse(Lily_target>Q1_target, NA, Lily_target)
Lily_target <- ifelse(Lily_target==0, 0.1, Lily_target)

# Transform it in a network graph format
network_actual <- graph_from_adjacency_matrix(Lily_actual, weighted = TRUE, diag=F)
network_target <- graph_from_adjacency_matrix(Lily_target, weighted = TRUE, diag=F)

# Remove edges with NA weights
network_actual <- delete_edges(network_actual, E(network_actual)[is.na(E(network_actual)$weight)])
network_target <- delete_edges(network_target, E(network_target)[is.na(E(network_target)$weight)])

flip <- function(x) max(x)-x+1  # function to flip the values so that wider edges = closer connection

# Make the graph
lily_actual <- ggraph(network_actual) +
  geom_edge_link(aes(edge_width=(edge_width=flip(weight))),
                 edge_colour="grey", edge_alpha=0.1) +
  geom_node_point(color="#69b3a2", size=2) +
  geom_node_text(aes(label=name), repel = TRUE, size=4, color="#69b3a2") +
  theme_void() +
  ggtitle("Actual network") +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  )

lily_target <- ggraph(network_target) +
  geom_edge_link(aes(edge_width=flip(weight)), edge_colour="grey", edge_alpha=0.1) +
  geom_node_point(color="#69b3a2", size=2) +
  geom_node_text(aes(label=name), repel = TRUE, size=4, color="#69b3a2") +
  theme_void() +
  ggtitle("Target network") +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  )

#multiplot(lily_actual, lily_target, cols=2)

IPA_table_Lily <- read_csv("Data/FULLsample_withIPA.csv") %>%
  filter(Speaker == "Lily" & Age %in% c("01;01.17", "01;02.02", "01;02.13", "01;02.28")) %>%
  distinct(Gloss, IPAtarget, IPAactual, .keep_all=T) %>%
  arrange(Gloss) %>%
  group_by(Gloss) %>%
  slice(1) %>%
  dplyr::select(Gloss, Age, IPAtarget, IPAactual) %>%
  mutate(Age = ifelse(Age %in% c("01;02.02", "01;02.13", "01;02.28"), 14, 13)) %>%
  arrange(Age, Gloss) %>%
  rename(Target = IPAtarget,
         Actual = IPAactual)

## Plots ~ Network size

path_length_plot_DT <- ggplot(data=subset(SWD_red,
                                       data_type %in% c("target", "actual")),
                           aes(x = numNodes, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 12),
        legend.position = "bottom", legend.box="vertical") +
  facet_wrap(~corpus, ncol=2)

path_length_plot <- ggplot(data=subset(SWD_red,
                                       !(data_type %in% c("target", "WS_target"))),
                           aes(x = numNodes, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Network size (number of nodes)") +
  ylab("Mean Path Length") +
  #guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw()  +
  theme(text = element_text(size = 10),
        legend.position = "bottom", legend.box="vertical") +
  facet_wrap(~corpus, ncol=2)

clust_coef_plot_DT <- ggplot(data=subset(SWD_red,
                                      data_type %in% c("target", "actual")),
                          aes(x = numNodes, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size =12),
        legend.position = "bottom", legend.box="vertical") +
  facet_wrap(~corpus, ncol=2)

clust_coef_plot <- ggplot(data=subset(SWD_red,
                                      !(data_type %in% c("target", "WS_target"))),
                          aes(x = numNodes, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Network size (number of nodes)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 10),
        legend.position = "bottom", legend.box="vertical") +
  facet_wrap(~corpus, ncol=2)

## Plots ~ Age

path_length_plot_DT_age <- ggplot(data=subset(SWD_red,
                                          data_type %in% c("target", "actual")),
                              aes(x = age, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 10),
        legend.position = "bottom") +
  facet_wrap(~corpus, ncol=2)

path_length_plot_age <- ggplot(data=subset(SWD_red,
                                       !(data_type %in% c("target", "WS_target"))),
                           aes(x = age, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 10),
        legend.position = "bottom") +
  facet_wrap(~corpus, ncol=2)

clust_coef_plot_DT_age <- ggplot(data=subset(SWD_red,
                                         data_type %in% c("target", "actual")),
                             aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size =10),
        legend.position = "bottom") +
  facet_wrap(~corpus, ncol=2)

clust_coef_plot_age <- ggplot(data=subset(SWD_red,
                                      !(data_type %in% c("target", "WS_target"))),
                          aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 10),
        legend.position = "bottom") +
  facet_wrap(~corpus, ncol=2)
