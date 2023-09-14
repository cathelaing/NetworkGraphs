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
  theme(text = element_text(size = 16),
        legend.position = "bottom")

path_length_plot <- ggplot(data=subset(SWD_red,
                                       !(data_type %in% c("target", "WS_target"))),
                           aes(x = numNodes, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Network size (number of nodes)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")

clust_coef_plot_DT <- ggplot(data=subset(SWD_red,
                                      data_type %in% c("target", "actual")),
                          aes(x = numNodes, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size =16),
        legend.position = "bottom")

clust_coef_plot <- ggplot(data=subset(SWD_red,
                                      !(data_type %in% c("target", "WS_target"))),
                          aes(x = numNodes, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Network size (number of nodes)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")

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
  theme(text = element_text(size = 16),
        legend.position = "bottom")

path_length_plot_age <- ggplot(data=subset(SWD_red,
                                       !(data_type %in% c("target", "WS_target"))),
                           aes(x = age, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")

clust_coef_plot_DT_age <- ggplot(data=subset(SWD_red,
                                         data_type %in% c("target", "actual")),
                             aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size =16),
        legend.position = "bottom")

clust_coef_plot_age <- ggplot(data=subset(SWD_red,
                                      !(data_type %in% c("target", "WS_target"))),
                          aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos–Renyi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")
