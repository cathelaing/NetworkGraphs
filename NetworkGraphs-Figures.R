path_length_plot_DT <- ggplot(data=subset(SWD_red,
                                       data_type %in% c("target", "actual")),
                           aes(x = numNodes, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 16))

path_length_plot <- ggplot(data=subset(SWD_red,
                                       !(data_type %in% c("target", "WS_target"))),
                           aes(x = age, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Rényi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                        breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                        labels=c("Real", "Simulated - random (Erdos–Rényi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 16))

clust_coef_plot_DT <- ggplot(data=subset(SWD_red,
                                      data_type %in% c("target", "actual")),
                          aes(x = numNodes, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size =16))

clust_coef_plot <- ggplot(data=subset(SWD_red,
                                      !(data_type %in% c("target", "WS_target"))),
                          aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_fill_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Rényi)", "Simulated - small world (Watts-Strogatz)")) +
  scale_colour_discrete(name="Data type",
                      breaks=c("actual", "Erdos_Renyi", "WS_actual"),
                      labels=c("Real", "Simulated - random (Erdos–Rényi)", "Simulated - small world (Watts-Strogatz)")) +
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 16))

threshold.comp <- ggplot(data=globalthresholds,
                         aes(x = threshold, y = estimate)) +
  geom_point(aes(fill = data_type, colour = data_type, shape = corpus)) +
  geom_line(aes(colour = data_type, linetype = corpus)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  # xlab("Age (months)") +
  # ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")

connectivity <- ggplot(data=subset(SWD_red, data_type %in% c("actual", "target")),
                         aes(x = numNodes, y = mean_k)) +
  geom_point(aes(fill = data_type, colour = data_type, shape = corpus)) +
  geom_smooth(aes(fill = data_type, colour = data_type, linetype = corpus)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Network size (number of nodes)") +
  ylab("Mean number of connections per node (mean k)") +
  theme_bw()  +
  theme(text = element_text(size = 16),
        legend.position = "bottom")

# phonological.distance <- ggplot(comparison_summary, aes(x = age, y = dist_mean, colour = Speaker)) +
#   geom_smooth() +  
#   #geom_point(shape = 1, size = 1, position=position_jitter(.1), colour = "lightgrey") +
#   #coord_cartesian(ylim=c(0,4.5)) +
#   xlab("Age (months)") +
#   ylab("Mean phonological distance between \nActual and Target form") +
#   theme_bw(base_size = 18) +
#   facet_wrap(~Speaker, ncol=3)

