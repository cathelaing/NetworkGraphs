path_length_plot <- ggplot(data=subset(SWD_red,
                                       data_type %in% c("target", "actual")),
                           aes(x = age, y = path_length)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  theme_bw()  +
  theme(text = element_text(size = 20),
        legend.position = "bottom")
clust_coef_plot <- ggplot(data=subset(SWD_red,
                                      data_type %in% c("target", "actual")),
                          aes(x = age, y = clust_coef_avg)) +
  geom_smooth(size = 2, aes(fill = data_type, colour = data_type)) +
  scale_colour_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  scale_fill_manual(name = "Data type", labels = c("Actual", "Target"), values = c("#F8766D", "#00BFC4")) + 
  xlab("Age (months)") +
  ylab("Clustering Coefficient") +
  theme_bw()  +
  theme(text = element_text(size = 20),
        legend.position = "bottom")
