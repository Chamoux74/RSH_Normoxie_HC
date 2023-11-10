test <- data_summary(stim_HC, varname = "Naprime", groupnames = c("Condition", "Timing"))


b <-ggplot() +
  geom_errorbar(
    aes(
      ymin = Naprime - sd ,
      ymax =  Naprime +sd,
      x = Timing,
      y = Naprime,
      color = Condition
    ),
    data = test,
    width = .05
  ) +
  geom_line(
    aes(
      x = Timing,
      y = Naprime,
      color = Condition ,
      group = Condition
    ),
    size = 1,
    data = test
  ) +
  geom_line(
    aes(
      x = Timing,
      y = Naprime,
      group = Sujet,
      color = Condition
    ),
    size = 0.05,
    alpha = 0.35,
    data = stim_HC
  ) +
  geom_point(
    aes(
      x = Timing,
      y = Naprime,
      color = Condition
    ),
    data = test,
    size = 4
  ) + scale_color_manual(
    "Condition",
    values = c('#03017c', '#d88600')
  ) + theme_classic() +
  theme(
    axis.text = element_text(size = 8) ,
    axis.title = element_text(size = 10 , face = "bold"),
    axis.line = element_line(colour = "black",
                             linewidth = rel(1)) , legend.position = "none",
  ) +
  labs(x = "Timing", y = "Corrected activation level (%)") +
  scale_x_discrete(labels  = c("PRE", "POST")) +
  scale_y_continuous(breaks = seq(70, 110, 5), limits = c(70,105)) +
  facet_grid(~Condition)

b

b +
  geom_bracket(data = subset(stim_HC, Condition == "NOR"),
               xmin = 1,
               xmax = 2 ,
               y.position = 99 ,
               label = "p= 0.028",
               tip.length = 0.01,
               size = .5,
               inherit.aes = F,
               vjust = -0.5,
               label.size = 3) +
  geom_bracket(data = subset(stim_HC, Condition == "HC"),xmin = 1,
               xmax = 2 ,
               y.position = 99 ,
               label = "p= 0.012",
               tip.length = 0.01,
               size = .5,
               inherit.aes = F,
               vjust = -0.5,
               label.size = 3)
