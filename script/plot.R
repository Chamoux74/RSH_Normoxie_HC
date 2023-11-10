data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


test <- data_summary(stim_HC, varname = "MVC", groupnames = c("Condition", "Timing"))


ggplot() +
  geom_errorbar(
    aes(
      ymin = ifelse(Condition == "NOR", MVC , MVC - sd) ,
      ymax = ifelse(Condition == "PC", MVC , MVC +sd),
      x = Timing,
      y = MVC,
      group = Condition,
      color = Condition
    ),
    data = test,
    width = .05,
    position = position_dodge(0.1)
  ) +
  geom_line(
    aes(
      x = Timing,
      y = MVC,
      group = Condition,
      color = Condition
    ),
    size = 1,
    position = position_dodge(0.1),
    data = test
  ) +
  geom_line(
    aes(
      x = Timing,
      y = MVC,
      group = interaction(Sujet, Condition),
      color = Condition
    ),
    size = 0.05,
    alpha = 0.35,
    position = position_dodge(0.1),
    data = stim_HC
  ) +
  geom_point(
    aes(
      x = Timing,
      y = MVC,
      group = Condition,
      color = Condition
    ),
    data = test ,
    position = position_dodge(0.1),
    size = 4
  ) + scale_color_manual(
    "Condition",
    values = c('#03017c', '#d88600'),
    labels = c("NOR", "HC")
  ) + theme_classic() +
  theme(
    axis.text = element_text(size = 8) ,
    axis.title = element_text(size = 10 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 10) ,
    legend.background = element_rect(color = "black" , size = 0.1),
    axis.line = element_line(colour = "black",
                             linewidth = rel(1))
  ) +
  labs(x = "Timing", y = "MVC (Nm)") +
  scale_x_discrete(labels  = c("PRE", "POST")) +
  scale_y_continuous(breaks = seq(100, 270, 50)) +
  geom_bracket(
    xmin = c(0.97),
    xmax = c(1.97),
    y.position = c(215),
    label = c("p< 0.0001"),
    tip.length = 0.01,
    size = .5,
    inherit.aes = F,
    color = '#03017c',
    vjust = -0.5,
    label.size = 3
  ) +
  geom_bracket(
    xmin = c(1.02),
    xmax = c(2.02),
    y.position = c(120),
    label = c("p< 0.0001"),
    tip.length = -0.01,
    size = 0.5,
    inherit.aes = F,
    color = '#d88600',
    vjust = 2,
    label.size = 3
  )
