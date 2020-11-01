cook_plot <- function(infl, cutoff = "default"){

  infl <- filter(infl, measure == "cook")

  n_group <- nrow(infl)

  if(cutoff == "default"){

    cutoff <- 4/n_group
  }

  infl$critic <- ifelse(infl$value > cutoff, "yes", "no")

  group_factor <- colnames(infl)[1]

  cookplot <- infl %>%
    ggplot(aes_string(x = "value", y = group_factor, color = "critic")) +
    geom_rect(aes(xmin = cutoff, xmax = +Inf, ymin = -Inf, ymax = +Inf),
              alpha = .01, fill = "lightpink2", color = NA) +
    geom_point(size = 3) +
    geom_vline(xintercept = cutoff, col = "red", linetype = "dashed", size = 1) +
    xlab("Cook Distance") +
    theme(aspect.ratio = 1) +
    ggtitle(paste("cut-off =", round(cutoff, 2))) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "red"))

  return(cookplot)

}
