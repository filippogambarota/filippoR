dfbeta_plot <- function(infl, cutoff = "default", only_critic = TRUE){

  if(cutoff == "default"){
    cutoff <- 2/sqrt(n_group)
  }

  infl <- filter(infl, measure == "dfbeta")

  n_group <- nrow(unique(infl[ ,1]))

  group_factor <- colnames(infl)[1]

  infl$critic <- ifelse(abs(infl$value) > cutoff, "yes", "no")

  critic_param <- unique(infl$param[infl$critic == "yes"])

  if(length(critic_param != 0)){

    if(only_critic){
      infl <- infl[infl$param %in% critic_param, ]
    }

    ggplot(infl, aes_string(y = "value", x = group_factor, color = "critic")) +
      geom_point(size = 2) +
      scale_color_manual(values = c("black", "red")) +
      facet_wrap(~param) +
      geom_rect(aes(ymin = cutoff, xmax = +Inf, xmin = -Inf, ymax = +Inf),
                alpha = .01, fill = "lightpink2", color = NA) +
      geom_rect(aes(ymin = -cutoff, xmax = -Inf, xmin = +Inf, ymax = -Inf),
                alpha = .01, fill = "lightpink2", color = NA) +
      geom_hline(yintercept = cutoff, size = 0.7, linetype = "dashed", col = "red") +
      geom_hline(yintercept = -cutoff, size = 0.7, linetype = "dashed", col = "red") +
      geom_text(data = infl[infl$critic == "yes", ],
                aes_string(y = "value", x = group_factor, label = group_factor),
                hjust = -1) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
      ylab("DFBeta Values") +
      ggtitle("DFBeta Plot") -> df_beta_plot

    return(df_beta_plot)
  }
  else{

    message("The model has no critic DFBeta values according to the cutoff")

  }

}
