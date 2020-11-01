diagnostic_plot <- function(model, return_plots = F){

  diagnostic <- tibble(
    fitted = fitted(model),
    res_response = resid(model, type = "response"),
    res_standard = resid(model, type = "response")/sigma(model),
    res_sqrt = sqrt(abs(res_standard))
  )

  ggplot(diagnostic, aes(x = fitted, y = res_response)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", col = "blue") +
    xlab("Fitted Values") +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted") +
    theme(aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5)) -> fitted_residuals

  ggplot(diagnostic, aes(sample = res_standard)) +
    geom_qq_line(color = "red", linetype = "dashed") +
    geom_qq(alpha = 0.7) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5)) -> qq_plot


  ggplot(diagnostic, aes(x = fitted, y = sqrt(abs(res_standard)))) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", col = "blue", se = F) +
    theme(aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5)) +
    ylab(expression(sqrt("Standardized Residuals"))) +
    xlab("Fitted Values") +
    ggtitle("Scale-Location") -> scale_location

  diagnostic_plot <- grid.arrange(
    fitted_residuals,
    qq_plot,
    scale_location,
    nrow = 1,
    top = "Diagnostic Plots",
    respect = T)

  if(return_plots){

    diagnostic_list <- list(
      diagnostic_plot = diagnostic_plot,
      fitted_residuals = fitted_residuals,
      qq_plot = qq_plot,
      scale_location = scale_location
    )

    return(diagnostic_list)
  }else{
    return(diagnostic_plot)
  }
}
