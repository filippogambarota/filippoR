influence_table <- function(model, group_factor){

  infl <- suppressMessages(influence(model, group = group_factor))

  if(isSingular(model)){
    warning("The model has some fitting problems, results could be unreliable")
  }

  dat <- model@frame

  dat %>%
    tibble() %>%
    select(group_factor) %>%
    distinct() %>%
    mutate(cook = as.vector(cooks.distance.estex(infl))) -> dat

  df_beta <- dfbetas.estex(infl)

  dat <- cbind(dat, df_beta)

  dat <- dat %>%
    pivot_longer(2:ncol(dat), names_to = "param", values_to = "value") %>%
    mutate(measure = ifelse(param == "cook", "cook", "dfbeta"),
           param = ifelse(param == "cook", NA, param))

  return(dat)
}
