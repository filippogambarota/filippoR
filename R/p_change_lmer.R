p_change_lmer <- function(fit, group_factor){

  group_factor_tidy <- eval(str2expression(group_factor)) # for the filter evaluation

  dat <- fit@frame # get the dataframe

  fit <- as_lmerModLmerTest(fit) # refit for having p-value from lmerTest

  group <- as.character(unique(dat[, group_factor])) # all single group_factor levels

  # All datasets inside a list, without a group[i] element

  dat_list <- lapply(group, function(x){
    filter(dat, eval(str2expression(group_factor)) != x)
  })

  names(dat_list) <- paste0("remove_", group) # renaming the list

  # Fitting all models

  fit_list <- lapply(dat_list, function(x){
    suppressMessages(update(fit, data = x)) # broom for better models
  })

  # Getting warnings

  singular_fit <- sapply(fit_list, function(x) isSingular(x))

  # Extract all models info with broom

  fit_list <- lapply(fit_list, function(x){
    tidy(x) %>%
      filter(effect == "fixed") %>%
      select(effect, term, estimate, std.error, statistic, df, p.value)
  })

  #adding the full model

  fit_list$full_model <- tidy(fit) %>%
    filter(effect == "fixed") %>%
    select(effect, term, estimate, std.error, statistic, df, p.value)

  # Final dataset

  p_change <- bind_rows(fit_list, .id = "mod")

  p_change$sign <- ifelse(p_change$p.value < 0.05, "yes", "no")

  return(p_change)
}
