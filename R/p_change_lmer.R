p_change_lmer <- function(fit, group_factor){

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

  fit_list$full_model <- fit #adding the full model

  # Getting warnings

  singular_fit <- lapply(fit_list, function(x) isSingular(x))

  # Extract all models info with broom

  fit_list <- lapply(fit_list, function(x){
    tidy(x) %>%
      filter(effect == "fixed") %>%
      select(effect, term, estimate, std.error, statistic, df, p.value)
  })

  # Adding singularity warn

  for(i in 1:length(fit_list)){
    fit_list[[i]]$singular_fit <- singular_fit[[i]]
  }

  # Final dataset

  p_change <- bind_rows(fit_list, .id = "mod")

  p_change$sign <- ifelse(p_change$p.value < 0.05, "yes", "no")

  return(p_change)
}
