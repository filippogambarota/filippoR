sim_binomial <- function(sample_size, ntrials, p, nsim = 1, sample_p = FALSE, sample_trials = FALSE,
                         dist_trial = "norm", dist_p = "norm",
                         mean_p = 0, sd_p = 0, mean_trial = 0, sd_trial = 0, min_unif_p = 0, max_unif_p = 0,
                         min_unif_trial = 0, max_unif_trial = 0,
                         multi_sample_p = F,
                         parallel = F){

  # Creating Dataframe with conditions

  nsim <- 1:nsim

  if(sample_trials & !sample_p){

    dat <- expand_grid(
      sample_size,
      p
    )

    id <- unlist(lapply(dat$sample_size, function(x) 1:x))

    dat <- slice(dat, rep(1:n(), dat$sample_size))

    dat$id <- id

    dat <- expand_grid(
      dat,
      nsim
    )

    if(dist_trial == "norm"){
      dat$ntrials <- round(rnorm(nrow(dat), mean_trial, sd_trial))
    }
    else if(dist_trial == "unif"){
      dat$ntrials <- round(runif(nrow(dat), min_unif_trial, max_unif_trial))
    }

  }
  else if(sample_p & !sample_trials){

    dat <- expand_grid(
      sample_size,
      ntrials
    )

    id <- unlist(lapply(dat$sample_size, function(x) 1:x))

    dat <- slice(dat, rep(1:n(), dat$sample_size))

    dat$id <- id

    dat <- expand_grid(
      dat,
      nsim
    )

    if(dist_p == "norm"){
      dat$p <- round(rnorm(nrow(dat), mean_p, sd_p), 3)
    }
    else if(dist_trial == "unif"){
      dat$p <- round(runif(nrow(dat), min_unif_p, max_unif_p), 3)
    }
  }

  else if(sample_trials & sample_p & multi_sample_p){   ##### NEW

    dat <- expand_grid(
      sample_size,
      nsim
    )

    id <- unlist(lapply(dat$sample_size, function(x) 1:x))

    dat <- slice(dat, rep(1:n(), dat$sample_size))

    dat$id <- id

    if(dist_trial == "norm"){
      dat$ntrials <- round(rnorm(nrow(dat), mean_trial, sd_trial))
    }
    else if(dist_trial == "unif"){
      dat$ntrials <- round(runif(nrow(dat), min_unif_trial, max_unif_trial))
    }

    if(dist_p == "norm"){

      temp_p <- tibble(mean_p, np = 1:length(mean_p))

      dat <- expand_grid(dat, temp_p, sd_p)

      dat$p = mapply(function(x, y) round(rnorm(1, x, y), 3), dat$mean_p, dat$sd_p)
    }
    else if(dist_p == "unif"){

      temp_p <- tibble(min_unif_p, max_unif_p, np = 1:length(max_unif_p))

      dat <- expand_grid(dat, temp_p)

      dat$p = mapply(function(x, y) round(runif(1, x, y), 3), dat$min_unif_p, dat$max_unif_p)

    }
  }

  else if(sample_trials & sample_p & !multi_sample_p){

    dat <- expand_grid(
      sample_size,
      nsim
    )

    id <- unlist(lapply(dat$sample_size, function(x) 1:x))

    dat <- slice(dat, rep(1:n(), dat$sample_size))

    dat$id <- id

    if(dist_trial == "norm"){
      dat$ntrials <- round(rnorm(nrow(dat), mean_trial, sd_trial))
    }
    else if(dist_trial == "unif"){
      dat$ntrials <- round(runif(nrow(dat), min_unif_trial, max_unif_trial))
    }

    if(dist_p == "norm"){
      dat$p <- round(rnorm(nrow(dat), mean_p, sd_p), 3)
    }
    else if(dist_p == "unif"){
      dat$p <- round(runif(nrow(dat), min_unif_p, max_unif_p), 3)
    }
  }

  else if(!sample_trials & !sample_p){

    dat <- expand_grid(
      sample_size,
      p,
      ntrials,
      nsim
    )

    id <- unlist(lapply(dat$sample_size, function(x) 1:x))

    dat <- slice(dat, rep(1:n(), dat$sample_size))

    dat$id <- id

  }

  # Simulating Binomial Data

  if(parallel){
    warning("using plan(multicore)")
    plan(multicore)
    dat$success <- future_mapply(function(x, y) rbinom(1, x, y), dat$ntrials, dat$p,
                                 future.seed = T)
  }
  else{
    dat$success <- pbmapply(function(x, y) rbinom(1, x, y), dat$ntrials, dat$p)
  }

  # Cleaning dataset

  if(multi_sample_p & dist_p == "norm"){
    dat %>%
      mutate(fail = ntrials - success,
             p_sample = success/ntrials,
             q_sample = fail/ntrials,
             se = sqrt((p_sample*q_sample)/ntrials)) %>%
      select(id, sample_size, ntrials, p, success, fail, p_sample, q_sample, nsim, se, mean_p) -> dat
  }

  else if(multi_sample_p & dist_p == "unif"){
    dat %>%
      mutate(fail = ntrials - success,
             p_sample = success/ntrials,
             q_sample = fail/ntrials,
             se = sqrt((p_sample*q_sample)/ntrials)) %>%
      select(id, sample_size, ntrials, p, success, fail, p_sample, q_sample, nsim, se, max_unif_trial) -> dat
  }
  else{
    dat %>%
      mutate(fail = ntrials - success,
             p_sample = success/ntrials,
             q_sample = fail/ntrials,
             se = sqrt((p_sample*q_sample)/ntrials)) %>%
      select(id, sample_size, ntrials, p, success, fail, p_sample, q_sample, nsim, se) -> dat
  }

  # Returning

  return(dat)

}
