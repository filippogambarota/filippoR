sim_binomial <- function(sample_size, ntrials, p, nsim = 1, sample_p = FALSE, sample_trials = FALSE,
                         dist_trial = "norm", dist_p = "norm",
                         mean_p = 0, sd_p = 0, mean_trial = 0, sd_trial = 0, min_unif = 0, max_unif = 0,
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
      dat$ntrials <- round(runif(x, min_unif_trial, max_unif_trial))
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
      dat$p <- round(runif(x, min_unif_p, max_unif_p), 3)
    }
  }

  else if(sample_trials & sample_p){

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
      dat$ntrials <- round(runif(x, min_unif_trial, max_unif_trial))
    }

    if(dist_p == "norm"){
      dat$p <- round(rnorm(nrow(dat), mean_p, sd_p), 3)
    }
    else if(dist_p == "unif"){
      dat$p <- round(runif(x, min_unif_p, max_unif_p), 3)
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

  dat %>%
    mutate(fail = ntrials - success,
           p_sample = success/ntrials,
           q_sample = fail/ntrials) %>%
    select(id, sample_size, ntrials, p, success, fail, p_sample, q_sample, nsim) -> dat

  # Returning

  return(dat)

}
