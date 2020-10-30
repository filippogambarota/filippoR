sim_binomial = function(sample_size, ntrials, p, nsim = 1, sample_p = FALSE, mean_p = 0, sd_p = 0,
                        sample_trials = FALSE, mean_trial = 0, sd_trial = 0, min_unif = 0, max_unif = 0, dist_trial = "norm") {
  require(dplyr)

  if(sample_trials){
    if(dist_trial == "norm"){
      ntrials = round(rnorm(sample_size, mean_trial, sd_trial)) # get individual trials
    }else if(dist_trial == "unif"){
      ntrials = round(runif(sample_size, min_unif, max_unif))
    }
    if(sample_p){
      p = round(rnorm(sample_size, mean_p, sd_p),2)
    }
    temp = as.vector(replicate(nsim, expr = rbinom(sample_size, ntrials, p)))
    sim = data.frame(
      id = 1:sample_size,
      success = temp,
      nsim = rep(1:nsim, each = sample_size),
      sample_size = sample_size,
      ntrials = ntrials,
      p = p,
      p_sample = temp/ntrials)
  }
  else{
    sim = lapply(sample_size, function(x) { # sample size level
      if(sample_p){
        p = rnorm(x, mean_p, sd_p)
      }
      lapply(ntrials, function(y) { # trials level
        temp = as.vector(replicate(nsim, expr = rbinom(x, y, p)))
        temp_sim = data.frame(
          id = 1:x,
          success = temp,
          nsim = rep(1:nsim, each = x),
          sample_size = x,
          ntrials = y,
          p = p,
          p_sample = temp/y)
      })
    })
    sim = lapply(sim, function(x){dplyr::bind_rows(x)})
    sim = dplyr::bind_rows(sim)
  }
  return(sim)
}
