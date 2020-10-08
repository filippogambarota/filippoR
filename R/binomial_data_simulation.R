sim_binomial = function(sample_size, ntrials, p, nsim = 1, sample_p = FALSE, mean_p = 0, sd_p = 0) {
  require(dplyr)
  
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
        p = p)
    })
  })
  sim = lapply(sim, function(x){dplyr::bind_rows(x)})
  sim = dplyr::bind_rows(sim)
  return(sim)
}
