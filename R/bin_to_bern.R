bin_to_bern <- function(data_bin){

  success = lapply(data_bin$success, function(x){
    rep(1, x)
  })

  fail = lapply(data_bin$fail, function(x){
    rep(0, x)
  })

  acc = unlist(mapply(c, success, fail, SIMPLIFY=FALSE))

  data_bin <- lazy_dt(data_bin)

  data_bern <- data_bin %>%
    slice(rep(1:length(id), ntrials)) %>%
    as.data.table() %>%
    as_tibble()

  data_bern$acc = acc

  return(data_bern)
}
