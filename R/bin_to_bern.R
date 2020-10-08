bin_to_bern <- function(data_bin){
  
  success = lapply(data_bin$success, function(x){
    rep(1, x)
  })
  
  fail = lapply(data_bin$ntrials - data_bin$success, function(x){
    rep(0, x)
  })
  
  acc = unlist(mapply(c, success, fail, SIMPLIFY=FALSE))
  
  data_bern = slice(data_bin, rep(1:length(data_bin$id), data_bin$ntrials))
  
  data_bern$acc = acc
  
  return(data_bern)
}











