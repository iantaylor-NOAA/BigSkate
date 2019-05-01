getbs <- function(n, plot = FALSE){
  require(r4ss)
  # function to figure out full path associated with a given model number,
  # run SS_output and assign results to an object with a name like "bs9"
  bsdir <- 'C:/SS/skates/models/'
  mods <- dir(bsdir)
  mod <- mods[grep(pattern = paste0("bigskate(0)*", n), mods)]
  message("reading model from: ", mod)
  out <- SS_output(file.path(bsdir, mod))
  assign(x=paste0("bs", n), value=out, pos=1)
  if(plot){
    SS_plots(out, res=200)
  }
}


#SSplotPars(bs11, strings=rownames(bs11$estimated_non_dev_parameters)[bs11$estimated_non_dev_parameters$Pr_type!="No_prior"])
