
if (WCP == TRUE) { # within-chain parallelization

# WCP Model 2 Simple
if (exists("path_s")) {
  if (length(list.files(path_s)) > 0) {
    fit_s <- as_cmdstan_fit(list.files(path_s, full.names = TRUE))
  } else {
    model_s <- cmdstan_model(paste0('stan/', modelNr, '_simple_wcp.stan'),
                             cpp_options = list(stan_threads = TRUE))
    dir.create(path_s, recursive = TRUE, showWarnings = FALSE)
    # Run Stan Reparam Model
    fit_s <- model_s$sample(
      data = data_s,
      chains = Chains,
      parallel_chains=ParallelChains,
      threads_per_chain = CorePerChain, 
      refresh=100,
      iter_warmup = Burnin,
      iter_sampling = Iters,
      thin = 1,
      save_warmup = F,
      output_dir = path_s,
      output_basename = "chain")
  }
}

# WCP Model 2 Reparam
if (exists("path_r")) {
  if (length(list.files(path_r)) > 0) {
    fit_r <- as_cmdstan_fit(list.files(path_r, full.names = TRUE))
  } else {
    model_r <- cmdstan_model(paste0('stan/', modelNr, '_reparam_wcp.stan'),
                             cpp_options = list(stan_threads = TRUE))
    dir.create(path_r, recursive = TRUE, showWarnings = FALSE)
    # Run Stan Reparam Model
    fit_r <- model_r$sample(
      data = data_r,
      chains = Chains,
      parallel_chains=ParallelChains,
      threads_per_chain = CorePerChain, 
      refresh=100,
      iter_warmup = Burnin,
      iter_sampling = Iters,
      thin = 1,
      save_warmup = F,
      output_dir = path_r,
      output_basename = "chain")
  }
}

# WCP Model 2 Full
if (exists("path_f")) {
  if (length(list.files(path_f)) > 0) {
    fit_f <- as_cmdstan_fit(list.files(path_f, full.names = TRUE))
  } else {
    model_f <- cmdstan_model(paste0('stan/', modelNr, '_full_wcp.stan'),
                             cpp_options = list(stan_threads = TRUE))
    dir.create(path_f, recursive = TRUE, showWarnings = FALSE)
    # Run Stan Reparam Model
    fit_f <- model_f$sample(
      data = data_f,
      chains = Chains,
      parallel_chains=ParallelChains,
      threads_per_chain = CorePerChain, 
      refresh=100,
      iter_warmup = Burnin,
      iter_sampling = Iters,
      thin = 1,
      save_warmup = F,
      output_dir = path_f,
      output_basename = "chain")
  }
}
} else { # No within-chain parallelization

  # Simple Model
  if (exists("path_s")) {
    if (length(list.files(path_s)) > 0) {
      fit_s <- as_cmdstan_fit(list.files(path_s, full.names = TRUE))
    } else {
      model_s <- cmdstan_model(paste0('stan/', modelNr, '_simple.stan'))
      dir.create(path_s, recursive = TRUE, showWarnings = FALSE)
      # Run Stan Reparam Model
      fit_s <- model_s$sample(
        data = data_s,
        chains = Chains,
        parallel_chains=ParallelChains,
        refresh=250,
        iter_warmup = Burnin,
        iter_sampling = Iters,
        thin = 5,
        save_warmup = T,
        output_dir = path_s,
        output_basename = "chain")
    }
  }
  
  # Reparameterized Model
  if (exists("path_r")) {
    if (length(list.files(path_r)) > 0) {
      fit_r <- as_cmdstan_fit(list.files(path_r, full.names = TRUE))
    } else {
      model_r <- cmdstan_model(paste0('stan/', modelNr, '_reparam.stan'))
      dir.create(path_r, recursive = TRUE, showWarnings = FALSE)
      # Run Stan Reparam Model
      fit_r <- model_r$sample(
        data = data_r,
        chains = Chains,
        parallel_chains=ParallelChains,
        refresh=250,
        iter_warmup = Burnin,
        iter_sampling = Iters,
        thin = 5,
        save_warmup = T,
        output_dir = path_r,
        output_basename = "chain")
    }
  }
  
  # Full Model
  if (exists("path_f")) {
    if (length(list.files(path_f)) > 0) {
      fit_f <- as_cmdstan_fit(list.files(path_f, full.names = TRUE))
    } else {
      model_f <- cmdstan_model(paste0('stan/', modelNr, '_full.stan'))
      dir.create(path_f, recursive = TRUE, showWarnings = FALSE)
      # Run Stan Reparam Model
      fit_f <- model_f$sample(
        data = data_f,
        chains = Chains,
        parallel_chains=ParallelChains,
        refresh=250,
        iter_warmup = Burnin,
        iter_sampling = Iters,
        thin = 5,
        save_warmup = T,
        output_dir = path_f,
        output_basename = "chain")
    }
  }
  
}


