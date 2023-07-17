rhat_comp_bars <- function(savePlots = TRUE) {
  
  mplus_draws <- read.table(paste0("mplus/", modelName, "/draws_m", modelNr, ".dat"), header=F)
  
  save_rhat <- data.frame(rhat = rep(NA, times = ncol(mplus_draws)))
  save_bESS <- data.frame(ess_bulk = rep(NA, times = ncol(mplus_draws)))
  
  for (r in 3:ncol(mplus_draws)) {
    
    save_rhat[r-2,1] = rstan::Rhat(matrix(mplus_draws[,r], 
                                          ncol= max(mplus_draws[,1]), 
                                          nrow = max(mplus_draws[,2])))
    
    save_bESS[r-2,1] = posterior::ess_bulk(matrix(mplus_draws[,r], 
                                          ncol= max(mplus_draws[,1]), 
                                          nrow = max(mplus_draws[,2])))
  }
  
  extract_time <- substr(grep("Elapsed Time:", 
              readLines(paste0("mplus/", modelName, "/model_", modelNr, ".out")), 
              value = TRUE),
         23, 30)
  
  mplus_time <- as.POSIXlt(paste(Sys.Date(), extract_time))
  mplus_time <- if (mplus_time$sec/60 >= .5) {
    1 + mplus_time$hour*60 + mplus_time$min
  } else {
    mplus_time$hour*60 + mplus_time$min
  }
    
  
  a = fit_s$summary()$rhat
  b = fit_r$summary()$rhat
  c = fit_f$summary()$rhat
  d = save_rhat[,1]
  a_name = "Simple"
  b_name = "Reparam"
  c_name = "Full"
  d_name = "Mplus"
  
  data <- rbind(data.frame(rhat = a, model = a_name, rating = NA),
                data.frame(rhat = b, model = b_name, rating = NA),
                data.frame(rhat = c, model = c_name, rating = NA),
                data.frame(rhat = d, model = d_name, rating = NA))
  
  for (r in 1:nrow(data)) {
    data$rating[r] = ifelse(data$rhat[r] >= 1.1,
                            ">1.10", ifelse(data$rhat[r] >= 1.05,
                                          "<1.10", ifelse(data$rhat[r] >= 1.01, "<1.05",
                                                         "<1.01")))}
  
  count <- na.omit(data) %>% 
    group_by(model, rating) %>%
    summarise(cnt = n()) %>%
    mutate(rhat = round(cnt/sum(cnt), 3)) %>% 
    dplyr::select(-cnt)
  
  data <- data.frame(model = rep(c(a_name, b_name, c_name, d_name), each = 4),
                     rating = c("<1.01", "<1.05", "<1.10", ">1.10"))
  
  data <- merge(data, count, by = c("model", "rating"), all = T)
  
  
  data$rating <- factor(data$rating,
                        levels = c("<1.01", "<1.05", "<1.10", ">1.10"))
  data$model <- factor(data$model,
                       levels = c(a_name, b_name, c_name, d_name))
  
  model_names <- c(
    `Simple`  = paste("Simple (runtime ~ ",round(mean(fit_s$metadata()$time$total) / 60, 0), "min)"),
    `Reparam` = paste("Reparameterized (runtime ~ ",round(mean(fit_r$metadata()$time$total) / 60, 0), "min)"),
    `Full` = paste("Full (runtime ~ ",round(mean(fit_f$metadata()$time$total) / 60, 0), "min)"),
    `Mplus` = paste("Mplus (runtime ~ ", mplus_time, "min)")
    )
  
  
  theme_set(theme_pubr())
  
  rhatPlot <- ggplot(data, aes(x = rating, y = rhat, fill = rating)) +
    geom_bar(stat = "identity", width = .7, color = "#011f4b") +
    labs(x = expression(paste("Potential Scale Reduction Factor (", hat(R), ")")), y = "%") +
    scale_fill_manual(values = c("#39B185", "#9CCB86","#E9E29C", "#EEB479"), 
                      name = "PSRF",
                      drop = F) +
    scale_x_discrete(labels = c(expression(paste(" < 1.01")),
                                expression(paste(" < 1.05")),
                                expression(paste(" < 1.10")),
                                expression(paste(" > 1.10")))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0,1.00)) +
    facet_wrap(vars(model),
               nrow = 2,
               scales = "free_x",
               labeller = as_labeller(model_names))
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(rhatPlot),
      filename = paste0("plots/", modelName, "/", modelNr, "_rhat.png"),
      width = 1200*2,
      height = 734*2.5,
      pointsize = 12,
      res = 300)
    } else {
      print(rhatPlot)
    }
}

# Missing Estimates

rhat_comp_bars_m <- function(savePlots = TRUE) {
  
  e = fit_05$summary()$rhat 
  f = fit_10$summary()$rhat
  g = fit_15$summary()$rhat
  h = fit_20$summary()$rhat
  e_name = "m5"
  f_name = "m10" 
  g_name = "m15" 
  h_name = "m20"
  
  data <- rbind(data.frame(rhat = e, model = e_name, rating = NA),
                data.frame(rhat = f, model = f_name, rating = NA),
                data.frame(rhat = g, model = g_name, rating = NA),
                data.frame(rhat = h, model = h_name, rating = NA))
  
  for (r in 1:nrow(data)) {
    data$rating[r] = ifelse(data$rhat[r] >= 1.1,
                            ">1.10", ifelse(data$rhat[r] >= 1.05,
                                          "<1.10", ifelse(data$rhat[r] >= 1.01, "<1.05",
                                                         "<1.01")))
  }
  
  count <- na.omit(data) %>% 
    group_by(model, rating) %>%
    summarise(cnt = n()) %>%
    mutate(rhat = round(cnt/sum(cnt), 3)) %>% 
    dplyr::select(-cnt)
  
  data <- data.frame(model = rep(c(e_name, f_name, g_name, h_name), each = 4),
                     rating = c("<1.01", "<1.05", "<1.10", ">1.10"))
  
  data <- merge(data, count, by = c("model", "rating"), all = T)
  
  
  data$rating <- factor(data$rating,
                        levels = c("<1.01", "<1.05", "<1.10", ">1.10"))
  data$model <- factor(data$model,
                       levels = c(e_name, f_name, g_name, h_name))
  
  model_names <- c(
    `m5`  = paste("5% Missing (runtime ~ ",round(mean(fit_05$metadata()$time$total) / 60, 0), "min)"),
    `m10` = paste("10% Missing (runtime ~ ",round(mean(fit_10$metadata()$time$total) / 60, 0), "min)"),
    `m15` = paste("15% Missing (runtime ~ ",round(mean(fit_15$metadata()$time$total) / 60, 0), "min)"),
    `m20` = paste("20% Missing (runtime ~ ",round(mean(fit_20$metadata()$time$total) / 60, 0), "min)")
  )
  
  
  theme_set(theme_pubr())
  
  rhatPlot_m <- ggplot(data, aes(x = rating, y = rhat, fill = rating)) +
    geom_bar(stat = "identity", width = .7, color = "#011f4b") +
    labs(x = expression(paste("Potential Scale Reduction Factor (", hat(R), ")")), y = "%") +
    scale_fill_manual(values = c("#39B185", "#9CCB86","#E9E29C", "#EEB479"), 
                      name = "PSRF",
                      drop = F) +
    scale_x_discrete(labels = c(expression(paste(" < 1.01")),
                                expression(paste(" < 1.05")),
                                expression(paste(" < 1.10")),
                                expression(paste(" > 1.10")))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0,1.00)) +
    facet_wrap(vars(model),
               nrow = 2,
               scales = "free_x",
               labeller = as_labeller(model_names))
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(rhatPlot_m),
      filename = paste0("plots/", modelName, "/", modelNr, "_rhat_m.png"),
      width = 1200*2,
      height = 734*2.5,
      pointsize = 12,
      res = 300)
  } else {
    print(rhatPlot_m)
  }
}


### Estimates

est_comp <- function(savePlots = TRUE) {
  
  gammaPlot <- function(varsNames = c("X", "Y", "phi_X", "phi_Y", "phi_XY", "beta_YX", "psi_X", "psi_Y")) {

    # stub to use with MPlus values
    mp <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
    # replace the values
    mp$m  <- as.numeric(mplus_means$est)         
    mp$ll <- as.numeric(mplus_means$lower_2.5ci)
    mp$hh <- as.numeric(mplus_means$upper_2.5ci)
    mp$l <- NA
    mp$h <- NA
    
    combined <- rbind(mcmc_intervals_data(fs, prob_outer = 0.95),
                      mcmc_intervals_data(fr, prob_outer = 0.95),
                      mcmc_intervals_data(ff, prob_outer = 0.95),
                      mp)
    
    combined$Model <- rep(c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"), each = nrow(mp))
    combined$Model <- factor(combined$Model, levels = c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"))
    if (modelNr == 1) {
      combined$parameter <- factor(combined$parameter,
                                   labels = c(expression(X),
                                              expression(Y), 
                                              expression(phi[X]), 
                                              expression(phi[Y]),
                                              expression(phi[XY]),
                                              expression(beta[YX])))
    } else {
      combined$parameter <- factor(combined$parameter,
                                   labels = c(expression(X),
                                              expression(Y), 
                                              expression(phi[X]), 
                                              expression(phi[Y]),
                                              expression(phi[XY]),
                                              expression(beta[YX]),
                                              expression(psi[X]),
                                              expression(psi[Y]))) 
    }
    
    combined <- combined %>% 
      rowwise() %>% 
      mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
             m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
             hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
      mutate(ll = ifelse(is.na(ll), 0, ll),
             m = ifelse(is.na(m), 0, m),
             hh = ifelse(is.na(hh), 0, hh)) %>% 
      mutate(vline = ifelse(ll <= 0 && hh >= 0,
                            as.numeric(0),
                            as.numeric(NA)))
    
    # theme_set(bayesplot::theme_default())
    theme_set(theme_pubr(base_size = 12))
    pos <- position_nudge(y = ifelse(combined$Model == "Stan Simple", .3,
                                     ifelse(combined$Model == "Stan Reparam", .1,
                                            ifelse(combined$Model == "Stan Full", -.1,
                                                   -.3))))
    brewer.pal(name="Blues",n=9)[c(9)]
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                    brewer.pal(name="Blues",n=9)[c(6)],
                    brewer.pal(name="Blues",n=9)[c(8)], 
                    "#D94725") 
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, color="black", size = 2.5, stroke = .6) +
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) +
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,2,4)) +
      scale_y_discrete(limits=rev) +
      xlab("") +
      ylab("") +
      facet_wrap(vars(parameter),
                 ncol = 2,
                 scales = "free",
                 labeller = label_parsed,
                 strip.position = "left") +
      theme(legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 14),
            strip.background = element_rect(fill="lightgrey"),
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    
  }
  
  latentPlot <- function() {
    varsNames <- c("C~gamma[1]", "C~gamma[2]", "C~phi_X", "C~phi_Y", "C~phi_XY", "C~beta_YX", 
                   "C~psi_X", "C~psi_Y")

    # stub to use with MPlus values
    mp <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
    # replace the values
    mp$m  <- as.numeric(mplus_weights$est)         
    mp$ll <- as.numeric(mplus_weights$lower_2.5ci)
    mp$hh <- as.numeric(mplus_weights$upper_2.5ci)
    mp$l <- NA
    mp$h <- NA
    
    combined <- rbind(mcmc_intervals_data(fs, prob_outer = 0.95),
                      mcmc_intervals_data(fr, prob_outer = 0.95),
                      mcmc_intervals_data(ff, prob_outer = 0.95),
                      mp)
    
    combined$Model <- rep(c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"), each = nrow(mp))
    combined$Model <- factor(combined$Model, levels = c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"))
    combined$parameter <- factor(combined$parameter,
                                 labels = c(expression(C ~ "~" ~ X),
                                            expression(C ~ "~" ~ Y), 
                                            expression(C ~ "~" ~ phi[X]), 
                                            expression(C ~ "~" ~ phi[Y]),
                                            expression(C ~ "~" ~ phi[XY]),
                                            expression(C ~ "~" ~ beta[YX]),
                                            expression(C ~ "~" ~ psi[X]),
                                            expression(C ~ "~" ~ psi[Y])))
    
    combined <- combined %>% 
      rowwise() %>% 
      mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
             m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
             hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
      mutate(ll = ifelse(is.na(ll), 0, ll),
             m = ifelse(is.na(m), 0, m),
             hh = ifelse(is.na(hh), 0, hh)) %>% 
      mutate(vline = ifelse(ll <= 0 && hh >= 0,
                            as.numeric(0),
                            as.numeric(NA)))
    
    theme_set(theme_pubr(base_size = 12))
    pos <- position_nudge(y = ifelse(combined$Model == "Stan Simple", .3,
                                     ifelse(combined$Model == "Stan Reparam", .1,
                                            ifelse(combined$Model == "Stan Full", -.1,
                                                   -.3))))
    brewer.pal(name="Blues",n=9)[c(9)]
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                    brewer.pal(name="Blues",n=9)[c(6)],
                    brewer.pal(name="Blues",n=9)[c(8)], 
                    "#D94725") 
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, color="black", size = 2.5, stroke = .6) + 
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) + 
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,2,4)) +
      scale_y_discrete(limits=rev) +
      xlab("") +
      ylab("") +
      facet_wrap(vars(parameter),
                 ncol = 2,
                 scales = "free",
                 labeller = label_parsed,
                 strip.position = "left") +  
      theme(legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 14),
        strip.background = element_rect(fill="lightgrey"),
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()
      )
  }
  
  
  predPlot <- function() {
    varsNames <- c("P~gamma[1]", "P~gamma[2]", "P~phi_X", "P~phi_Y", "P~phi_XY", 
                   "P~beta_YX", "P~psi_X", "P~psi_Y")
    
    # stub to use with MPlus values
    mp <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
    # replace the values
    mp$m  <- as.numeric(mplus_weights$est)         
    mp$ll <- as.numeric(mplus_weights$lower_2.5ci)
    mp$hh <- as.numeric(mplus_weights$upper_2.5ci)
    mp$l <- NA
    mp$h <- NA
    
    combined <- rbind(mcmc_intervals_data(fs, prob_outer = 0.95),
                      mcmc_intervals_data(fr, prob_outer = 0.95),
                      mcmc_intervals_data(ff, prob_outer = 0.95),
                      mp)
    
    combined$Model <- rep(c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"), each = nrow(mp))
    combined$Model <- factor(combined$Model, levels = c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"))
    combined$parameter <- factor(combined$parameter,
                                 labels = c(expression(P ~ "~" ~ X),
                                            expression(P ~ "~" ~ Y), 
                                            expression(P ~ "~" ~ phi[X]), 
                                            expression(P ~ "~" ~ phi[Y]),
                                            expression(P ~ "~" ~ phi[XY]),
                                            expression(P ~ "~" ~ beta[YX]),
                                            expression(P ~ "~" ~ psi[X]),
                                            expression(P ~ "~" ~ psi[Y])))
    
    combined <- combined %>% 
      rowwise() %>% 
      mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
             m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
             hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
      mutate(ll = ifelse(is.na(ll), 0, ll),
             m = ifelse(is.na(m), 0, m),
             hh = ifelse(is.na(hh), 0, hh)) %>% 
      mutate(vline = ifelse(ll <= 0 && hh >= 0,
                            as.numeric(0),
                            as.numeric(NA)))
    
    theme_set(theme_pubr(base_size = 12))
    pos <- position_nudge(y = ifelse(combined$Model == "Stan Simple", .3,
                                     ifelse(combined$Model == "Stan Reparam", .1,
                                            ifelse(combined$Model == "Stan Full", -.1,
                                                   -.3))))
    brewer.pal(name="Blues",n=9)[c(9)]
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                    brewer.pal(name="Blues",n=9)[c(6)],
                    brewer.pal(name="Blues",n=9)[c(8)], 
                    "#D94725") 
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, color="black", size = 2.5, stroke = .6) + 
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) + 
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,2,4)) +
      scale_y_discrete(limits=rev) +
      xlab("") +
      ylab("") +
      facet_wrap(vars(parameter),
                 ncol = 2,
                 scales = "free",
                 labeller = label_parsed,
                 strip.position = "left") +  
      theme(legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 14),
        strip.background = element_rect(fill="lightgrey"),
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()
      ) 
    
  }
  
  outcomePlot <- function() {
    varsNames <- c("O_intercept", "O~P", "O~gamma[1]", "O~gamma[2]", "O~phi_X", "O~phi_Y", 
                   "O~phi_XY", "O~beta_YX", "O~psi_X", "O~psi_Y")

    # stub to use with MPlus values
    mp <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
    # replace the values
    mp$m  <- as.numeric(mplus_O$est)         
    mp$ll <- as.numeric(mplus_O$lower_2.5ci)
    mp$hh <- as.numeric(mplus_O$upper_2.5ci)
    mp$l <- NA
    mp$h <- NA
    
    combined <- rbind(mcmc_intervals_data(fs, prob_outer = 0.95),
                      mcmc_intervals_data(fr, prob_outer = 0.95),
                      mcmc_intervals_data(ff, prob_outer = 0.95),
                      mp)
    
    combined$Model <- rep(c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"), each = nrow(mp))
    combined$Model <- factor(combined$Model, levels = c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus"))
    combined$parameter <- factor(combined$parameter,
                                 labels = c(
                                   expression(O[intercept]),
                                   expression(O ~ "~" ~ P),
                                   expression(O ~ "~" ~ X),
                                   expression(O ~ "~" ~ Y),
                                   expression(O ~ "~" ~ phi[X]),
                                   expression(O ~ "~" ~ phi[Y]),
                                   expression(O ~ "~" ~ phi[XY]),
                                   expression(O ~ "~" ~ beta[YX]),
                                   expression(O ~ "~" ~ psi[X]),
                                   expression(O ~ "~" ~ psi[Y])))
    
    combined <- combined %>% 
      rowwise() %>% 
      mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
             m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
             hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
      mutate(ll = ifelse(is.na(ll), 0, ll),
             m = ifelse(is.na(m), 0, m),
             hh = ifelse(is.na(hh), 0, hh)) %>% 
      mutate(vline = ifelse(ll <= 0 && hh >= 0,
                            as.numeric(0),
                            as.numeric(NA)))
    
    # theme_set(bayesplot::theme_default())
    theme_set(theme_pubr(base_size = 12))
    pos <- position_nudge(y = ifelse(combined$Model == "Stan Simple", .3,
                                     ifelse(combined$Model == "Stan Reparam", .1,
                                            ifelse(combined$Model == "Stan Full", -.1,
                                                   -.3))))
    brewer.pal(name="Blues",n=9)[c(9)]
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                    brewer.pal(name="Blues",n=9)[c(6)],
                    brewer.pal(name="Blues",n=9)[c(8)], 
                    "#D94725") 
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, color="black", size = 2.5, stroke = .6) + 
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) + 
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,2,4)) +
      scale_y_discrete(limits=rev) +
      xlab("") +
      ylab("") +
      facet_wrap(vars(parameter),
                 ncol = 2,
                 scales = "free",
                 labeller = label_parsed,
                 strip.position = "left") +  
      theme(legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 14),
        strip.background = element_rect(fill="lightgrey"),
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()
      )
  }
  
  
  ### Start of est_comp
  
  if (modelNr %in% c(1,2)) {
    if (modelNr == 1) {
      mplus_means <- rbind(subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                             what = "parameters")$parameters[["unstandardized"]], 
                                  paramHeader == "Means"), 
                           subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                             what = "parameters")$parameters[["unstandardized"]])
                           [1:4,])
      mplus_means <- mplus_means[c(1,2,3,5,6,4),]
      
      # Grab simple params
      varsNames <- c("gamma[1]", "gamma[2]", "phi_X", "phi_Y", "phi_XY", "beta_YX")
      vars <- c("gamma[1]", "gamma[2]", "phi_X", "phi_Y", "phi_XY", "beta_YX")
      fs <- fit_s$draws(variables = vars)

      # Grab reparam params
      fr <- fit_r$draws(variables = vars)

      # Grab full params
      ff <- fit_f$draws(variables = vars)

      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(gammaPlot(varsNames = vars)),
          filename = paste0("plots/", modelName, "/", modelNr, "_gamma.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300)
      } else {
        print(gammaPlot(varsNames = vars))
        }
      
    } else { # model 2
      mplus_means <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                       what = "parameters")$parameters[["unstandardized"]], 
                            paramHeader == "Means")
      
      # Grab simple params
      params <- fit_s$metadata()$model_params
      vars <- grep('gamma\\[', params, value = TRUE)
      fs <- fit_s$draws(variables = vars)

      # Grab reparam params
      params <- fit_r$metadata()$model_params
      vars <- grep('gamma\\[', params, value = TRUE)
      fr <- fit_r$draws(variables = vars)

      # Grab full params
      params <- fit_f$metadata()$model_params
      vars <- grep('gamma\\[', params, value = TRUE)
      ff <- fit_f$draws(variables = vars)

      
      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(gammaPlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_gamma.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(gammaPlot())
        }
      
    }
    
  } else if (modelNr %in% c(3,4)) {
    mplus_means <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                     what = "parameters")$parameters[["unstandardized"]], 
                          paramHeader == "Intercepts")
    
    # Grab simple params
    params <- fit_s$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    fs <- fit_s$draws(variables = vars)

    # Grab reparam params
    params <- fit_r$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    fr <- fit_r$draws(variables = vars)

    
    # Grab full params
    params <- fit_f$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    ff <- fit_f$draws(variables = vars)

    
    if (savePlots == TRUE) {
      dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
      ggexport(
        print(gammaPlot()),
        filename = paste0("plots/", modelName, "/", modelNr, "_gamma.png"),
        width = 1200*2,
        height = 734*2.5,
        pointsize = 12,
        res = 300
      )} else {
        print(gammaPlot())
      }
    
    if (modelNr == 3) {
      mplus_weights <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              param == "P")
      mplus_weights <- mplus_weights[c(7,8,1:6),]
      
      # Grab simple params
      params <- fit_s$metadata()$model_params
      vars <- grep('^a\\[', params, value = TRUE)
      fs <- fit_s$draws(variables = vars)
      
      # Grab reparam params
      params <- fit_r$metadata()$model_params
      vars <- grep('^a\\[', params, value = TRUE)
      fr <- fit_r$draws(variables = vars)

      # Grab full params
      params <- fit_f$metadata()$model_params
      vars <- c(grep('^a\\[', params, value = TRUE),
                grep('^f\\[', params, value = TRUE))
      ff <- fit_f$draws(variables = vars)
      dimnames(ff)[[3]] <- gsub('^f\\[','a\\[', dimnames(ff)[[3]])

      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(predPlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_predictor.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(predPlot())
        }
      
      
    } else { # model 4
      mplus_weights <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              paramHeader == "C.BY")
      mplus_weights <- mplus_weights[c(1:8),]
      
      # Grab simple params
      params <- fit_s$metadata()$model_params #if params contains a, then
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      }
      fs <- fit_s$draws(variables = vars)
      dimnames(fs)[[3]] <- gsub('^b\\[','a\\[', dimnames(fs)[[3]])
      
      # Grab reparam params
      params <- fit_r$metadata()$model_params
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      }
      fr <- fit_r$draws(variables = vars)
      dimnames(fr)[[3]] <- gsub('^b\\[','a\\[', dimnames(fr)[[3]])
      
      # Grab full params
      params <- fit_f$metadata()$model_params
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      }
      ff <- fit_f$draws(variables = vars)
      dimnames(ff)[[3]] <- gsub('^f\\[','a\\[', dimnames(ff)[[3]])

      

      
      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(latentPlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_latent.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(latentPlot())
        }
    }
    
  } else { # model 5/6
    mplus_means <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                     what = "parameters")$parameters[["unstandardized"]], 
                          paramHeader == "Intercepts")[-1,]
    
    # Grab simple params
    params <- fit_s$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    fs <- fit_s$draws(variables = vars)

    
    # Grab reparam params
    params <- fit_r$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    fr <- fit_r$draws(variables = vars)

    
    # Grab full params
    params <- fit_f$metadata()$model_params
    vars <- grep('gamma\\[', params, value = TRUE)
    ff <- fit_f$draws(variables = vars)

    
    if (savePlots == TRUE) {
      dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
      ggexport(
        print(gammaPlot()),
        filename = paste0("plots/", modelName, "/", modelNr, "_gamma.png"),
        width = 1200*2,
        height = 734*2.5,
        pointsize = 12,
        res = 300
      )} else {
        print(gammaPlot())
      }
    
    if (modelNr == 5) {
      mplus_weights <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              param == "P")
      mplus_weights <- mplus_weights[c(7,8,1:6),]
      
      mplus_O <- rbind(subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              paramHeader == "Intercepts" & param == "O"), 
                       subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              paramHeader == "O.ON")[c(7,8,9,1:6),])
      
      # Grab simple params
      params <- fit_s$metadata()$model_params
      vars <- grep('^a\\[', params, value = TRUE)
      fs <- fit_s$draws(variables = vars)
      
      
      # Grab reparam params
      params <- fit_r$metadata()$model_params
      vars <- grep('^a\\[', params, value = TRUE)
      fr <- fit_r$draws(variables = vars)

      
      # Grab full params
      params <- fit_f$metadata()$model_params
      vars <- c(grep('^a\\[', params, value = TRUE),
                grep('^f\\[', params, value = TRUE))
      ff <- fit_f$draws(variables = vars)
      dimnames(ff)[[3]] <- gsub('^f\\[','a\\[', dimnames(ff)[[3]])
      
      
      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(predPlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_predictor.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(predPlot())
        }
      ### O plots
      
      # Grab simple params
      params <- fit_s$metadata()$model_params
      vars <- grep('^tau\\[', params, value = TRUE)
      fs <- fit_s$draws(variables = vars)
      
      # Grab reparam params
      params <- fit_r$metadata()$model_params
      vars <- grep('^tau\\[', params, value = TRUE)
      fr <- fit_r$draws(variables = vars)
      
      # Grab full params
      params <- fit_f$metadata()$model_params
      vars <- grep('^tau\\[', params, value = TRUE)
      ff <- fit_f$draws(variables = vars)
      

      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(outcomePlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_outcome.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(outcomePlot())
        }
      
    } else { # model 6
      mplus_weights <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                         what = "parameters")$parameters[["unstandardized"]], 
                              paramHeader == "C.BY")
      
       # Grab simple params
      params <- fit_s$metadata()$model_params
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      }
      fs <- fit_s$draws(variables = vars)
      dimnames(fs)[[3]] <- gsub('^b\\[','a\\[', dimnames(fs)[[3]])
      
      # Grab reparam params
      params <- fit_r$metadata()$model_params
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^b\\[', params, value = TRUE))
      }
      fr <- fit_r$draws(variables = vars)
      dimnames(fr)[[3]] <- gsub('^b\\[','a\\[', dimnames(fr)[[3]])
      
      # Grab full params
      params <- fit_f$metadata()$model_params
      if ("a[1]" %in% params) { 
        vars <- c(grep('^a\\[', params, value = TRUE),
                  grep('^f\\[', params, value = TRUE))
      } else {
        vars <- c(grep('^a_\\[', params, value = TRUE),
                  grep('^f\\[', params, value = TRUE))
      }
      ff <- fit_f$draws(variables = vars)
      dimnames(ff)[[3]] <- gsub('^f_\\[','a\\[', dimnames(ff)[[3]])

      
      if (savePlots == TRUE) {
        dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
        ggexport(
          print(latentPlot()),
          filename = paste0("plots/", modelName, "/", modelNr, "_latent.png"),
          width = 1200*2,
          height = 734*2.5,
          pointsize = 12,
          res = 300
        )} else {
          print(latentPlot())
        }
      
      mplus_sem <- rbind(subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "P" & paramHeader == "O.ON"),
                         subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "P" & paramHeader == "C.ON"),
                         subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "C" & paramHeader == "O.ON"),
                         subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "C" & paramHeader == "Residual.Variances"),
                         subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "O" & paramHeader == "Residual.Variances"),
                         subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                           what = "parameters")$parameters[["unstandardized"]], 
                                param == "INDIRECT"))
      
    }
  }
}


# Missing Estimates

est_comp_m <- function(savePlots = TRUE) {
  
  # Grab full params
  params <- fit_f$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  ff <- fit_f$draws(variables = vars)
  
  # Grab 05 params
  params <- fit_05$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  f05 <- fit_05$draws(variables = vars)

  # Grab 10 params
  params <- fit_10$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  f10 <- fit_10$draws(variables = vars)

  # Grab 15 params
  params <- fit_15$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  f15 <- fit_15$draws(variables = vars)

  # Grab 20 params
  params <- fit_20$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  f20 <- fit_20$draws(variables = vars)

  varsNames = c("X", "Y", "phi_X", "phi_Y", "phi_XY", "beta_YX", "psi_X", "psi_Y")
  colnames(f05) <- varsNames
  colnames(f10) <- varsNames
  colnames(f15) <- varsNames
  colnames(f20) <- varsNames
  colnames(ff) <- varsNames
  
  combined <- rbind(mcmc_intervals_data(f05, prob_outer = 0.95),
                    mcmc_intervals_data(f10, prob_outer = 0.95),
                    mcmc_intervals_data(f15, prob_outer = 0.95),
                    mcmc_intervals_data(f20, prob_outer = 0.95),
                    mcmc_intervals_data(ff, prob_outer = 0.95)
  )
  
  combined$Model <- rep(c("Missing 5%", "Missing 10%", 
                          "Missing 15%", "Missing 20%", "Full"), each = ncol(f05)) # does f05 work here?
  combined$Model <- factor(combined$Model, levels = c("Missing 5%", "Missing 10%", 
                                                      "Missing 15%", "Missing 20%", "Full"))
  combined$parameter <- factor(combined$parameter,
                               labels = c(expression(X),
                                          expression(Y), 
                                          expression(phi[X]), 
                                          expression(phi[Y]),
                                          expression(phi[XY]),
                                          expression(beta[YX]),
                                          expression(psi[X]),
                                          expression(psi[Y]))) 
  
combined <- combined %>%
    rowwise() %>% 
    mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
           m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
           hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
    mutate(ll = ifelse(is.na(ll), 0, ll),
           m = ifelse(is.na(m), 0, m),
           hh = ifelse(is.na(hh), 0, hh)) %>% 
    mutate(vline = ifelse(ll <= 0 && hh >= 0,
                          as.numeric(0),
                          as.numeric(NA))) 
  
  
  # theme_set(theme_pubr(base_size = 12))
  # pos <- position_nudge(y = ifelse(combined$Model == "Missing 5%", .3,
  #                                  ifelse(combined$Model == "Missing 10%", .15,
  #                                         ifelse(combined$Model == "Missing 15%", 0,
  #                                                ifelse(combined$Model == "Missing 20%", -.15,
  #                                                       -.3)))))
  # 
  # my_palette <- c(brewer.pal(name="Blues",n=9)[c(3)], 
  #                 brewer.pal(name="Blues",n=9)[c(4)], 
  #                 brewer.pal(name="Blues",n=9)[c(6)], 
  #                 brewer.pal(name="Blues",n=9)[c(8)],
  #                 brewer.pal(name="Reds",n=9)[c(5)]) 
  # 
  # ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
  #   geom_point(position = pos, color="black", size = 2.5) + 
  #   geom_errorbar(
  #     aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
  #     width=0.1 , linewidth = .6,
  #     position=pos) + 
  #   geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
  #   # geom_vline(data = fullModel, aes(xintercept = ll), color = "red", linetype = "dashed", alpha = .5) +
  #   # geom_vline(data = fullModel, aes(xintercept = m), color = "red", linetype = "dashed") +
  #   # geom_vline(data = fullModel, aes(xintercept = hh), color = "red", linetype = "dashed", alpha = .5) +
  #   scale_color_manual(name = "", values = my_palette) +
  #   scale_shape_manual(name = "", values = c(0,1,2,4, 5)) +
  #   scale_y_discrete(limits=rev) +
  #   xlab("") +
  #   ylab("") +
  #   facet_wrap(vars(parameter),
  #              ncol = 2,
  #              scales = "free",
  #              labeller = label_parsed,
  #              strip.position = "left") +  
  #   theme(strip.background = element_rect(fill="lightgrey"),
  #         strip.text.y.left = element_text(angle = 0),
  #         axis.text.y = element_blank(), 
  #         axis.ticks.y = element_blank()
  #   ) 
  
###############  
  fullModel <- combined %>%
    filter(Model == "Full")
  
  combined <- combined %>%
    filter(Model != "Full")
  
  theme_set(theme_pubr(base_size = 12))
  pos <- position_nudge(y = ifelse(combined$Model == "Missing 5%", .3,
                                   ifelse(combined$Model == "Missing 10%", .1,
                                          ifelse(combined$Model == "Missing 15%", -.1,
                                                 -.3))))
  
  my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                  brewer.pal(name="Blues",n=9)[c(6)], 
                  brewer.pal(name="Blues",n=9)[c(8)], 
                  brewer.pal(name="Blues",n=9)[c(9)]) 
  
 gammaPlot_m <-  ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
    geom_point(position = pos, color="black", size = 2.5) + 
    geom_errorbar(
      aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
      width=0.1 , linewidth = .6,
      position=pos) + 
    geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
    geom_vline(data = fullModel, aes(xintercept = ll), color = "red", linetype = "dashed", alpha = .5) +
    geom_vline(data = fullModel, aes(xintercept = m), color = "red", linetype = "dashed") +
    geom_vline(data = fullModel, aes(xintercept = hh), color = "red", linetype = "dashed", alpha = .5) +
    scale_color_manual(name = "", values = my_palette) +
    scale_shape_manual(name = "", values = c(0,1,2,4)) +
    scale_y_discrete(limits=rev) +
    xlab("") +
    ylab("") +
    facet_wrap(vars(parameter),
               ncol = 2,
               scales = "free",
               labeller = label_parsed,
               strip.position = "left") +  
    theme(legend.key.size = unit(2, "lines"),
          legend.text = element_text(size = 14),
      strip.background = element_rect(fill="lightgrey"),
          strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()
    ) 
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(gammaPlot_m),
      filename = paste0("plots/", modelName, "/", modelNr, "_gamma_m.png"),
      width = 1200*2,
      height = 734*2.5,
      pointsize = 12,
      res = 300
    )} else {
      print(gammaPlot_m)
    }
}


# Simulated Data

est_comp_sim <- function(savePlots = TRUE) {
  
  # Grab simple params
  params <- fit_s$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  fs <- fit_s$draws(variables = vars)
  
  # Grab reparam params
  params <- fit_r$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  fr <- fit_r$draws(variables = vars)
  
  # Grab full params
  params <- fit_f$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  ff <- fit_f$draws(variables = vars)
  
  mplus_means <- subset(readModels(paste0("mplus/", modelName, "/model_", modelNr, ".out"), 
                                   what = "parameters")$parameters[["unstandardized"]], 
                        paramHeader == "Means")
  
  # stub to use with MPlus values
  mp <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
  # replace the values
  mp$m  <- as.numeric(mplus_means$est)         
  mp$ll <- as.numeric(mplus_means$lower_2.5ci)
  mp$hh <- as.numeric(mplus_means$upper_2.5ci)
  mp$l <- NA
  mp$h <- NA
  
  # Ground Truth
  gt <- mcmc_intervals_data(fs, point_est = "mean", prob_outer = 0.95)
  # replace the values
  gt$m  <- gamma[1:8]        
  gt$ll <- NA
  gt$hh <- NA
  gt$l <- NA
  gt$h <- NA
  gt$vline <- NA
  
  
  combined <- rbind(mcmc_intervals_data(fs, prob_outer = 0.95),
                    mcmc_intervals_data(fr, prob_outer = 0.95),
                    mcmc_intervals_data(ff, prob_outer = 0.95),
                    mp)
  
  combined <- combined %>% 
    rowwise() %>% 
    mutate(ll = ifelse(any(is.na(c(ll, m, hh))), NA, ll),
           m = ifelse(any(is.na(c(ll, m, hh))), NA, m),
           hh = ifelse(any(is.na(c(ll, m, hh))), NA, hh)) %>%
    mutate(ll = ifelse(is.na(ll), 0, ll),
           m = ifelse(is.na(m), 0, m),
           hh = ifelse(is.na(hh), 0, hh)) %>% 
    mutate(vline = ifelse(ll <= 0 && hh >= 0,
                          as.numeric(0),
                          as.numeric(NA)))
  
  combined <- rbind(combined, gt)
  
  
  if (modelNr == 1) {
    combined$parameter <- factor(combined$parameter,
                                 labels = c(expression(X),
                                            expression(Y), 
                                            expression(phi[X]), 
                                            expression(phi[Y]),
                                            expression(phi[XY]),
                                            expression(beta[YX])))
  } else {
    combined$parameter <- factor(combined$parameter,
                                 labels = c(expression(X),
                                            expression(Y), 
                                            expression(phi[X]), 
                                            expression(phi[Y]),
                                            expression(phi[XY]),
                                            expression(beta[YX]),
                                            expression(psi[X]),
                                            expression(psi[Y]))) 
  }
  
  combined$Model <- rep(c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus", "Ground Truth"), each = nrow(mp))
  combined$Model <- factor(combined$Model, levels = c("Stan Simple", "Stan Reparam", "Stan Full", "Mplus", "Ground Truth"))
  
  # theme_set(bayesplot::theme_default())
  theme_set(theme_pubr(base_size = 12))
  pos <- position_nudge(y = ifelse(combined$Model == "Stan Simple", .3,
                                   ifelse(combined$Model == "Stan Reparam", .1,
                                          ifelse(combined$Model == "Ground Truth", 0,
                                                 ifelse(combined$Model == "Stan Full", -.1,
                                                        -.3)))))
  
  my_palette <- c(brewer.pal(name="Blues",n=9)[c(4)], 
                  brewer.pal(name="Blues",n=9)[c(6)],
                  brewer.pal(name="Blues",n=9)[c(8)],
                  "#D94725",
                  'red') 
  
  gammaPlot <- function(varsNames = vars) {
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, 
                 color= c(rep("black", times = 4*nrow(gt)), rep('red', times = nrow(gt))), 
                 size = c(rep(2.5, times = 4*nrow(gt)), rep(5, times = nrow(gt))),
                 stroke = c(rep(.6, times = 4*nrow(gt)), rep(1, times = nrow(gt)))) + 
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) +
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,2,4,3)) +
      scale_y_discrete(limits=rev) +
      xlab("") +
      ylab("") +
      facet_wrap(vars(parameter),
                 ncol = 2,
                 scales = "free",
                 labeller = label_parsed,
                 strip.position = "left") +
      theme(
        # legend.key.size = unit(2, "lines"),
        legend.text = element_text(size = 14),
        strip.background = element_rect(fill="lightgrey"),
        strip.text.y.left = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      guides(shape = guide_legend(override.aes = list(
        size = 4,
        color = my_palette) # c(rep("black", times = 4),'red')
      ))
   
   }
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(gammaPlot(varsNames = vars)),
      filename = paste0("plots/", modelName, "/", modelNr, "_gamma.png"),
      width = 1200*2,
      height = 734*2.5,
      pointsize = 12,
      res = 300)
  } else {
    print(gammaPlot(varsNames = vars))
  }
}