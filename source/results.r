rhat_comp_F_MP <- function(savePlots = TRUE, mplusDraws, mplusOut, fit) {
  
  mplus_draws <- read.table(mplusDraws, header=F)
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
              readLines(mplusOut), 
              value = TRUE),
         23, 30)
  
  mplus_time <- as.POSIXlt(paste(Sys.Date(), extract_time))
  mplus_time <- if (mplus_time$sec/60 >= .5) {
    1 + mplus_time$hour*60 + mplus_time$min
  } else {
    mplus_time$hour*60 + mplus_time$min
  }
    
  
  a = fit$summary()$rhat
  b = save_rhat[,1]
  a_name = "Stan"
  b_name = "Mplus"
  
  data <- rbind(data.frame(rhat = a, model = a_name, rating = NA),
                data.frame(rhat = b, model = b_name, rating = NA)
                )
  
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
  
  data <- data.frame(model = rep(c(a_name, b_name), each = 4),
                     rating = c("<1.01", "<1.05", "<1.10", ">1.10"))
  
  data <- merge(data, count, by = c("model", "rating"), all = T)
  
  
  data$rating <- factor(data$rating,
                        levels = c("<1.01", "<1.05", "<1.10", ">1.10"))
  data$model <- factor(data$model,
                       levels = c(a_name, b_name))
  
  model_names <- c(
    `Stan` = paste("Stan (runtime ~ ",round(mean(fit$metadata()$time$total) / 60, 0), "min)"),
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
               nrow = 1,
               scales = "free_x",
               labeller = as_labeller(model_names))
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(rhatPlot),
      filename = paste0("plots/", modelName, "/", modelName, "rhat.png"),
      width = 1200*2,
      height = 734*2.5,
      pointsize = 12,
      res = 300)
    } else {
      print(rhatPlot)
    }
}

est_comp_F_MP <- function(savePlots = TRUE, mplusOut, fit) {
  ### Estimates - Full and MPlus
  
  gammaPlot <- function(varsNames = c("X", "Y", "phi_X", "phi_Y", "phi_XY", "beta_YX", "psi_X", "psi_Y")) {
    
    # stub to use with MPlus values
    mp <- mcmc_intervals_data(ff, point_est = "mean", prob_outer = 0.95)[1:length(gamma),]
    
    # replace the values
    mp$m  <- as.numeric(mplus_means$est)[1:length(gamma)]         
    mp$ll <- as.numeric(mplus_means$lower_2.5ci)[1:length(gamma)]
    mp$hh <- as.numeric(mplus_means$upper_2.5ci)[1:length(gamma)]
    mp$l <- NA
    mp$h <- NA
    
    # Ground Truth
    gt <- mcmc_intervals_data(ff, point_est = "mean", prob_outer = 0.95)
    # replace the values
    gt$m  <- gamma[1:length(gamma)]
    gt$ll <- NA
    gt$hh <- NA
    gt$l <- NA
    gt$h <- NA
    gt$vline <- NA

    combined <- rbind(mcmc_intervals_data(ff, prob_outer = 0.95)[1:length(gamma),], mp)
    
    
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
    
    combined$Model <- rep(c("Stan", "Mplus", "Ground Truth"), each = nrow(mp))
    combined$Model <- factor(combined$Model, levels = c("Stan", "Mplus", "Ground Truth"))
    combined$parameter <- factor(combined$parameter,
                                 labels = c(expression(X),
                                            expression(Y), 
                                            expression(phi[X]), 
                                            expression(phi[Y]),
                                            expression(phi[XY]),
                                            expression(beta[YX]),
                                            expression(psi[X]),
                                            expression(psi[Y]))[1:length(gamma)])
    
    # theme_set(bayesplot::theme_default())
    theme_set(theme_pubr(base_size = 12))
    pos <- position_nudge(y = ifelse(combined$Model == "Mplus", -.2,
                                     ifelse(combined$Model == "Ground Truth", 0,
                                              .2)))
    
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(8)],
                    brewer.pal(name="Greens",n=9)[c(8)],
                    "#D94725") 
    
    ggplot(combined, aes(x = m, y = parameter, color = Model, shape = Model)) +
      geom_point(position = pos, size = 2.5, stroke = .6) +
      geom_errorbar(
        aes(xmin = as.numeric(ll), xmax = as.numeric(hh)),
        width=0.1 , linewidth = .6,
        position=pos) +
      geom_vline(aes(xintercept = vline), color = "grey", linetype = "dashed") +
      scale_color_manual(name = "", values = my_palette) +
      scale_shape_manual(name = "", values = c(0,1,8)) +
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
  mplus_means <- subset(readModels(mplusOut, what = "parameters")$parameters[["unstandardized"]], 
                        paramHeader == "Means" | paramHeader == "Intercepts" & param %in% toupper(varsNames))
  
  # Grab full params
  params <- fit$metadata()$model_params
  vars <- grep('gamma\\[', params, value = TRUE)
  ff <- fit$draws(variables = vars)
  
  
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

est_100_comb <- function(savePlots = TRUE) {
  
  gammaPlot <- function() {
    
    simM$Model <- factor(simM$Model, levels = c("Stan", "Mplus", "Ground Truth"))
    simM$parameter <- factor(simM$parameter,
                             levels = c("time", "rhat", varsNames[1:length(gamma)]),
                             labels = c(expression(t),
                                        expression(hat(R)),
                                        expression(X),
                                        expression(Y), 
                                        expression(phi[X]), 
                                        expression(phi[Y]),
                                        expression(phi[XY]),
                                        expression(beta[YX]),
                                        expression(psi[X]),
                                        expression(psi[Y]))[1:(length(gamma)+2)])
    
    # theme_set(bayesplot::theme_default())
    theme_set(theme_pubr(base_size = 12))
    
    my_palette <- c(brewer.pal(name="Blues",n=9)[c(9)],
                    brewer.pal(name="Greens",n=9)[c(3)],
                    "#D94725") 
    
    ggplot(data = subset(simM, Model != "Ground Truth" & parameter %out% c("t", "hat(R)")), aes(x = m, fill = Model)) + 
      geom_density(alpha = 0.5) +
      geom_vline(data = subset(simM, Model == "Ground Truth" & parameter %out% c("t", "hat(R)")),
                 aes(xintercept = m, color = "Ground Truth"), linetype = "solid", size = 1) +
      scale_fill_manual(values = my_palette) +  # Customize colors if needed
      scale_color_manual(name = "", values = c("Ground Truth" = my_palette[3])) +
      labs(title = paste0("DSEM Parameter Recovery for ", modelName),
           x = "",
           y = "") +
      theme_minimal() +
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
            axis.ticks.y = element_blank())
    }
  
  
  if (savePlots == TRUE) {
    dir.create(paste0("plots/", modelName, "/"), recursive = TRUE, showWarnings = FALSE)
    ggexport(
      print(gammaPlot()),
      filename = paste0("plots/", modelName, "/", modelNr, "_sim100FMP_gamma.png"),
      width = 1200*2,
      height = ifelse(modelNr == 1, (734*2.5)/5, 734*2.5),
      pointsize = 12,
      res = 300
    )} else {
      print(gammaPlot())
    }
}
