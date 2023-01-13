# function to extract recruiter id in the coupon data
get_recruiter_id <- function(data){
  idx <- rep(NA,length(nrow(data)))
  for(i in 1:nrow(data)){
    j <- which(data$recruit_id[i] == data$seed_coupon1|data$recruit_id[i] == data$seed_coupon2)
    if(length(j)==0){
      idx[i] <- "Seed" 
    } else {
      idx[i] <- data$pid[j]
    }
  }
  return(idx)
}

plot_fsw_location <- function(plot.var=c("Institution","Label")){
  plot.var <- match.arg(plot.var)
  if(plot.var == "Institution"){
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white",lwd = 2) +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",lwd = 2,col = "white") +
      tm_shape(blantyre_pts) +
      tm_dots(plot.var,size = 0.8,palette = c("lightsalmon","steelblue"))+
      tm_scale_bar(position = c("right","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  } else{
    nc <- 9
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_pts) +
      tm_dots(plot.var,size = 0.8,palette = RColorBrewer::brewer.pal(nc,"Set1")) +
      tm_scale_bar(position = c("left","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  }
}

# function to plot the number of recruits by wave
plot_recruitment_wave <- function(summary.data,legend.title,outcome.var=c("HIV","Syphilis")){
  outcome.var <- match.arg(outcome.var)
  if(outcome.var=="HIV"){
    p <- ggplot(summary.data,aes(x=wave,y=nrecruit,colour=hivstatus)) +
      geom_line(size=1.5) +
      theme_bw()+
      scale_colour_manual(values = c("#f0b27a","#84e9d4"))+
      scale_x_continuous(breaks = c(seq(0,7,1)))+
      labs(x="Wave",y="Number of recruits",title = "Recruits by wave",colour=legend.title,caption = "There were 7 recruitment waves") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = c(0.1,0.93),
            plot.title = element_text(size = 22),
            plot.caption = element_text(size = 18))
    return(p)
  }else{
    p <- ggplot(summary.data,aes(x=wave,y=nrecruit,colour=syphstatus)) +
      geom_line(size=1.5) +
      theme_bw()+
      scale_colour_manual(values = c("#f0b27a","#84e9d4"))+
      scale_x_continuous(breaks = c(seq(0,7,1)))+
      labs(x="Wave",y="Number of recruits",title = "Recruits by wave",colour=legend.title,caption = "There were 7 recruitment waves") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = c(0.1,0.93),
            plot.title = element_text(size = 22),
            plot.caption = element_text(size = 18))
  }
  return(p)
}

# function to generate convergence  plot
# this is an edit of the function in the RDS package
rds_convergence_plot <- function (rds.data, outcome.variable, est.func = RDS.II.estimates, 
          as.factor = FALSE, n.eval.points = 25, plot.title,max.waves, ...){
  if (as.factor) {
    for (o in outcome.variable) {
      rds.data[[o]] <- as.factor(rds.data[[o]])
    }
  }
  f <- function(v) cumulative.estimate(rds.data, v, est.func,n.eval.points = n.eval.points, ...)
  ests <- lapply(outcome.variable, f)
  make.plot <- function(i) {
    Var1 <- Var2 <- value <- NULL
    e <- ests[[i]]
    nm <- outcome.variable[i]
    if (ncol(e) == 2) {
      e1 <- e[, 2, drop = FALSE]
      attr(e1, "n") <- attr(e, "n")
      e <- e1
      nm <- paste0(outcome.variable[i], "=", colnames(e)[1])
      rds.data[[outcome.variable[i]]] <- as.factor(rds.data[[outcome.variable[i]]])
    }
    if (ncol(e) > 1) {
      rownames(e) <- attr(e, "n")
      dat <- melt(e)
      datl <- melt(e[nrow(e), , drop = FALSE])
      p <- ggplot(dat) + 
        geom_line(aes(x = Var1, color = as.factor(Var2), y = value),size=1.5) + 
        scale_color_hue(nm) + 
        ylab("Estimate") + 
        xlab("Waves") + # replace with waves
        scale_y_continuous(limits = c(0,1)) + 
        theme_bw() +
        theme(axis.text = element_text(size = 22),
              axis.title = element_text(size = 22))
      p <- p + geom_hline(data = datl, aes(yintercept = value,color = as.factor(Var2)), linetype = 2, alpha = 0.5)
      p
    }
    else {
      dat <- data.frame(value = e[, 1], Var1 = attr(e,"n"))
      # add wave column
      dat$W <- NA
      dat$W <- 0:max.waves 
      datl <- dat[nrow(dat), , drop = FALSE]
      v <- rds.data[[outcome.variable[i]]]
      rng <- if (!is.numeric(v)) c(0, 1)
      else range(v, na.rm = TRUE)
      p <- ggplot(dat) + 
        geom_line(aes(x = W, y = value),size=1.5) + # replace Var1 with W (for wave). Var1 plots number of recruitees on the x axis
        ylab(paste("Estimated","Proportion",sep = " ")) + 
        xlab("Wave") + # replace xlab with waves
        labs(title = paste("Convergence plot for",plot.title,sep = " "))+
        scale_y_continuous(limits = rng) +
        scale_x_continuous(breaks = seq(0,max.waves,1)) +
        theme_bw() +
        theme(axis.title = element_text(size = 22),
              axis.text = element_text(size = 22),
              plot.title = element_text(size = 23))
      p <- p + geom_hline(data = datl, aes(yintercept = value),linetype = 2, alpha = 0.5)
      p
    }
    return(p)
  }
  plots <- lapply(1:length(outcome.variable), make.plot)
  do.call(gridExtra::grid.arrange, plots)
}

# function to plot recruitment trees
generate_recruitment_tree <- function(rds.data,stratify.var,label.var,seed=NULL){
  if(is.null(seed)){
    set.seed(9024)
  }
  reingold.tilford.plot(rds.data,
                        vertex.label.cex = 2,
                        vertex.color = stratify.var,
                        vertex.label = NA,
                        main = paste("Recruitment tree by",label.var,sep = " "))
}

# function to create contingency table and perform test of association 
make_bootstrap_contingency_test <- function(rds_data,row_var,col_var,nsim){
  car_test <- bootstrap.contingency.test(datRDS,
                                         row.var = row_var,
                                         col.var = col_var,
                                         number.of.bootstrap.samples = nsim,
                                         weight.type = "RDS-II",
                                         verbose = T)
  return(car_test)
}

# function to estimate recruitment and population homophily
get_homophily_estimates <- function(rds.data,outcome.var,estim.type=c("RDS-I","RDS-II"),recruitment=T){
  homophily.estimates(rds.data,
                      outcome.variable = outcome.var,
                      recruitment = recruitment,
                      weight.type = estim.type,
                      N = 1000)
}

# function to generate bottleneck plot
# adapted from the RDS package function bottleneck.plot()

make_bottleneck_plot <- function(rds.data, outcome.variable, est.func = RDS.II.estimates, as.factor = FALSE, n.eval.points = 25, ...){
  n <- value <- Seed <- NULL
  for (o in outcome.variable) {
    if (as.factor || is.character(rds.data[[o]])) {
      rds.data[[o]] <- as.factor(rds.data[[o]])
    }
  }
  f <- function(v, dat) {
    est <- cumulative.estimate(dat, v, est.func, n.eval.points = n.eval.points,...)
    n <- attr(est, "n")
    if (ncol(est) == 1) {
      colnames(est) <- v
      rownames(est) <- n
      est <- list(est)
    }
    else if (ncol(est) == 2) {
      nl <- colnames(est)[2]
      est <- est[, 2, drop = FALSE]
      attr(est, "n") <- n
      colnames(est) <- paste0(v, "=", nl)
      est <- list(est)
    }
    else {
      est <- lapply(1:ncol(est), function(i) {
        e <- est[, i, drop = FALSE]
        nl <- colnames(e)
        attr(e, "n") <- n
        colnames(e) <- paste0(v, "=", nl)
        e
      })
    }
    est
  }
  seeds <- get.seed.id(rds.data)
  sids <- unique(seeds)
  ls <- list()
  for (i in 1:length(sids)) {
    #print(sids[i])
    dat <- rds.data[seeds == sids[i], ]
    if (nrow(dat) == 0) 
      next
    res <- NULL
    nres <- NULL
    for (v in outcome.variable) {
      tmp <- f(v, dat)
      for (j in 1:length(tmp)) {
        res <- cbind(res, tmp[[j]])
        nres <- cbind(nres, attr(tmp[[j]], "n"))
      }
    }
    res <- reshape2::melt(res)[-1]
    res$n <- reshape2::melt(nres)[[3]]
    res$seed <- sids[i]
    ls[[i]] <- res
    names(ls)[i] <- sids[i]
  }
  result <- Reduce(function(a, b) {
    if (is.null(a)) 
      return(b)
    if (is.null(b)) 
      return(a)
    rbind(a, b)
  }, ls, init = NULL)
  result$Seed <- as.factor(result$seed)
  #result$wave <- 0:7 # add wave to the data
  ggplot(result, aes(x = n, y = value, color = Seed)) + 
    geom_line(size=1.5) + 
    #facet_wrap(~Var2, scales = "free_y") + 
    theme_bw() + 
    ylab("Estimate") + 
    scale_colour_manual(values = c("#c6ead9","#baa9aa","#c27ba0","#c5c3a1","#d26666","#febe9f"))+
    xlab("Observations") +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 21),
          legend.position = "bottom")
}

# new function for convergence

plot_rds_estim_convergence <- function(rds.data,outcome.variable,plot.type=c("ggplot","base")){
  estim_mat <- RDS::cumulative.estimate(rds.data,outcome.variable)
  estim_df <- as.data.frame(estim_mat)
  estim_df <- dplyr::mutate(estim_df,Wave=0:(nrow(estim_df)-1))
  plot.type <- match.arg(plot.type)
  if(plot.type == "ggplot"){
    p <- ggplot2::ggplot(estim_df,aes(x=Wave,y=Positive)) +
      geom_line(size=1.6) +
      theme_bw() +
      ylab("Prevalence") +
      ylim(0,1) +
      scale_x_continuous(breaks = seq(0,7,1)) +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20))
    return(p)
  } else {
    par(cex.axis = 1.4, cex.lab = 1.4)
    plot(estim_df$Wave,estim_df$Positive,type = "l",lwd = 2,ylim = c(0,1))
  }
}
