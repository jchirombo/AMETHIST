# function to extract recruiter id in the coupon data
get_recruiter_id <- function(data){
  idx <- rep(NA,length(nrow(data)))
  for(i in 1:nrow(data)){
    j <- which(data$recruit_id[i] == data$seed_coupon1|data$recruit_id[i] == data$seed_coupon2)
    if(length(j)==0){
      idx[i] <- "Seed" # review this part,add recruiter_id to the condition
    } else {
      idx[i] <- data$pid[j]
    }
  }
  return(idx)
}

# function to generate convergence  plot
# this is an edit of the function int the RDS package
rds_convergence_plot <- function (rds.data, outcome.variable, est.func = RDS.II.estimates, 
          as.factor = FALSE, n.eval.points = 25, plot.title,max_waves, ...){
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
        theme(axis.text = element_text(size = 38),
              axis.title = element_text(size = 38))
      p <- p + geom_hline(data = datl, aes(yintercept = value,color = as.factor(Var2)), linetype = 2, alpha = 0.5)
      p
    }
    else {
      dat <- data.frame(value = e[, 1], Var1 = attr(e,"n"))
      # add wave column
      dat$W <- 0:max_waves 
      datl <- dat[nrow(dat), , drop = FALSE]
      v <- rds.data[[outcome.variable[i]]]
      rng <- if (!is.numeric(v)) c(0, 1)
      else range(v, na.rm = TRUE)
      p <- ggplot(dat) + 
        geom_line(aes(x = W, y = value),size=1.5) + # replace Var1 with W (for wave). Var1 plots number of recruitees on the x axis
        ylab(paste("Estimated", "Proportion")) + 
        xlab("Waves") + # replace xlab with waves
        labs(title = plot.title)+
        scale_y_continuous(limits = rng) +
        theme_bw() +
        theme(axis.title = element_text(size = 38),
              axis.text = element_text(size = 38),
              plot.title = element_text(size = 38))
      p <- p + geom_hline(data = datl, aes(yintercept = value),linetype = 2, alpha = 0.5)
      p
    }
    return(p)
  }
  plots <- lapply(1:length(outcome.variable), make.plot)
  do.call(gridExtra::grid.arrange, plots)
}

# function to plot recruitment trees
make_reingold_tilford_plot <- function(rds.data,stratify.var,seed=NULL){
  if(is.null(seed)){
    set.seed(9024)
  }
  reingold.tilford.plot(rds.data,
                        vertex.label.cex = 2,
                        vertex.color = stratify.var,
                        vertex.label = NA)
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

make_bottleneck_plot <- function (rds.data, outcome.variable, est.func = RDS.II.estimates, 
                                  as.factor = FALSE, n.eval.points = 25, ...){
  n <- value <- Seed <- NULL
  for (o in outcome.variable) {
    if (as.factor || is.character(rds.data[[o]])) {
      rds.data[[o]] <- as.factor(rds.data[[o]])
    }
  }
  f <- function(v, dat) {
    est <- cumulative.estimate(dat, v, est.func, n.eval.points = n.eval.points, 
                               ...)
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
          legend.position = "none")
}
