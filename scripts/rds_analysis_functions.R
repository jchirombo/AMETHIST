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
rds_convergence_plot <- function (rds.data, outcome.variable, est.func = RDS.II.estimates, 
          as.factor = FALSE, n.eval.points = 25, ...){
  if (as.factor) {
    for (o in outcome.variable) {
      rds.data[[o]] <- as.factor(rds.data[[o]])
    }
  }
  f <- function(v) cumulative.estimate(rds.data, v, est.func, 
                                       n.eval.points = n.eval.points, ...)
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
      p <- ggplot(dat) + geom_line(aes(x = Var1, color = as.factor(Var2), y = value,size=1.5)) + 
        scale_color_hue(nm) + 
        ylab("Estimate") + 
        xlab("Number of Observations") + scale_y_continuous(limits = c(0,1)) + 
        theme_bw() +
        theme(axis.text = element_text(size = 20),
              axis.title = element_text(size = 20))
      p <- p + geom_hline(data = datl, aes(yintercept = value, 
                                           color = as.factor(Var2)), linetype = 2, alpha = 0.5)
      p
    }
    else {
      dat <- data.frame(value = e[, 1], Var1 = attr(e, 
                                                    "n"))
      datl <- dat[nrow(dat), , drop = FALSE]
      v <- rds.data[[outcome.variable[i]]]
      rng <- if (!is.numeric(v)) 
        c(0, 1)
      else range(v, na.rm = TRUE)
      p <- ggplot(dat) + geom_line(aes(x = Var1, y = value)) + 
        ylab(paste("Estimated", nm)) + xlab("Number of Observations") + 
        scale_y_continuous(limits = rng) + theme_bw() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 20))
      p <- p + geom_hline(data = datl, aes(yintercept = value), 
                          linetype = 2, alpha = 0.5)
      p
    }
    return(p + ggtitle(paste("Convergence plot of", 
                             nm)))
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