# function to extract recruiter id in the coupon data
get_recruiter_id <- function(data){
  idx <- rep(NA,length(nrow(data)))
  for (i in 1:nrow(data)) {
    j <- which(data$recruit_id[i] == data$seed_coupon1 | data$recruit_id[i] == data$seed_coupon2)
    if (length(j) == 0) {
      idx[i] <- "Seed" 
    } else {
      idx[i] <- data$pid[j]
    }
  }
  return(idx)
}

plot_fsw_location <- function(plot.var=c("Institution","Label")){
  plot.var <- match.arg(plot.var)
  if (plot.var == "Institution") {
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white",lwd = 2) +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",lwd = 2,col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = c("lightsalmon","steelblue")) +
      tm_scale_bar(position = c("right","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  } else{
    nc <- n_distinct(blantyre_study_pts$Label)
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = RColorBrewer::brewer.pal(nc,"Paired")) +
      tm_scale_bar(position = c("left","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  }
}

plot_fsw_location_blantyre_urban <- function(plot.var=c("Institution","Label")){
  plot.var <- match.arg(plot.var)
  if (plot.var == "Institution") {
    tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white",lwd = 2) +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = c("lightsalmon","steelblue")) +
      tm_scale_bar(position = c("left","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  } else{
    nc <- n_distinct(blantyre_study_pts$Label)
    tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = RColorBrewer::brewer.pal(nc,"Paired")) +
      tm_scale_bar(position = c("left","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.text.size = 1.5,
                legend.title.size = 1.8)
  }
}


# function to plot the number of recruits by wave
plot_recruitment_wave <- function(summary.data,legend.title,outcome.var = c("HIV","Syphilis")){
  outcome.var <- match.arg(outcome.var)
  if (outcome.var == "HIV") {
    p <- ggplot(summary.data,aes(x = wave,y = nrecruit,colour = hivstatus)) +
      geom_line(size = 1.5) +
      theme_bw() +
      scale_colour_manual(values = c("#f0b27a","#84e9d4")) +
      scale_x_continuous(breaks = c(seq(0,7,1))) +
      labs(x = "Wave",y = "Number of recruits",title = "Recruits by wave",colour = legend.title,caption = "There were 7 recruitment waves") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = c(0.1,0.93),
            plot.title = element_text(size = 22),
            plot.caption = element_text(size = 18))
    return(p)
  }else{
    p <- ggplot(summary.data,aes(x = wave,y = nrecruit,colour = syphstatus)) +
      geom_line(size = 1.5) +
      theme_bw() +
      scale_colour_manual(values = c("#f0b27a","#84e9d4")) +
      scale_x_continuous(breaks = c(seq(0,7,1))) +
      labs(x = "Wave",y = "Number of recruits",title = "Recruits by wave",colour = legend.title,caption = "There were 7 recruitment waves") +
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
rds_convergence_plot <- function(rds.data, outcome.variable, est.func = RDS.II.estimates, 
          as.factor = FALSE, n.eval.points = 25, plot.title, max.waves, ...){
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
        geom_line(aes(x = Var1, color = as.factor(Var2), y = value),size = 1.5) + 
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
        geom_line(aes(x = W, y = value),size = 1.5) + # replace Var1 with W (for wave). Var1 plots number of recruitees on the x axis
        ylab(paste("Estimated","Proportion",sep = " ")) + 
        xlab("Wave") + # replace xlab with waves
        labs(title = paste("Convergence plot for",plot.title,sep = " ")) +
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

# modified reingold.tilford.plot for producing recruitment trees
# the function has been modified to change the location of the legend
reingold_tilford_plot <- function(x, vertex.color = NULL, vertex.color.scale = hue_pal(), 
          vertex.size = 2, vertex.size.range = c(1, 5), edge.arrow.size = 0, 
          vertex.label.cex = 0.2, vertex.frame.color = NA, vertex.label = get.id(x), 
          show.legend = TRUE, plot = TRUE, ...) 
{
  x <- as.rds.data.frame(x)
  if (!is.null(vertex.color)) {
    color.name <- vertex.color
    color.var <- factor(x[[vertex.color]])
    levs <- levels(color.var)
    ncol <- length(levs)
    cols <- vertex.color.scale(ncol)
    color <- cols[as.integer(color.var)]
  }
  else color <- NULL
  if (is.character(vertex.size)) {
    vertex.size.name <- vertex.size
    vertex.size <- as.numeric(x[[vertex.size]])
  }
  else {
    vertex.size.name <- deparse(substitute(vertex.size))
  }
  if (length(vertex.size) > 1) {
    vrange <- range(vertex.size, na.rm = TRUE)
    vertex.size <- vertex.size + vrange[1]
    vertex.size <- vertex.size/vrange[2]
    vertex.size <- vertex.size * (vertex.size.range[2] - 
                                    vertex.size.range[1]) + vertex.size.range[1]
    vertex.size[is.na(vertex.size)] <- 0
  }
  if (length(vertex.label) == 1 && !is.na(vertex.label)) {
    vertex.label <- as.character(x[[vertex.label]])
  }
  id <- get.id(x)
  rid <- get.rid(x)
  sid <- get.seed.rid(x)
  seeds <- get.seed.id(x)
  el <- cbind(rid, id, seeds)
  el <- el[rid != sid, ]
  el <- el[order(el[, 3]), , drop = FALSE]
  xyl <- list()
  grl <- list()
  for (seed in unique(seeds)) {
    els <- el[el[, 3] == seed, , drop = FALSE]
    if (nrow(els) > 0) {
      gr <- igraph::graph.edgelist(els[, 1:2, drop = FALSE])
      lo <- igraph::layout.reingold.tilford(gr, root = seed)
      tmp <- lo
      tmp[, 1] <- round(lo[, 1]/0.25)
      overplt <- duplicated(tmp)
      if (any(overplt)) {
        lo[overplt, 2] <- lo[overplt, 2] - 0.5
      }
    }
    else if (nrow(els) == 1) {
      gr <- igraph::graph.edgelist(els[, 1:2, drop = FALSE])
      lo <- matrix(c(1, 0, 0, 1), ncol = 2)
    }
    else {
      gr <- igraph::graph.empty() + seed
      lo <- matrix(c(0, 0), ncol = 2)
    }
    if (!is.null(color)) {
      i <- match(igraph::V(gr)$name, id)
      igraph::V(gr)$color <- color[i]
    }
    if (length(vertex.size) > 1) {
      i <- match(igraph::V(gr)$name, id)
      igraph::V(gr)$size <- vertex.size[i]
    }
    if (length(vertex.label) > 1) {
      i <- match(igraph::V(gr)$name, id)
      igraph::V(gr)$label <- vertex.label[i]
    }
    xyl[[seed]] <- lo
    grl[[seed]] <- gr
  }
  last <- 1
  overlap <- function(x1, y1, sw1, sh1, boxes) {
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    last <<- 1
    FALSE
  }
  ord <- order(sapply(xyl, nrow), decreasing = TRUE)
  tstep = 0.1
  rstep = 0.1
  boxes <- list()
  xyl2 <- xyl
  for (ind in 1:length(xyl)) {
    i <- ord[ind]
    r <- 0
    theta <- stats::runif(1, 0, 2 * pi)
    x1 <- xo <- 0
    y1 <- yo <- 0
    wid <- diff(range(xyl[[i]][, 1], na.rm = TRUE)) * 1.1
    sdx <- 1
    ht <- diff(range(xyl[[i]][, 2], na.rm = TRUE)) * 1.1
    sdy <- 1
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, 
                   ht, boxes)) {
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        theta <- theta + tstep
        r <- r + rstep * tstep/(2 * pi)
        x1 <- xo + sdx * r * cos(theta)
        y1 <- yo + sdy * r * sin(theta)
      }
    }
    xyl2[[i]][, 1] <- xyl2[[i]][, 1] + x1 - wid * 0.45 - 
      min(xyl2[[i]][, 1])
    xyl2[[i]][, 2] <- xyl2[[i]][, 2] + y1 - ht * 0.45 - min(xyl2[[i]][, 
                                                                      2])
  }
  t <- do.call(rbind, xyl2)
  if (plot) {
    igr <- igraph::graph.disjoint.union(grl)
    nm <- do.call(c, lapply(grl, function(a) igraph::V(a)$name))
    vcol <- do.call(c, lapply(grl, function(a) igraph::V(a)$color))
    if (length(vertex.size) > 1) 
      vsize <- do.call(c, lapply(grl, function(a) igraph::V(a)$size))
    else vsize <- vertex.size
    if (length(vertex.label) > 1) {
      vlab <- do.call(c, lapply(grl, function(a) igraph::V(a)$label))
    }
    else vlab <- vertex.label
    igraph::V(igr)$name <- nm
    if (is.null(color)) {
      igraph::plot.igraph(igr, layout = t, vertex.size = vsize, 
                          edge.arrow.size = edge.arrow.size, vertex.label.cex = vertex.label.cex, 
                          vertex.frame.color = vertex.frame.color, vertex.label = vlab, 
                          ...)
    }
    else {
      igraph::plot.igraph(igr, layout = t, vertex.size = vsize, 
                          edge.arrow.size = edge.arrow.size, vertex.label.cex = vertex.label.cex, 
                          vertex.frame.color = vertex.frame.color, vertex.color = vcol, 
                          vertex.label = vlab, ...)
      if (show.legend) 
        graphics::legend("topright", legend = levs, 
                         col = cols, pch = 16, title = color.name, horiz = FALSE, 
                         box.col = NA)
    }
    if (length(vsize) > 1 && show.legend) {
      levs <- paste(vrange, " ")
      s <- vertex.size.range
      lg <- graphics::legend("topleft", legend = levs, 
                             pt.cex = c(2,2),cex = 2, pch = 16, title = vertex.size.name, 
                             horiz = FALSE, box.col = NA, x.intersp = 2)
      t <- lg$text
      s <- s/200
      graphics::symbols(x = t$x - 0.075, y = t$y, circles = s, 
                        add = TRUE, inches = FALSE, bg = "SkyBlue2")
    }
  }
  invisible(t)
}

# function to plot recruitment trees
generate_recruitment_tree <- function(rds.data,stratify.var,label.var,seed = NULL){
  if (is.null(seed)) {
    set.seed(9024)
  }
  reingold.tilford.plot(rds.data,
                        vertex.label.cex = 2,
                        vertex.color = stratify.var,
                        vertex.label = NA,
                        main = paste("Recruitment tree by",label.var,sep = " "))
}

## another function for recruitment trees

generate_recruitment_tree_plot <- function(rds.data,stratify.var,label.var,seed = NULL){
  if (is.null(seed)) {
    set.seed(9024)
  }
  reingold_tilford_plot(rds.data,
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
get_homophily_estimates <- function(rds.data,outcome.var,estim.type = c("RDS-I","RDS-II"), recruitment = TRUE){
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


# function for plottig random effects
# https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=0.9), colour = "steelblue") 
    return(p)
  }
  
  lapply(re, f)
}

for (i in 1:nrow(U)) {
}

# function to create a symmetric recruitment matrix
recruit_id <- rdsfinal$recruit_id
pid <- rdsfinal$pid

U <- matrix(0, nrow = length(pid), ncol = length(pid), dimnames = list(c(pid), c(recruit_id)))
dimnames(U)
for (k in 1:dim(U)[1]) {
  if (recruit_id[k] == U[,k][k]) {
    U[,k] <- 1
  }
  #j <- which(recruit_id[k] %in% names[U[k,]])
}
