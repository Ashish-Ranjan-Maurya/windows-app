options(digits = 6)
options(scipen = 500)

# new mod
ct.to.conc <- function(ct, bottom, top, ec50, slope, asym = 1.0)
{
  return(2^(ec50 - (log2(((top - bottom)/(ct - bottom)) ^ (1/asym) - 1)/slope)))
}


limit.of.detection.1 <- function(data.final, bottom, top, ec50, slope, asym)
{
  lod.conc <- NA
  
  if(dim(data.final[data.final[, "Task"] == "NTC", ])[1] > 1){
    mm <- mean(data.final[data.final[, "Task"] == "NTC", "delta.Ct"], na.rm = T)
    ss <- sd(data.final[data.final[, "Task"] == "NTC", "delta.Ct"], na.rm = T)
    
    lod.Ct <- 
      if(mm - 2 * ss < top){
        mm - 2 * ss
      } else if (mm - 3 * ss < top){
        mm - 3 * ss
      } else if (mm - 4 * ss < top){
        mm - 4 * ss
      } else{
        top - 0.01
      }
    
    lod.conc <- ct.to.conc(lod.Ct, bottom, top, ec50, slope, asym)
  }
  
  return(lod.conc)
}
# new mod. end

# M-statistics algorithm
# reference: The Analysis of Protein Arrays_Brad Love_Mstats.pdf, IRBP_Toolbox_v5.2_Manual.pdf
M.statistics.fn <- function(in.mat, group.rows, lod, fold.thresh)
{
  grp1      <- group.rows[[1]]
  grp2      <- group.rows[[2]]
  grp.names <- names(group.rows)
  
  M.stat <- function(in.vec)
  {
    g1.vec     <- sort(in.vec[grp1], decreasing = T)
    g2.vec     <- sort(in.vec[grp2], decreasing = T)
    above      <- lod                # minimum concentration threshold
    g1.above   <- (g1.vec > above)   # True/False
    g2.above   <- (g2.vec > above)   # True/False
    n1         <- length(g1.vec)
    n2         <- length(g2.vec)
    
    stat.list  <- list(NULL, NULL)
    best       <- list(g = 1, p = 1, i = 1)
    
    # grp1 ref to grp2, counts number of signals in group 1 above reference signal in group 2, then
    # calculates probability of that count (M statistic) occuring randomly according to the hypergeometric dist.
    
    for(i in 1:n2){
      cutoff   <- g2.vec[i] * fold.thresh   # determines reference signal (i.e. what order from group 2)
      
      g1.count <- sum(g1.vec[g1.above] >= cutoff)
      g2.count <- sum(g2.vec[g2.above] >= cutoff)
      p.val    <- phyper(g1.count - 1, n1, n2, g1.count + i - 1, lower.tail = F)
      #print(p.val)
      
      stat.list[[2]][[i]] <- c(g1.count, g2.count, p.val, cutoff)
      if(p.val < best$p){
        best <- list(g = 2, p = p.val, i = i)
      }
    }
    
    # grp2 ref to grp1, counts number of signals in group 1 above reference signal in group 2, then
    # calculates probability of that count (M statistic) occuring randomly according to the hypergeometric dist.
    
    for(i in 1:n1){
      cutoff   <- g1.vec[i] * fold.thresh  # determines reference signal (i.e. what order from group 1)
      
      g1.count <- sum(g1.vec[g1.above] >= cutoff)
      g2.count <- sum(g2.vec[g2.above] >= cutoff)
      p.val    <- phyper(g2.count - 1, n2, n1, i - 1 + g2.count, lower.tail = F)
      #print(p.val)
      
      stat.list[[1]][[i]] <- c(g1.count, g2.count, p.val, cutoff)
      if(p.val < best$p){
        best <- list(g = 1, p = p.val, i = i)
      }
    }
    
    c(stat.list[[best$g]][[best$i]], round(mean(g1.vec), 0), round(mean(g2.vec), 0), n1, n2)
  }
  
  out <- apply(in.mat, 2, M.stat)
  #out <- M.stat(in.mat$sample.conc.df.measured.concentration)
  
  prevalence <- rbind(round((out[1] + 1)/(out[7] + 2), 3), round((out[2] + 1)/(out[8] + 2), 3))
  up.in.g1   <- as.numeric(prevalence[1] > prevalence[2])
  up.in.g2   <- as.numeric(prevalence[1] < prevalence[2])
  
  ms.out     <- matrix(c(out[7:8], out[1:2], prevalence, up.in.g1, up.in.g2, out[5:6], 
                         ifelse(up.in.g1 == 1, round(out[5] - out[6], 0), round(out[6] - out[5], 0)), 
                         ifelse(up.in.g1 == 1, round(out[5]/out[6], 1), round(out[6]/out[5], 1)), 
                         out[3], round(out[4], 0)))
  
  rownames(ms.out) <- c(paste(grp.names, "Size"), paste(grp.names, "Mstat-Count"), paste(grp.names, "Prevalence"), 
                        paste("More Prev. in", grp.names), paste(grp.names, "Mean"), 
                        "Mean Difference", "Mean Ratio", "P-Value", "Cutoff")
  ms.out
}


compare2Groups <- function(sample.conc.df.agg.2, group1, group2)
{
  grp.rows  <- list(grp1 = as.numeric(rownames(sample.conc.df.agg.2[sample.conc.df.agg.2[,"Biogroup.Name"] == group1,])),
                    grp2 = as.numeric(rownames(sample.conc.df.agg.2[sample.conc.df.agg.2[,"Biogroup.Name"] == group2,])))
  
  mstat.results <- tryCatch({
    #results <- as.data.frame(M.statistics.fn(data.frame(sample.conc.df.agg.2$serum.conc), grp.rows, lod = results$LOD.conc, fold.thresh = 1.5))
    
    se.df   <- data.frame(sample.conc.df.agg.2$serum.conc)
    ms      <- M.statistics.fn(se.df, grp.rows, lod = results$LOD.conc, fold.thresh = 1.0)
    results <- as.data.frame(ms)
    
    #return(mstat.results)
    }, warning = function(war){
      # warning handler picks up where error was generated
      print(paste("M.statistics Warning: ", war))
      return(NA)
      }, error = function(err){
        # error handler picks up where error was generated
        print(paste("M.statistics Error: ", err))
        return(NA)
        }
  ) # END tryCatch
}


GroupComparison <- function(sample.conc.df.agg.2)
{
  GroupsPresent <- sort(as.vector(unique(sample.conc.df.agg.2$Biogroup.Name)))
  groupComparisonResults <- list()
  groupComparisonResults[["mstat.results.grps12"]] <- NA
  groupComparisonResults[["mstat.results.grps23"]] <- NA
  groupComparisonResults[["mstat.results.grps13"]] <- NA
  
  if(length(GroupsPresent) == 1){
    print("Only one group present, so no group comparison")
  } else if(length(GroupsPresent) == 3){
    mstat.results.grps12 <- compare2Groups(sample.conc.df.agg.2, GroupsPresent[1], GroupsPresent[2])
    print(mstat.results.grps12)
    mstat.results.grps23 <- compare2Groups(sample.conc.df.agg.2, GroupsPresent[2], GroupsPresent[3])
    print(mstat.results.grps23)
    mstat.results.grps13 <- compare2Groups(sample.conc.df.agg.2, GroupsPresent[1], GroupsPresent[3])
    print(mstat.results.grps13)
    
    groupComparisonResults[["mstat.results.grps12"]] <- mstat.results.grps12
    groupComparisonResults[["mstat.results.grps23"]] <- mstat.results.grps23
    groupComparisonResults[["mstat.results.grps13"]] <- mstat.results.grps13
    
  } else if(length(GroupsPresent) == 2){
    #print("masn: 2 groups detected.")
    mstat.results.grps <- compare2Groups(sample.conc.df.agg.2, GroupsPresent[1], GroupsPresent[2])
    print(mstat.results.grps)
    if("UNKNOWN" %in% GroupsPresent){
      if("UNKNOWN-2" %in% GroupsPresent){
        groupComparisonResults[["mstat.results.grps12"]] <- mstat.results.grps
      } else{
        groupComparisonResults[["mstat.results.grps13"]] <- mstat.results.grps
      }
    } else{
        groupComparisonResults[["mstat.results.grps23"]] <- mstat.results.grps
    }
  }
  
  return(groupComparisonResults)
}


parse.row.wise <- function(ct.values)
{
  rows <- LETTERS[seq(from = 1, to = 8)]
  cols <- c(1:12)
  well.addresses <- vector()
  
  i <- 1
  for(row in rows){
    for(col in cols){
      address = paste0(row, col)
      #print(address)
      well.addresses[i] <- as.character(address)
      i <- i + 1
    }
  }
  
  # if(do.transpose){
  #   ct.values = t.data.frame(ct.input)[,1]
  #   ct.results = data.frame(ct.values, well.addresses, stringsAsFactors = F)
  #   
  # }else {
  #   ct.values = ct.input[,1]
  #   ct.results = data.frame(ct.values, well.addresses, stringsAsFactors = F)
  # }
  
  ct.results <- data.frame(ct.values, well.addresses, stringsAsFactors = F)
  colnames(ct.results) <- c("Ct", "Well")
  
  return(ct.results)
}


parse.col.wise <- function(ct.values)
{
  rows <- LETTERS[seq(from = 1, to = 8)]
  cols <- c(1:12)
  well.addresses <- vector()
  
  i <- 1
  for(col in cols){
    for(row in rows){
      address <- paste0(row, col)
      #print(address)
      well.addresses[i] <- address
      i <- i + 1
    }
  }
  
  # if(do.transpose){
  #   ct.values = t.data.frame(ct.input)[,1]
  #   ct.results = data.frame(ct.values, well.addresses, stringsAsFactors = F)
  # }else {
  #   ct.values = ct.input[,1]
  #   ct.results = data.frame(ct.values, well.addresses, stringsAsFactors = F)
  # }
  ct.results <- data.frame(ct.values, well.addresses, stringsAsFactors = F)
  colnames(ct.results) <- c("Ct", "Well")
  
  return(ct.results)  
}


parse.row.col.mix <- function(ct.values)
{
  rows <- LETTERS[seq(from = 1, to = 8)]
  cols <- c(1:12)
  well.addresses <- vector()
  
  i <- 1
  for(row in rows){
    for(col in cols){
      address = paste0(row,col)
      #print(address)
      well.addresses[i] <- address
      i <- i + 1
    }
  }
  
  ct.results <- data.frame(ct.values, well.addresses, stringsAsFactors = F)
  colnames(ct.results) <- c("Ct", "Well")
  
  return(ct.results)
}


logistic.4PL <- function(a, d, c, b, f, X) 
{
  yfit <- a + (d - a)/(1 + 2^((c - X) * b))
  
  return(yfit)
}


logistic.5PL <- function(a, d, c, b, f, X) 
{
  yfit <- a + (d - a)/(1 + 2^((c - X) * b))^f
  
  return(yfit)
}


logisticModel.lma <- function(npars) 
{
  switch(as.character(npars),
         `4` = { nPL <- logistic.4PL }, 
         `5` = { nPL <- logistic.5PL }
         )
  
  return(nPL)
}


sef.lma <- function(pars, x, yobs, sd.weight.fnc.lma.2, nPL) 
{
  bottom  <- pars[1]
  top     <- pars[2]
  ec50    <- pars[3]
  slope   <- pars[4]
  asym    <- pars[5]
  
  yfit      <- nPL(bottom, top, ec50, slope, asym, x)
  residuals <- (yobs - yfit)^2
  w         <- sd.weight.fnc.lma.2(x, yobs)
  
  return(sum(w * residuals))
}


initPars.lma <- function(x, y, npars) 
{
  bottom  <- as.numeric(quantile(y, 0.025, na.rm = T))
  top     <- as.numeric(quantile(y, 0.975, na.rm = T))
  ec50    <- (max(x) + min(x))/2
  slope   <- estSlope.lma(x, y)
  asym    <- 1
  
  return(c(bottom, top, ec50, slope, asym))
}


estSlope.lma <- function(x, y) 
{
  bottom    <- as.numeric(quantile(y, 0.025, na.rm = T))
  top       <- as.numeric(quantile(y, 0.975, na.rm = T))
  
  z         <- (y - bottom)/(top - bottom)
  z[z <= 0] <- 0.025
  z[z >= 1] <- 0.975
  
  z         <- as.vector(by(z, x, mean, na.rm = T))
  z.log     <- log(z/(1 - z))
  model     <- lm(z.log ~ unique(x))
  
  return(as.numeric(coef(model)[2]))
}


sd.weight.fnc.lma.1 <- function(x, yobs) 
{
  v <- as.numeric(by(yobs, x, var, na.rm = T))
  v <- ifelse(is.na(v), 1, v)
  
  return(1/rep(v, times = table(x)))
}


sd.weight.fnc.lma.2 <- function(x, yobs) 
{
  repTable <- table(x)
  maxRep   <- max(repTable, na.rm = T)
  
  if(maxRep == 1){
    reg.weights = rep(1, length(x))
  } else{
    m.Ct <- max(yobs) + 1
    yobs <- m.Ct - yobs
    yobs.linear <- 2^yobs
    #x.log       <- log2(x)

    mean.y <- as.numeric(by(yobs.linear, x, mean, na.rm = T))
    v      <- as.numeric(by(yobs.linear, x, var, na.rm = T))
    v      <- ifelse(is.na(v), 2, v)
    v[which(v < 2)]  <- 2
	  
    v.approx <- approx(c(min(log2(v)), max(log2(v))), y = NULL, method = "linear", n = length(v))$y
    lm.var <- lm(v.approx ~ log2(mean.y) - 1)
    #lm.var <- lm(log2(v) ~ log2(mean.y) - 1)
    #lm.var <- lm(log2(v) ~ unique(x) - 1)
    #summary(lm.var)
    #summary(lm.var)$r.squared

    pred.var                        <- 2^(predict(lm.var))
    pred.var[which(pred.var < 2)]   <- 2
    reg.weights                     <- as.numeric(1/log2(pred.var))
    #print(reg.weights)
  }
  
  return(reg.weights1 = reg.weights)
  #return(list(reg.weights1 = reg.weights, reg.weights2 = rep(reg.weights, times = repTable)))
}


perf.diag.lma <- function(y, yfit, reg.Weights, npars) 
{
  resid.sq     <- (y - yfit)^2
  lmtest       <- summary(lm(y ~ yfit, weights = resid.sq))
  fstat        <- lmtest$fstatistic
  p            <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
  goodness     <- gof.lma(y, resid.sq)
  
  w.resid.sq   <- reg.Weights * (y - yfit)^2
  resid.stdev  <- sqrt(sum(w.resid.sq))
  resid.stdErr <- sqrt(sum(w.resid.sq)/(length(y) - npars))
  
  return(cbind.data.frame(goodness = goodness, resid.stdev = resid.stdev, resid.stdErr = resid.stdErr, p = p))
}


gof.lma <- function(y, resid.sq) 
{
  n    <- length(y)
  S2y  <- var(y)
  Rw   <- 1 - sum(resid.sq)/((n - 1) * S2y)
  return(Rw)
}


inflPoint.lma <- function(pars) 
{
  x <- pars$ec50 + (1/pars$slope) * log2(pars$asym)
  y <- pars$bottom + (pars$top - pars$bottom) * (pars$asym/(pars$asym + 1))^pars$asym
  
  return(cbind.data.frame(x = x, y = y))
}


# Coefficient of variation (reproducibility)
coeff.var.standards <- function(data.clean)
{
  cv.df.standards  <- aggregate(data.clean[,"measured.concentration"], by = list(data.clean[,"Quantity"]), 
                                function(x){
                                  sd(x, na.rm = T)/mean(x, na.rm = T) * 100 #* multiplied by 100 to report %CV
                                }, simplify = T)
  colnames(cv.df.standards) <- c("Quantity", "Coeff.Var")
  
  return(cv.df.standards)
} 


coeff.var.unknowns <- function(data.clean)
{
  cv.df.unknowns  <- aggregate(data.clean[,"measured.concentration"], by = list(data.clean[,"Sample"]), 
                               function(x){
                                 sd(x, na.rm = T)/mean(x, na.rm = T) * 100 #* multiplied by 100 to report %CV
                               }, simplify = T)
  colnames(cv.df.unknowns) <- c("Sample", "Coeff.Var")
  cv.df.unknowns <- cv.df.unknowns[which(cv.df.unknowns$Sample != ""),]
  
  return(cv.df.unknowns)
	
}


# Outlier detection
outlier.detection.advanced <- function(outlier.input.df, outlier.sig.level, perf, npars)
{
  outlier.df             <- outlier.input.df
  outlier.df$pos.id.out  <- 1:nrow(outlier.df)
  outlier.df$resid       <- round(abs(outlier.df$yFit - outlier.df$yObs), 4)
  outlier.df             <- outlier.df[order(outlier.df$resid),]
  
  outlier.df$fdr.level <- 0
  for(i in floor(0.90 * dim(outlier.df)[1]):dim(outlier.df)[1]){
    outlier.df$fdr.level[i] <- round((outlier.sig.level * (dim(outlier.df)[1] - (i-1)))/dim(outlier.df)[1], 4)
  }
  
  outlier.df$t.stat   <- round(outlier.df$resid/(perf$resid.stdev), 4)
  outlier.df$t.pvalue <- round(2 * pt(outlier.df$t.stat, df = dim(outlier.df)[1] - npars, lower.tail = F), 4)  # two-sided
  
  outlier.df$outlier.Flag <- 0
  outlier.df$outlier.Flag[which(outlier.df$t.pvalue < outlier.df$fdr.level)] <- 1
  outlier.df <- outlier.df[order(outlier.df$pos.id.out),]
	
  return(outlier.df)
}


outlier.detection.cv <- function(data.clean, cutoff)
{
  cutoff.pass.index <- which(data.clean$Task == "STANDARD" & data.clean$Coeff.Var >= cutoff)
  data.clean$outlier.Flag <- 0
  data.clean$outlier.Flag[cutoff.pass.index] <- 1
  
  return(data.clean)
}


outlier.detection.rec <- function(data.clean, cutoff)
{
  cutoff.pass.index <- which(data.clean$Task == "STANDARD" & (data.clean$standards.recovery < cutoff[1] | data.clean$standards.recovery > cutoff[2])) 
  data.clean$outlier.Flag <- 0
  data.clean$outlier.Flag[cutoff.pass.index] <- 1
  
  return(data.clean)
}


outlier.detection.cv.rec <- function(data.clean, outlier.params)
{
  cutoff.cv  <- as.numeric(outlier.params[1])
  cutoff.rec <- as.numeric(unlist(strsplit(outlier.params[2], split = "-")))
  cutoff.cv.pass.index  <- which(data.clean$Task == "STANDARD" & data.clean$Coeff.Var >= cutoff.cv)
  cutoff.rec.pass.index <- which(data.clean$Task == "STANDARD" & (data.clean$standards.recovery < cutoff.rec[1] | data.clean$standards.recovery > cutoff.rec[2]))
  data.clean$outlier.Flag <- 0
  data.clean$outlier.Flag[union(cutoff.cv.pass.index,cutoff.rec.pass.index)] <- 1
  
  return(data.clean)
}


# Linear range
linear.range <- function(bottom, top, ec50, slope, asym, newX, pars)
{
  a <- bottom
  d <- top
  c <- ec50
  b <- slope
  f <- asym
  
  dy    <- round(log(2)*(d-a)*f*(newX-c)*2^((c-newX)*b)*(2^((c-newX)*b)+1)^(-f-1), 5)
  dy2   <- function(x.var){
    log(2)*(d-a)*f*2^(b*(c-x.var))*(2^(b*(c-x.var))+1)^(-f-2)*((log(2)*b*f*x.var-log(2)*b*c*f+1)*2^(b*(c-x.var))-log(2)*b*x.var+log(2)*b*c+1)
  }
  
  #dy2.newX <- dy2(newX)
  #
  #windowsFonts(Calibri = windowsFont("Calibri"))
  #windows(width=16, height=8)
  #plot.new()
  #
  #plot(dy ~ newX)#, ylim = c(0,100))
  #points(dy2.newX ~ newX)
  
  dy2.x  <- seq(min(newX) - 10, inflPoint.lma(pars)$x, length = 10000)
  dy2.y  <- round(dy2(dy2.x), 1)
  
  bendL.conc <- dy2.x[which(diff(sign(dy2.y)) == 1)][1]
  if(is.na(bendL.conc) == T) bendL.conc <- min(newX)
  
  dy2.x  <- seq(inflPoint.lma(pars)$x, inflPoint.lma(pars)$x + 10, length = 10000)
  #dy2.x  <- seq(inflPoint.lma(pars)$x, max(newX) + 10, length = 10000)
  dy2.y  <- round(dy2(dy2.x), 1)
  
  bendH.conc <- dy2.x[which(diff(sign(dy2.y)) == -1)][1]
  if(is.na(bendH.conc) == T) bendH.conc <- max(newX)
  
  bendL.conc.linear   <- 2^bendL.conc
  bendH.conc.linear   <- 2^bendH.conc
  linear.range.log10  <- log10(2^bendH.conc) - log10(2^bendL.conc)
  
  linear.range.results <- list()
  linear.range.results[["bendL.conc.linear"]] <- bendL.conc.linear
  linear.range.results[["bendH.conc.linear"]] <- bendH.conc.linear
  linear.range.results[["linear.range.log10"]] <- linear.range.log10
  
  return(linear.range.results)
}


limit.of.detection <- function(data.final, bottom, top, ec50, slope, asym)
{
  lod.conc <- NA
  
  if(dim(data.final[data.final[,"Task"] == "NTC",])[1] > 1){
    lod.Ct <- 
      if(mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 2 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) > bottom){
        mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 2 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T)
      } else if(mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 3 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) > bottom){
        mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 3 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T)
      } else if(mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 4 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) > bottom){
        mean(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T) + 4 * sd(data.final[data.final[,"Task"] == "NTC","delta.Ct"], na.rm = T)
      } else{
        bottom + 0.01
      }
    
    lod.conc <- 2^(ec50 - (log2(((top - bottom)/(lod.Ct - bottom))^(1/asym) - 1)/slope))
  }
  
  return(lod.conc)
}


# function starts
log.logistic.lma <- function(data = dr.df, use.log = T, npars, outlier.method, outlier.sig.level, omittedStdWells, omittedUnkWells) 
{
  #* omittedStdWells contain user omitted wells (could be R predicted outlier(s) or/and any other standard(s)), and we need to filter out data(rows) corresponding to these wells 
  data.clean <- data[!(data$Well %in% omittedStdWells | data$Well %in% omittedUnkWells),]
  #*
  #data.clean <- data.clean[!data$Well %in% omittedUnkWells,]
  
  well.id  <- data.clean$Well
  x        <- data.clean$Quantity
  y        <- data.clean$Ct
  
  if(length(x) != length(y)) 
    stop("x and y lengths differ.")
  
  if(is.numeric(npars) & (npars < 4 | npars > 5)) 
    stop("\n'npars' must be 4 or 5")
  
  #repTable <- table(x)
  #maxRep   <- max(repTable, na.rm = T)
  #minRep   <- min(repTable, na.rm = T)
  
  missing.values <- 0
  
  if(any(is.na(x) | is.na(y))){
    missing.values <- union(which(is.na(x)), which(is.na(y)))
    x              <- x[-missing.values]
    y              <- y[-missing.values]
    well.id.stand  <- as.character(well.id[-missing.values])
    
    #warning(call. = F, paste(length(missing.values), " point(s) has(ve) been removed for missingness.", sep = ""), immediate. = T)
    #message()
  } else{
    well.id.stand <- as.character(well.id)
  }
  
  #max.Ct         <- max(y) + 3		#old logic
  if(length(which(data.clean$Task == "NTC")) > 0){
    max.Ct <- mean(data.clean$Ct[which(data.clean$Task == "NTC")], na.rm = T)          #new logic for max.Ct
  } else{
    max.Ct <- NA
    #stop("At least 1 NTC well is required to compute delta Ct values.")
  }
  
  y              <- y[order(x)]
  #ct-for-dct
  #y             <- max.Ct - y             #log10(2^((max.Ct) - y))
  well.id.stand  <- well.id.stand[order(x)]
  
  x              <- sort(x)
  if(use.log) x  <- log2(x)
  
  nPL            <- logisticModel.lma(npars)
  init.pars      <- initPars.lma(x, y, npars)
  reg.Weights    <- sd.weight.fnc.lma.2(x, y)    #$reg.weights1
  
  print("x: ")
  print(x)
  print("y: ")
  print(y)
  print("init.pars: ")
  print(init.pars)
  print("reg.Weights: ")
  print(reg.Weights)
  
  print("before nlm")
  x.nlm <- as.numeric(by(x, x, mean, na.rm = T))
  print(x.nlm)
  
  yobs.nlm <- as.numeric(by(y, x, mean, na.rm = T))
  print(yobs.nlm)
  
  # Newton-Raphson optimization algorithm
  
  best <- nlm(f = sef.lma, p = init.pars, x = as.numeric(by(x, x, mean, na.rm = T)), yobs = as.numeric(by(y, x, mean, na.rm = T)), sd.weight.fnc.lma.2, nPL, hessian = T,
              print.level = 0, gradtol = 1e-4, steptol = 1e-5, iterlim = 10000)
  
  #best <- nlm(f = sef.lma, p = init.pars, x = x, yobs = y, sd.weight.fnc.lma.2, nPL, print.level = 0, gradtol = 1e-4, steptol = 1e-8, iterlim = 10000)
  
  if(best$iterations == 0)
    stop("'nlm' failed to estimate parameters.\n")
  
  bottom <- best$estimate[1]
  top    <- best$estimate[2]
  ec50   <- best$estimate[3]
  slope  <- best$estimate[4]
  asym   <- best$estimate[5]
  
  print("5pl parametesr: ")
  print(bottom)
  print(top)
  print(ec50)
  print(slope)
  print(asym)
  
  gradient   <- best$gradient
  hessian    <- best$hessian
  code       <- best$code
  iterations <- best$iterations
  
  yObs       <- y
  yObs.mean  <- as.numeric(by(y, x, mean, na.rm = T))
  newX       <- seq(min(x) - 0.75, max(x) + 0.75, length = 500)
  newY       <- nPL(bottom, top, ec50, slope, asym, newX)
  yFit       <- nPL(bottom, top, ec50, slope, asym, x)
  yFit.mean  <- nPL(bottom, top, ec50, slope, asym, as.numeric(by(x, x, mean, na.rm = T)))
  
  if(length(unique(signif(yFit, 5))) == 1)
    stop("convergence failed and returned constant fitted values.")
  
  perf <- perf.diag.lma(yObs.mean, yFit.mean, reg.Weights, npars)
  pars <- cbind.data.frame(bottom = bottom, top = top, ec50 = ec50, slope = slope, asym = asym)
  
  # ct-for-dct
  #data.clean$delta.Ct                <- max.Ct - data.clean$Ct
  data.clean$delta.Ct                <- data.clean$Ct
  data.clean$measured.concentration  <- 2^(ec50 - (log2(((top - bottom)/(data.clean$delta.Ct - bottom))^(1/asym) - 1)/slope))
  
  # 18may2017: setting meas conc for NTC as zero(o)
  data.clean$measured.concentration[which(data.clean$Task == "NTC")] <- 0
  data.clean$standards.recovery <- round(data.clean$measured.concentration/data.clean$Quantity * 100, 0)
  
  data.clean$pos.id.clean  <- 1:nrow(data.clean)
  
  # Linear range
  linear.range.results <- linear.range(bottom, top, ec50, slope, asym, newX, pars)
  
  # Coefficient of variation (reproducibility)
  cv.df.standards <- coeff.var.standards(data.clean)
  cv.df.unknowns  <- coeff.var.unknowns(data.clean)
  
  data.clean <- merge(data.clean, cv.df.standards, by = "Quantity", all = T)
  data.clean$Coeff.Var[which(data.clean$Task == "UNKNOWN")] <- cv.df.unknowns$Coeff.Var[match(data.clean$Sample[which(data.clean$Task == "UNKNOWN")], cv.df.unknowns$Sample)]
  data.clean <- data.clean[order(data.clean$pos.id.clean),]
  
  # Outlier detection
  outlier.df <- data.frame()
  if(outlier.method == "advanced"){
    #outlier.df <- data.frame()
    outlier.input.df  <- data.frame(Well = well.id.stand, yObs = round(yObs, 4), yFit = round(yFit, 4))
    outlier.sig.level <- as.numeric(outlier.sig.level)
    
    # Use following only if standards with quantity value in linear range need to be used in outlier detection
    #stdWellsInLinearRange <- data.clean[which(data.clean$Quantity<linear.range.results$bendH.conc.linear & data.clean$Quantity>linear.range.results$bendL.conc.linear),"Well"]
    #outlier.input.df <- outlier.input.df[outlier.input.df$Well %in% stdWellsInLinearRange,]
    
    outlier.df <- outlier.detection.advanced(outlier.input.df, outlier.sig.level, perf, npars)
    
    data.clean <- merge(data.clean, outlier.df, by = "Well", all = T)
    data.clean <- data.clean[order(data.clean$pos.id.clean),]
  } else if(outlier.method == "basic"){
    outlier.params <- unlist(strsplit(outlier.sig.level,split = ","))
    if(outlier.params[1] == "none" && outlier.params[2] == "none"){
      data.clean$outlier.Flag <- 0
    } else if(outlier.params[1] != "none" && outlier.params[2] == "none"){
      cutoff     <- as.numeric(outlier.params[1])
      data.clean <- outlier.detection.cv(data.clean, cutoff)
    } else if(outlier.params[1] == "none" && outlier.params[2] != "none"){
      cutoff     <- as.numeric(unlist(strsplit(outlier.params[2], split = "-")))
      data.clean <- outlier.detection.rec(data.clean, cutoff)
    } else if(outlier.params[1] != "none" && outlier.params[2] != "none"){
       data.clean <- outlier.detection.cv.rec(data.clean, outlier.params)
    }
  }
  
  data.final         <- data
  data.final$pos.id  <- 1:nrow(data.final)
  data.final         <- merge(data.final[,c("Well", "Sample", "Target", "Task", "Ct", "Quantity", "Omit", "Serum.Percentage", "pos.id", "Biogroup.Name")], data.clean, all = T)
  
  #* for user omitted wells/standards: calculating delta.Ct and setting userOmit as True
  omitted.wells.index <- which(data.final$Well %in% omittedStdWells)
  print(omitted.wells.index)
  data.final$userOmit <- "False"
  
  for(i in omitted.wells.index){
    # ct-for-dct
    #data.final[i,"delta.Ct"] <- max.Ct-data.final[i,"Ct"]
    data.final[i,"delta.Ct"] <- data.final[i,"Ct"]
    data.final$userOmit[i]   <- "True"
  }
  #*
  
  # for user omitted unknowns: calculating delta.Ct and setting userOmit as True
  omittedUnkWells.index <- which(data.final$Well %in% omittedUnkWells)
  print(omittedUnkWells.index)
  
  for(i in omittedUnkWells.index){
    # ct-for-dct
    #data.final[i,"delta.Ct"] <- max.Ct-data.final[i,"Ct"]
    data.final[i,"delta.Ct"] <- data.final[i,"Ct"]
    data.final[i,"measured.concentration"] <- 2^(ec50 - (log2(((top - bottom)/(data.final[i,"delta.Ct"] - bottom))^(1/asym) - 1)/slope))
    data.final$userOmit[i] <- "True"
  }
  
  data.final <- data.final[order(data.final$pos.id),]
  
  #* setting outlier as 0 for all NA values i.e. unknowns -> as UI code handles 0 or 1
  #data.final$outlier.Flag[(which(is.na(data.final$outlier.Flag)))] <- 0
  #*
  
  data.final$Omit[which(data.final$outlier.Flag == 1)] <- TRUE
  
  #print("data.final before computing serum.conc")
  #print(data.final)
  #* adding serum dilution factor logic to calculate actual serum concentration for unknowns
  serumPercentagePresent.index <- which(!is.na(data.final$Serum.Percentage))
  #print("serumPercentagePresent.index: ")
  #print(serumPercentagePresent.index)
  
  #print(data.final$serum.conc[which(!is.na(data.final$Serum.Percentage))])
  data.final$serum.conc <- NA
  # 18may2017: instead of dilution percentage, user is directly entering dilution factor/ fold
  data.final$serum.conc[which(!is.na(data.final$Serum.Percentage))] <- data.final$measured.concentration[which(!is.na(data.final$Serum.Percentage))] * (data.final$Serum.Percentage[which(!is.na(data.final$Serum.Percentage))])
  #print(data.final$serum.conc)
  data.final$serum.conc[which(is.na(data.final$Serum.Percentage))]  <- data.final$measured.concentration[which(is.na(data.final$Serum.Percentage))]
  #*
  #print("data.final after computing serum.conc")
  #print(data.final)
  #* Commenting out below line bcz -> dr.df.updated data set was required to re-run 5pl on updated Omit column, which we agreed to discontinue
  #dr.df.updated <- data.final[,c("Well", "Sample", "Target", "Task", "Ct", "Quantity", "Omit")]
  #*
  
  standard.exp.conc  <- 2^(ec50 - (log2(((top - bottom)/(yFit - bottom))^(1/asym) - 1)/slope))
  standard.meas.conc <- 2^(ec50 - (log2(((top - bottom)/(y - bottom))^(1/asym) - 1)/slope))
  
  #####
  ##### code for median CV, linear range and LOD
  #####
  
  # median Coefficient of Variation of the standards only
  median.CV.standards <- paste(round(median(cv.df.standards$Coeff.Var, na.rm = T) * 100, 1), "%", sep = "")
  
  # Experimental LOD
  lod.conc <- tryCatch({
    #results <- limit.of.detection(data.final,bottom,top,ec50,slope,asym)
    results <- limit.of.detection.1(data.final, bottom, top, ec50, slope, asym)
    }, warning = function(war){
      # warning handler picks up where error was generated
      print(paste("lod computation Warning: ", war))
      return(NA)
      }, error = function(err){
        # error handler picks up where error was generated
        print(paste("lod computation Error: ", err))
        #results <- NA
        return(NA)
        }) # END tryCatch
  
  #####
  #####
  #####
  
  npl.list <- list()
  
  npl.list[["missing.Values"]]       <-  length(missing.values)
  npl.list[["numPar"]]               <-  npars
  npl.list[["initialPar"]]           <-  init.pars
  npl.list[["estPar"]]               <-  pars
  npl.list[["reg.Weights"]]          <-  reg.Weights
  npl.list[["yObs"]]                 <-  yObs
  npl.list[["yObs.mean"]]            <-  yObs.mean
  npl.list[["yFit"]]                 <-  yFit
  npl.list[["yFit.mean"]]            <-  yFit.mean
  npl.list[["x.ext"]]                <-  newX
  npl.list[["y.ext"]]                <-  newY
  npl.list[["standard.exp.conc"]]    <-  standard.exp.conc
  npl.list[["standard.meas.conc"]]   <-  standard.meas.conc
  npl.list[["max.Ct"]]               <-  max.Ct
  npl.list[["infl"]]                 <-  inflPoint.lma(pars)
  npl.list[["goodness"]]             <-  perf$goodness
  npl.list[["resid.stdev"]]          <-  perf$resid.stdev
  npl.list[["resid.stdErr"]]         <-  perf$resid.stdErr
  npl.list[["outlier.df"]]           <-  outlier.df
  npl.list[["outlier.Flag"]]         <-  outlier.df$outlier.Flag
  npl.list[["gradient"]]             <-  gradient
  npl.list[["hessian"]]              <-  hessian
  npl.list[["code"]]                 <-  code
  npl.list[["iterations"]]           <-  iterations
  npl.list[["results.df"]]           <-  data.final
  npl.list[["median.CV.standards"]]  <-  median.CV.standards
  npl.list[["linear.range.log10"]]   <-  linear.range.results$linear.range.log10
  npl.list[["bendL.conc.linear"]]    <-  linear.range.results$bendL.conc.linear
  npl.list[["bendH.conc.linear"]]    <-  linear.range.results$bendH.conc.linear
  npl.list[["LOD.conc"]]             <-  lod.conc
  
  #* dr.df.updated not needed now
  #npl.list[["dr.df.updated"]]       <- dr.df.updated
  #* 
  return(npl.list)
}


#* adding code to identify replicates' set and compute avg plot data for unknowns, standards and NTC
average.plot.data <- function(results.df)
{
  all.df <- data.frame("Well" = results.df$Well, 
                       "Sample" = results.df$Sample, 
                       "Task" = results.df$Task, 
                       "delta.Ct" = results.df$delta.Ct, 
                       "Quantity" = results.df$Quantity, 
                       "measured.concentration" = results.df$measured.concentration, 
                       "serum.conc" = results.df$serum.conc, 
                       "cv" = results.df$Coeff.Var, 
                       "recov" = results.df$standards.recovery, 
                       "Target" = results.df$Target, 
                       "userOmit" = results.df$userOmit)
  print("all.df for avgPlotdata")
  print(all.df)
  
  samples.avgPlotData.df   <- data.frame()
  standards.avgPlotData.df <- data.frame()
  NTC.avgPlotData.df       <- data.frame()
  
  # averaging for unknowns
  if(is.element("UNKNOWN", unique(all.df$Task))){
    samples.df <- subset(all.df, all.df$Task == "UNKNOWN" & all.df$userOmit == "False")
    colnames(samples.df)[colnames(samples.df) == "Sample"] <- "ID"
    samples.df$x.mean <- samples.df$measured.concentration
    
    # calculating conc. value for average plot
    samples.x.mean.df <- aggregate(x.mean ~ ID, data = samples.df, mean, na.rm = T, na.action = NULL)
    
    #* calculating avg serum conc values for unknowns
    samples.serumConc.mean.df <- aggregate(serum.conc ~ ID, data = samples.df, mean, na.rm = T, na.action = NULL)
    #*
    
    # calculating dct values for average plot: computing 5pl derived dct value using averaged conc. value for each replicates' set
    samples.xMeanValues <- log2(samples.x.mean.df$x.mean)
    samples.y.5plVal.using.xMeanVal <- results$parameter$bottom + (results$parameter$top - results$parameter$bottom)/(1 + 2^((results$parameter$ec50 - samples.xMeanValues) * results$parameter$slope))^results$parameter$asym
    #logistic.5PL(results$parameter$bottom, results$parameter$top, results$parameter$ec50, results$parameter$slope, results$parameter$asym, samples.xMeanValues)
    
    samples.y.mean.df <- data.frame(ID = samples.x.mean.df$ID, delta.Ct.mean = samples.y.5plVal.using.xMeanVal, delta.Ct.sd = NA)
    
    samples.wellInfo.df   <- aggregate(Well ~ ID, data = samples.df, paste, collapse = ",")
    samples.targetInfo.df <- aggregate(Target ~ ID, data = samples.df, function(x) list(unique(x)))
    samples.taskInfo.df   <- aggregate(Task ~ ID, data = samples.df, function(x) unique(x))
    
    #* as of now, we compute Coeff.var only for standards; If we will compute Coeff.var for unknowns in future, following line will include them in avgPlotData
    samples.cv.df <- aggregate(cv ~ ID, data = samples.df, function(x) unique(x), na.action = NULL)
    #*
    
    samples.avgPlotData.df <- merge(merge(merge(merge(merge(merge(samples.x.mean.df, samples.y.mean.df, by = "ID"), samples.wellInfo.df, by = "ID"), samples.targetInfo.df, by = "ID"), samples.taskInfo.df, by = "ID"), samples.cv.df, by = "ID"), samples.serumConc.mean.df, by = "ID")
    
    # we don't compute standards.recovery for unknowns
    samples.avgPlotData.df$recov <- NA
    samples.avgPlotData.df$conc  <- NA
    print(samples.avgPlotData.df)
  }
  
  # averaging for standards
  if(is.element("STANDARD", unique(all.df$Task))){
    standards.df <- subset(all.df, all.df$Task == "STANDARD" & all.df$userOmit == "False")
    # for standards we are using Quantity (not measured conc.) for x.mean
    standards.df$x.mean <- standards.df$Quantity # 26may2017
    standards.df$conc   <- standards.df$measured.concentration
    colnames(standards.df)[colnames(standards.df) == "Quantity"] <- "ID"
    # convert numeric ID for standards to characters (to avoid factors related error for ID column in rbind step)
    standards.df$ID <- as.character(standards.df$ID)
    
    # calculating dct values for average plot: for standards we are using arithmetic mean of dct values
    standards.y.mean.df     <- do.call(data.frame, aggregate(delta.Ct ~ ID, data = standards.df, function(x) c(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))))
    standards.x.mean.df     <- aggregate(x.mean ~ ID, data = standards.df, mean, na.rm = T, na.action = NULL)
    standards.conc.df       <- aggregate(conc ~ ID, data = standards.df, mean, na.rm = T, na.action = NULL)
    standards.wellInfo.df   <- aggregate(Well ~ ID, data = standards.df, paste, collapse = ",")
    standards.targetInfo.df <- aggregate(Target ~ ID, data = standards.df, function(x) list(unique(x)))
    standards.taskInfo.df   <- aggregate(Task ~ ID, data = standards.df, function(x) unique(x))
    standards.cv.df         <- aggregate(cv ~ ID, data = standards.df, function(x) unique(x), na.action = NULL)
    standards.recov.df      <- aggregate(recov ~ ID, data = standards.df, mean,na.rm = T, na.action = NULL)
    
    standards.avgPlotData.df <- merge(merge(merge(merge(merge(merge(merge(standards.x.mean.df, standards.y.mean.df, by = "ID"), standards.wellInfo.df, by = "ID"), standards.targetInfo.df, by = "ID"), standards.taskInfo.df, by = "ID"), standards.cv.df, by = "ID"), standards.recov.df, by = "ID"),standards.conc.df, by = "ID")
    #* we don't compute serum.conc for standards
    standards.avgPlotData.df$serum.conc <- NA
    print(standards.avgPlotData.df)
  }
  
  # averaging for NTC
  if(is.element("NTC", unique(all.df$Task))){
    NTC.df        <- subset(all.df, all.df$Task == "NTC")
    NTC.df$ID     <- NTC.df$Task
    NTC.df$x.mean <- NTC.df$measured.concentration
    NTC.x.mean.df <- aggregate(x.mean ~ ID, data = NTC.df, mean, na.rm = T, na.action = NULL)
    
    # calculating dct values for average plot: computing 5pl derived dct value using averaged conc. value for each replicates' set
    #NTC.xMeanValues = log2(NTC.x.mean.df$measured.concentration)
    #print(NTC.xMeanValues)
    #NTC.y.5plVal.using.xMeanVal = results$parameter$bottom + (results$parameter$top - results$parameter$bottom)/(1 + 2^((results$parameter$ec50 - NTC.xMeanValues) * results$parameter$slope))^results$parameter$asym
    #logistic.5PL(results$parameter$bottom, results$parameter$top, results$parameter$ec50, results$parameter$slope, results$parameter$asym, NTC.xMeanValues)
    
    #NTC.y.mean.df = data.frame(ID = NTC.x.mean.df$ID, delta.Ct.mean=NTC.y.5plVal.using.xMeanVal, delta.Ct.sd=NA)
    
    #18may2017: For NTC we are setting delta.Ct.mean as zero(0); arithmetic mean of NTC dct values should give zero by formula but, due to round off we get non-zero value
    # NTC.y.mean.df = data.frame(ID = NTC.x.mean.df$ID, delta.Ct.mean = 0, delta.Ct.sd = NA)
    # masn. fix mean ntc = 0 issue
    NTC.y.mean.df <- data.frame(ID = NTC.x.mean.df$ID, delta.Ct.mean = mean(NTC.df$delta.Ct), delta.Ct.sd = sd(NTC.df$delta.Ct))
    
    NTC.wellInfo.df    <- aggregate(Well ~ ID, data = NTC.df, paste, collapse = ",")
    NTC.targetInfo.df  <- aggregate(Target ~ ID, data = NTC.df, function(x) list(unique(x)))
    NTC.taskInfo.df    <- aggregate(Task ~ ID, data = NTC.df, function(x) unique(x))
    NTC.avgPlotData.df <- merge(merge(merge(merge(NTC.x.mean.df, NTC.y.mean.df, by = "ID"), NTC.wellInfo.df, by = "ID"), NTC.targetInfo.df, by = "ID"), NTC.taskInfo.df, by = "ID")
    
    #* we don't compute standards.recovery, Coeff.Var and serum.conc for NTC
    NTC.avgPlotData.df$cv         <- NA
    NTC.avgPlotData.df$recov      <- NA
    NTC.avgPlotData.df$serum.conc <- NA
    NTC.avgPlotData.df$conc       <- NA
    print(NTC.avgPlotData.df)
  }
  
  # Including NTC average data as of now....
  avgPlotData.df = rbind(standards.avgPlotData.df, samples.avgPlotData.df, NTC.avgPlotData.df) 
  return(avgPlotData.df)
}
