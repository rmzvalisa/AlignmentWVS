# Functions

## groupwiseCFA.modified, runAlignment.modified, globalMI.modified.ord, and
## plotCutoff.modified functions are the modified versions of groupwiseCFA, runAlignment, 
## globalMI, and plotCutoff functions that are part of the MIE R package. 
## See Rudnev M. (2018-2019) Measurement Invariance Explorer. 
## Retrieved from: https://github.com/MaksimRudnev/MIE

# Function for CFA

groupwiseCFA.modified <- function(model,  data, group, ..., out = c("fit", "models", "goodfit", "modification")) {
  fit.list <- lapply(unique(data[, group]), function(gr) {
    print(gr)
    lavaan::cfa(model, data = data[data[, group]==gr, ], ...)
  })
  
  names(fit.list)<- unique(data[, group])
  
  tb.countrywise <- lapply(fit.list, function(x) data.frame(
    converged = x@optim$converged, 
    CFI=ifelse (x@optim$converged, fitMeasures(x)[c("cfi.scaled")],  NA),
    RMSEA=ifelse (x@optim$converged, fitMeasures(x)["rmsea.scaled"], NA),
    SRMR=ifelse (x@optim$converged, fitMeasures(x)["srmr"],NA),
    CHI.sq = ifelse (x@optim$converged, fitMeasures(x)[c("chisq.scaled")],NA),
    Pvalue = ifelse (x@optim$converged, fitMeasures(x)[c("pvalue.scaled")],NA),
    Low.Load = ifelse (x@optim$converged, ifelse(any(inspect(x, what="std")$lambda < 0.3 & inspect(x, what="std")$lambda != 0), "+", ""), NA),
    Neg.Var = ifelse (x@optim$converged, ifelse(any(inspect(x, what="std")$lambda > 1), "+", ""),NA),
    mod.ind=ifelse (lavInspect(x, what = "post.check"), 
                    paste(modindices(x, sort = T)[1,1:3], collapse = ""), ""),
    mod.ind.v=ifelse (lavInspect(x, what = "post.check"),  (round(modindices(x, sort = T)[1,4], 3)), ""),
    stringsAsFactors = F))
  tb.countrywise1 <- Reduce("rbind", tb.countrywise)
  rownames(tb.countrywise1) <- names(tb.countrywise)
  tb.countrywise1
  tb.countrywise1$mod.ind <-  gsub("~~",  " W ",  tb.countrywise1$mod.ind )
  
  
  if("models" %in% out) return(fit.list)
  if("fit" %in% out) { 
    b=tb.countrywise1[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR", "Low.Load", "Neg.Var")]
    b$CHI.sq <- round(b$CHI.sq, 3)
    b$Pvalue <- round(b$Pvalue, 3)
    b$CFI <- round(b$CFI, 3)
    b$RMSEA <- round(b$RMSEA, 3)
    b$SRMR <- round(b$SRMR, 3)
    df_to_viewer(b)
    invisible(tb.countrywise1)
    
  }
  
  if("modification" %in% out) { 
    
    tb.countrywise1 <- tb.countrywise1[!(tb.countrywise1$Low.Load=="+"|
                                           tb.countrywise1$Neg.Var=="+"|
                                           (tb.countrywise1$CFI>=0.9 & tb.countrywise1$RMSEA<=0.08 &
                                              tb.countrywise1$SRMR<=0.08)),]
    tb.countrywise1 <- tb.countrywise1[rowSums(is.na(tb.countrywise1)) != ncol(tb.countrywise1),]
    
    b=tb.countrywise1[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR", "mod.ind", "mod.ind.v")]
    b$CHI.sq <- round(b$CHI.sq, 3)
    b$Pvalue <- round(b$Pvalue, 3)
    b$CFI <- round(b$CFI, 3)
    b$RMSEA <- round(b$RMSEA, 3)
    b$SRMR <- round(b$SRMR, 3)
    df_to_viewer(b)
    invisible(tb.countrywise1)
    
  }
  
  if("goodfit" %in% out) { 
    
    tb.countrywise1 <- tb.countrywise1[!(tb.countrywise1$Low.Load=="+"|
                                           tb.countrywise1$Neg.Var=="+"),]
    tb.countrywise1 <- tb.countrywise1[(tb.countrywise1$CFI>=0.9 & tb.countrywise1$RMSEA<=0.08 &
                                          tb.countrywise1$SRMR<=0.08),]
    tb.countrywise1 <- tb.countrywise1[rowSums(is.na(tb.countrywise1)) != ncol(tb.countrywise1),]
    
    b=tb.countrywise1[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR")]
    b$CHI.sq <- round(b$CHI.sq, 3)
    b$Pvalue <- round(b$Pvalue, 3)
    b$CFI <- round(b$CFI, 3)
    b$RMSEA <- round(b$RMSEA, 3)
    b$SRMR <- round(b$SRMR, 3)
    df_to_viewer(b)
    invisible(tb.countrywise1)
    
  }
  
  
} 




# Function for comparison of MI levels 

globalMI.modified.ord <- function(...) {
  r.conf<-lavaan::cfa(...)
  r.scalar<-lavaan::cfa(..., group.equal = c("loadings", "intercepts", "thresholds"))
  
  model.list <- list(r.conf, r.scalar)
  
  fit.diff <- c("cfi.scaled", "rmsea.scaled")
  fit.general <- c("chisq.scaled", "df", "srmr")
  
  out.diff<-t(sapply(model.list, function(x) 
    if(x@optim$converged) lavaan::fitMeasures(x)[fit.diff] else rep(NA, length(fit.diff))))
  out.diff2 <- apply(out.diff, 2, function(x) (c(NA, x[2]-x[1])))
  out.diff2 <- abs(out.diff2)
  out.diff3 <- t(Reduce("rbind", lapply(1:ncol(out.diff), function(x) rbind(out.diff[,x], out.diff2[,x]))))
  
  out.gen<-t(sapply(model.list, function(x) 
    if(x@optim$converged) lavaan::fitMeasures(x)[fit.general] else rep(NA, length(fit.general))))
  
  out <- cbind(c("Configural", "Scalar"), round(cbind(out.diff3, out.gen), 3))
  
  
}


# Function for pairwise fit comparison

plotCutoff.modified <- function(measures, fit.index = "cfi", cutoff = NULL, weighted = TRUE, drop = NULL) {
  
  
  if(any(!fit.index %in% c("cfi", "rmsea", "srmr", "rmsea.scaled", "cfi.scaled"))) {
    if (interactive()) {
      showNotification("Cutoffs for this fit measure are not available. Using convenient .01 (unrealiable!).\n Consider switching off 'Use cutoffs' option.", type = "warning", duration = NULL, id = "nocutoffs")  
    } # else {
    #   warning("Cutoffs for this fit measure are not available. Using convenient .01 (unrealiable!).\n Consider switching off 'Use cutoffs' option.")
    # }
    
  } 
  
  # remove dropped groups
  #if(!is.null(drop)) measures <- measures[!rownames(measures) %in% drop, ]
  
  #abs.thrshld <- switch(fit.index, cfi=function(x) `>`(x, .90), rmsea = function(x) `<`(x, .05))
  if(is.null(cutoff)) {
    #Chen's
    # thrshld <- switch(fit.index, 
    #                       cfi   = function(x) `<`(x, .01),
    #                       rmsea = function(x) `<`(x, .01),
    #                       srmr  = function(x) `<`(x, .01),
    #                       nnfi  = function(x) `<`(x, .01) 
    # 
    #                   )
    
    thrshld <- function(x) `<`(x, .01)
    
  } else {
    thrshld <- function(x) `<`(x, cutoff)
  }
  
  # mtrx <- measures$detailed[[fit.index]]
  # mtrx.df <- data.frame(
  #   i = gsub("^.*_", "",  rownames(mtrx)), 
  #   j = gsub("_.*$", "",  rownames(mtrx)),
  #   # absolute
  #   #tie = abs.thrshld(unname(mtrx[,2]))
  #   
  #   # incremental
  #   tie = thrshld(unname(mtrx[,"fit.decrease"]))
  # )
  
  dist1 <- reshape2::dcast(rbind( cbind(get_pairs(measures$bunch), measures$bunch[fit.index,]),
                                  cbind(`colnames<-`(get_pairs(measures$bunch)[,2:1], c("V1", "V2")), measures$bunch[fit.index,])
  ), V2 ~ V1, value.var = "measures$bunch[fit.index, ]")
  row.names(dist1)<- dist1$V2
  dist1$V2 <- NULL
  dist1<-as.matrix(dist1)
  # remove dropped groups
  if(!is.null(drop)) dist1 <- dist1[!rownames(dist1) %in% drop, !(colnames(dist1) %in% drop) ]
  
  thrshld.dist1 = 
    
    dist2<-thrshld(dist1)*1 
  dist1[!thrshld(dist1)] <- 0 
  #diag(dist1)<-rep(0, nrow(dist1))
  
  if(weighted) {
    
    dist3 <- dist1
    diag(dist1)<-0
    dist1[!thrshld(dist1)] <-0
    dist1[ thrshld(dist1)] <- 1/(dist1[thrshld(dist1)]+1)
    
    diag(dist2)<-0
    dist1[dist2==1]<-1/(dist1[dist2==1]*100+1)
    dist1[dist2==0]<-0
    # 
    
    net <- igraph::graph_from_adjacency_matrix(dist1, diag = F, mode = "lower", weighted = TRUE)
  } else {
    
    dist1 <- dist2
    net <- igraph::graph_from_adjacency_matrix(dist2, diag = F, mode = "lower", weighted = NULL)
  }
  
  
  
  
  
  clp <- igraph::cluster_label_prop(net)
  # set.seed(123)
  # igraph:::plot.communities(clp, net, edge.color = "darkgrey", layout = layout_with_fr)
  set.seed(123)
  coords <- layout_with_fr(net, dim = 2, niter = 500)
  colnames(coords)<- c("dim1", "dim2")
  coords<- as.data.frame.matrix(coords)
  coords$group <- rownames(dist1)
  clusters <- Reduce("rbind",  lapply(1:length(clp), function(x) data.frame( 
    group = clp[[x]], 
    cluster = rep(x, length(clp[[x]])), stringsAsFactors = F)))
  coords <- merge(coords, clusters, by = "group")
  find_hull <- function(df) df[chull(df$dim1, df$dim2), ]
  hulls <- plyr::ddply(coords, "cluster", find_hull)
  
  d1 <- reshape2::melt(dist1)
  d1 <- d1[d1$value!=0,]
  d2 <- merge(d1, coords, by.x = "Var1", by.y = "group", all.x = T)
  d3 <- merge(d2, coords, by.x = "Var2", by.y = "group", all.x = T)
  
  requireNamespace("ggplot2")
  requireNamespace("ggforce")
  
  g<-  ggplot(coords, aes(dim1, dim2,  col=as.factor(cluster)))+
    geom_segment(data = d3, aes(x = dim1.x, xend = dim1.y, y = dim2.x, yend = dim2.y), col = "black", alpha = .5)+
    #geom_text_repel(aes(label=group), point.padding = unit(.3, "lines"), show.legend=F)+
    
    geom_shape(data = hulls, aes(fill = as.factor(cluster) ),
               alpha = ifelse(length(unique(coords$cluster))>1, .4, 0), linetype="blank",
               expand = unit(10, "points"), radius = unit(10, "points"),
               show.legend = F)+
    geom_point( size=5, show.legend = F)+labs(x="", y="", col="")+
    geom_label(aes(label=group), show.legend=F, alpha = 1)+
    theme_void()+
    coord_fixed()+
    scale_colour_hue(l = 50, c = 120)+
    theme(panel.grid = element_blank(), axis.line=element_line(size=.5),axis.ticks=element_line(size=.5), plot.title=element_text(face="bold", size=18))+
    labs(caption=paste("Lines represent measurement invariance"))
  
  g
  # clp <- igraph::cluster_label_prop(igraph::graph_from_edgelist(as.matrix(mtrx.df[mtrx.df$tie,-3]), directed = F))
  # net <- igraph::graph_from_edgelist(as.matrix(mtrx.df[mtrx.df$tie,-3]), directed = F)
  # igraph:::plot.communities(clp, net)
  
}



# Function for alignment 

runAlignment.modified <- function(
  model = "Moral BY prostit homosex abortion divorce;", 
  group = "country",
  dat = wvs.s, 
  estim = "mlr",
  categorical = NULL,
  sim.samples = c(100, 500, 1000), # can be NULL to avoid simulations
  sim.reps = 500,
  Mplus_com = "Mplus",
  path = getwd(),
  summaries = FALSE
) {
  oldwd <- getwd()
  setwd(path)
  message("Create input for free alignment.\n")
  var.list <- strsplit(model, ";|\n") [[1]]
  var.list <-   var.list[!var.list==""]
  var.list <-   unlist(strsplit(var.list, "(?i)(by)", perl=TRUE))
  var.list <-   unlist(strsplit(var.list[seq(2, length(var.list), by=3)], " "))
  var.list <- paste(unique(unlist(var.list)), collapse=" ")
  var.list <- strsplit(var.list, " ")[[1]]
  var.list <-   var.list[!var.list==""]
  # var.list <- paste0("; ", model, " ;")
  # var.list<- gsub("\n", ";", var.list)
  # var.list <- paste(sapply(var.list, function(i) sub(".*BY *(.*?) *;.*", "\\1", i)), collapse=" ")
  # var.list <- strsplit(gsub(" BY | +|;", " ", var.list), " ")[[1]]
  # var.list <- var.list[!var.list ==""]
  d <- dat[c(group, var.list)]
  for(i in colnames(d)) d[,i] <- unclass(d[,i])
  rm(i)
  if(!is.numeric(d[,group])) {
    #d[,group] <- gsub(" ", "_", as.character( d[,group] )  )
    message("The group variable must be numeric!")
  }
  #require(MplusAutomation)
  #inp <- capture.output(prepareMplusData(d,  "mplus_temp.tab"))
  write.table(d, "mplus_temp.tab", quote=F, sep="\t", row.names=F, col.names=F, na=".")
  #var.list <- gsub("\\.", "_", var.list)
  list.of.groups = unique(as.matrix(d[,1]))
  ngroups = length(list.of.groups)
  inp <- c("DATA:","\n",
           "   file = 'mplus_temp.tab';", "\n",
           "   listwise = ON;", "\n",
           " VARIABLE:", "\n",
           "   names =", gsub("\\.", "_", group), " ", paste(gsub("\\.", "_", var.list), collapse=" "), ";\n",
           "   missing = .;", "\n",
           ifelse(any(is.null(categorical)),
                  "\n",
                  paste("   categorical = ", paste(categorical, collapse = " "), ";\n")
           ),
           "   classes = c(", ngroups, ");\n",
           "   knownclass = c(", paste0(gsub("\\.", "_", group), " = ", list.of.groups, " \n    ", collapse=""),
           ");\n\n",
           "ANALYSIS:\n",
           "  type = mixture;\n",
           "  estimator =", estim, ";\n",
           "  alignment =", kind = "", ";\n", 
           ifelse(any(is.null(categorical)),
                  "\n",  
                  "  algorithm = integration;\n\n"),
           "MODEL:\n",
           "  %OVERALL%\n",
           model, 
           "\n\n",
           "OUTPUT: align tech8 SVALUES;", 
           "\n\n",
           "SAVEDATA: ", "\n",
           "  RANKING = ranking.dat; "
  )
  inp["kind"]<-"FREE"
  cat(inp, file = "free.inp", sep="")
  message("Run free in Mplus.")
  trash <- system(paste(Mplus_com, "free.inp"))
  outFree <- paste(readLines("free.out"), collapse = "\n") 
  if(grepl("TO AVOID MISSPECIFICATION USE THE GROUP WITH VALUE", outFree)) {
    refGroup <- sub(".*TO AVOID MISSPECIFICATION USE THE GROUP WITH VALUE *(.*?) *AS THE BASELINE GROUP.*", "\\1", outFree)
  } else {
    free.tab.means <- sub(".*FACTOR MEAN COMPARISON AT THE 5% SIGNIFICANCE LEVEL IN DESCENDING ORDER *(.*?) *QUALITY OF NUMERICAL RESULTS.*", "\\1", outFree)
    refGroup <- as.character(read.table(text=sub(".*\n *(.*?) *\n\n\n\n\n.*", "\\1", free.tab.means))[3])
  }
  inp["kind"]<-paste0("FIXED(", refGroup, ")")
  cat(inp, file = "fixed.inp", sep="")
  message("Run fixed in Mplus.")
  trash <- system(paste(Mplus_com, "fixed.inp"))
  # Creating simulations
  if(!is.null(sim.samples)) {
    outFixed <- paste(readLines("fixed.out"), collapse = "\n") 
    
    rownames(list.of.groups)  <- (1:length(list.of.groups))
    refClass <- rownames(list.of.groups)[list.of.groups[,1] == refGroup]
    
    stValues <- sub(".*MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES *(.*?) *\n\n\n\n.*", "\\1", outFixed)
    stValues <- gsub("%C#", "%g#", stValues)
    stValues <- gsub("c#", "g#", stValues)
    corrupt.code <- sub(".*%OVERALL% *(.*?) *%g#1%.*", "\\1", stValues)
    correction <-strsplit(corrupt.code, "\n")[[1]]
    correction <- correction[grep(" BY ",  correction)]
    correction <- gsub(";", "*1;", correction)
    stValues <- paste(paste(correction, collapse="\n"), "\n", substr(stValues, regexpr("%g#1%", stValues), nchar(stValues)))
    if(!any(is.null(categorical))) {
      g1 <- sub(".*%g#1% *(.*?) *%g#2%.*", "\\1", stValues)
      g1 <- strsplit(g1, "\n")[[1]]
      g1 <- g1[grep("\\[", g1)]
      g1 <- g1[grep("\\$", g1)]
      g1 <- sapply(g1 , function(x)   sub(" *\\[ *(.*?) *\\$.*", "\\1", x))
      gen.cat <- paste0(names(table(g1)), " (", table(g1), ")")
    }
    for(x in sim.samples) { 
      code <- c("MONTECARLO:",
                " NAMES = ", paste(gsub("\\.", "_", var.list), collapse = " "), ";\n",
                " ngroups = ", ngroups, ";\n", 
                " NOBSERVATIONS =", ngroups, "(", x, ");\n", 
                " NREPS =", sim.reps, ";\n\n",
                ifelse(any(is.null(categorical)),
                       "\n",  
                       paste(
                         " CATEGORICAL =", paste(categorical, collapse = " "), ";\n", 
                         " GENERATE = ", paste(gen.cat, collapse = " "),
                         ";\n\n"  )),
                "ANALYSIS:",
                " TYPE = MIXTURE;",
                " PROCESSORS = 8;",
                " ESTIMATOR = ", estim, ";",
                " alignment = fixed(", refClass, ");\n",
 
                ifelse(any(is.null(categorical)),
                       "\n",  
                       " algorithm = integration;\n\n"),
                "MODEL POPULATION:",
                " %OVERALL%\n",
                paste(stValues, collapse="\n"),
                "\nMODEL:",
                " %OVERALL%\n",
                paste(stValues, collapse="\n")
      )
      cat(code, sep="", file = paste0("sim", x , ".inp"))
    }
    for (x in sim.samples) {
      message("Run simulation", x, "in Mplus.\n")
      trash <- system(paste(Mplus_com, paste0("sim", x, ".inp")))
    }
  }
  # Return summaries
  if(summaries) {
    if(!is.null(sim.samples)) {
      otpt <- list(fixed= extractAlignment("fixed.out", silent = TRUE),
                   free = extractAlignment("free.out", silent = TRUE),
                   simulations = extractAlignmentSim(sapply(sim.samples, function(x) paste0("sim", x, ".out")), silent = TRUE)
      )
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Free alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="")
      print(otpt$free$summary)
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Fixed alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$fixed$summary)
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of simulations", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$simulations)
    } else {
      otpt <- list(fixed = extractAlignment("fixed.out", silent = TRUE),
                   free =  extractAlignment("free.out", silent = TRUE))
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Free alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="")
      print(otpt$free$summary)
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Fixed alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$fixed$summary)
    }
  } else {
    message("Done running models. Refer to the free.out, fixed.out, ranking.dat and some sim###.out files.\nConsider  using `extractAlignment()` and `extractAlignmentSim()` to extract important parts.")
  }
  setwd(oldwd)
  if(summaries) invisible(otpt)
}


## Function for SE alignment extraction

extractSE.WVS <-  function(file = "fixed.out") {
  
  # Basic extraction function  
  extractBetween <- function(begin, end, string) {
    mapply(function(a, b) substr(string, a, b),
           gregexpr(begin, string)[[1]]+nchar(begin),
           gregexpr(end, string)[[1]]-1
    )  
  }
  
  # Read file
  output <-  paste(readLines("fixed.out"), collapse="\n")
  
  output <- extractBetween("MODEL RESULTS", "Categorical Latent Variables", output)
  output <-strsplit(output, "Means\n    RELIGIOSIT")[[1]]
  output <- as.list(output)
  output[[1]] <- NULL
  for (i in 1:length(output)){
    output[[i]] <- extractBetween("        ", "Intercepts\n", output[[i]])[1]
    output[[i]] <- strsplit(output[[i]], "    ")[[1]][2]
    
  }
  output <- lapply(output, function (output) trim(output))
  
  output <- unlist(output)
  
  
}




## Function for means plot

ggmeans <- function(data) {
  ggplot(data, aes(y = data[, 2], reorder(data[, 3], -data[, 2]))) +
    geom_linerange(
      aes(
        ymin = data[, 2] - 1.96 * data[, 4], ymax = data[, 2] + 1.96 * data[, 4]
      ),
      colour = "black", alpha = 0.5, size = 0.7
    ) +
    geom_point(col = "black", shape = 19, size = 1.5) +
    coord_flip() + geom_hline(yintercept = 0, lty = 3) +
    labs(x = "", y = names(data[2])) +
    theme_minimal() + theme(
      legend.position = "none", panel.grid = element_blank(), axis.line.x = element_line(size = 0.6),
      axis.text.x = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      panel.background = element_rect(fill = "transparent", colour = NA)
    ) +
    geom_text(
      aes(label = data[, 2]),
      vjust = -0.5, size = 5
    )
}




