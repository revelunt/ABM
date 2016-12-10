
list.of.packages <- c("cowplot","ggplot2", "moments","car","gridExtra","grid","fields","foreach","doSNOW","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(cowplot)
require(ggplot2)
require(car)
require(gridExtra)
require(grid)
require(foreach)
require(doSNOW)

# following functions were directly incorporated within NETLOGO model.
#
# kurtosis.attr <- function(x) {
#
#   # kurtosis from "moments" pacakge returns Pearson Kurtosis
#   # we need to substract 3 (kurtosis value of a standard normal distribution)
#   # to make a comparison (i.e., excess kurtosis).
#   kurtosis.attr <- moments::kurtosis(x)-3
#   return(kurtosis.attr)
# }
#
# # HH index is currently not used...
# HHindex.attr <- function(x) {
#
#   # HH index is defined by the sum of squared proportions
#   prob <- table(x)/sum(table(x))
#   return(sum(prob^2))
# }
#
# # ICC is currently not used...
# ICC.attr <- function(y) {
#   group <- car::recode(y, "c(-3,-2,-1)='dem';c(1,2,3)='rep';else='other'", as.factor.result = T)
#   out <- ICC::ICCest(x=group,y=y)
#   out <- out$ICC
#   return(out)
# }
#
# ER.pol.attr <- function(y) {
#
#   # Esteban & Ray's (1994; 2005; 2011) polarization index
#   # also see Montalvo & Reynal-Querol (2008)
#   # and http://www.sciencedirect.com/science/article/pii/S0304387804001129
#   group <- car::recode(y, "c(-3,-2,-1)='dem';c(1,2,3)='rep';else='other'", as.factor.result = T)
#   prob.group <- table(group)/sum(table(group))
#   group.names <- names(summary(group))
#   group.mean <- aggregate(y,by=list(factor(group)),mean)[,2]
#
#   ER.pol.attr <- numeric()
#   for (i in 1:length(prob.group)) {
#     for (j in 1:length(prob.group)) {
#       x <- y[group==group.names[i]]
#       z <- y[group==group.names[j]]
#       ER.pol.attr <- cbind(ER.pol.attr, (prob.group[i]^2)*prob.group[j]*mean(fields::rdist(x,z)))
#     }
#   }
#   ER.pol.attr <- sum(ER.pol.attr)/1.5 ## 1.5 is the max value of index based on max bimodal distribution.
#
#   return(ER.pol.attr)
# }

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, widths=NULL, heights=NULL,
                      title=NULL, titlefont = "", titleface = 1, titlesize = 16) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (!is.null(title)) { # Add a narrow row at the top for the title
    layout <- rbind(rep(0,ncol(layout)),layout)
    if (is.null(heights)) {
      plotrows <- nrow(layout)-1
      rowheights <- c(0.1, rep(1,plotrows)/plotrows)
    } else {
      rowheights <- c(0.1, heights/sum(heights))
    }
  } else {
    if (is.null(heights)) {
      rowheights <- rep(1,nrow(layout))
    } else {
      rowheights <- heights
    }
  }

  if (is.null(widths)) {
    colwidths <- rep(1, cols)
  } else {
    colwidths <- widths
  }

  if (numPlots==1) {

    return(plots[[1]] + labs(title=title))

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),
                                               widths=colwidths,
                                               heights=rowheights)))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }

    if (!is.null(title)) {
      grid.text(title, vp = viewport(layout.pos.row = 1
                                     , layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontfamily = titlefont, fontface = titleface,
                          fontsize = titlesize))
    }

  }
  return(invisible(NULL))
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# define function to extract 95% CIs from replications
quantile_95 <- function(x) {
  r <- quantile(x, probs=c(0.025,0.975),na.rm = T)
  names(r) <- c("LLCI","ULCI")
  return(r)
}


## initialization function for parallel processing
pre_process <- function(dummy, gui, nl.path, model.path) {
  library(RNetLogo)
  library(fields)
  library(moments)
  library(car)
  NLStart(nl.path, gui=gui)
  NLLoadModel(model.path)
}


## simulation function for parallel processing
sim_model1 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"weighted-mean-average\"")
  NLCommand("set model-election-cycle \"No\"")
  NLCommand("set model-preference-for-politics \"No\"")

  NLCommand(paste0("set custom-random-seed ",custom.seed))
  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  ##return the result
  return(attr.dist)

}

sim_model2 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"No\"")
  NLCommand("set model-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))

  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  ##return the result
  return(attr.dist)


}

sim_model3 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"weighted-mean-average\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))

  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  ##return the result
  return(attr.dist)

}

sim_model4 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))

  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  ##return the result
  return(attr.dist)

}

sim_model5 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"weighted-mean-average\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))

  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  ##return the result
  return(attr.dist)

}

sim_model6 <- function(custom.seed) {

  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")

  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")

  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")

  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")

  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))

  ## create a world
  NLCommand("setup")

  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))

  return(attr.dist)
}

sim_model4_strong_attitudes <- function(custom.seed) { 
  ## this model is to examine the influence of strong attitudes (e.g., ideology)
  ## compared to the previous cases, media influnece / social influence are less strong (0.4 -> 0.2)
  ## and the stability (i.e., random decay) is improved (0.2 -> 0.1)
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter ## this is initial condition. 
  NLCommand("set media-influence-parameter 0.2")
  NLCommand("set social-influence-parameter 0.2")
  NLCommand("set random-decay-parameter 0.1")
  
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model6_strong_attitudes <- function(custom.seed) { 
  ## this model is to examine the influence of strong attitudes (e.g., ideology)
  ## compared to the previous cases, media influnece / social influence are less strong (0.4 -> 0.2)
  ## and the stability (i.e., random decay) is improved (0.2 -> 0.1)
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter ## this is initial condition. 
  NLCommand("set media-influence-parameter 0.2")
  NLCommand("set social-influence-parameter 0.2")
  NLCommand("set random-decay-parameter 0.1")
  
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model4_indirect_exposure <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"On\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model6_indirect_exposure <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"On\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model4_europian_cases <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 0.5 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"Off\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model6_europian_cases <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 0.5 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"Off\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model4_pro_interest_interaction_and_avoidance <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"Off\"")
  NLCommand("set media-interest-interaction \"On\"")
  NLCommand("set selective-avoidance \"On\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"No\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

sim_model6_pro_interest_interaction_and_avoidance <- function(custom.seed) {
  
  ## set the size of a world
  NLCommand("set world-size-x 50 set world-size-y 50")
  
  ## set media exposure variable
  NLCommand("set exposure-to-pro-media 1 set exposure-to-counter-media 0.4")
  
  ## set political discussion variables
  ## cf. workgin with quotation marks for command line requires escape character (\" \")
  NLCommand("set discussant-select-base-on-homophily true")
  NLCommand("set social-influence-model \"mean-average-model\"")
  NLCommand("set propensity-for-homophily 0.4")
  
  ## set opinion dynamics model parameter
  NLCommand("set media-influence-parameter 0.4")
  NLCommand("set social-influence-parameter 0.4")
  NLCommand("set random-decay-parameter 0.20")
  
  NLCommand("set indirect-exposure \"Off\"")
  NLCommand("set media-interest-interaction \"On\"")
  NLCommand("set selective-avoidance \"On\"")
  NLCommand("set opinion-update-model \"WMA-disagree-pro-and-counter-exposure\"")
  NLCommand("set model-election-cycle \"Yes\"")
  NLCommand("set model-dropout-based-on-preference-for-politics \"Yes\"")
  NLCommand(paste0("set custom-random-seed ",custom.seed))
  
  ## create a world
  NLCommand("setup")
  
  ## set reporter
  attr.dist <- NLDoReport(timestep, "go", c("variance [attitudes] of turtles","kurtosis [attitudes] of turtles","ER.pol.index [attitudes] of turtles"))
  
  ##return the result
  return(attr.dist)
  
}

## the quit function
postpro <- function(x) {NLQuit()}


## define function to process output

process.output <- function(attr.dist) {

  for (i in 1:timestep) {
  attr.dist[[i]] <- do.call("cbind",attr.dist[[i]])
  }

  attr.dist <- as.data.frame(do.call("rbind",attr.dist))
  rownames(attr.dist) <- paste0("t",1:timestep)
  colnames(attr.dist) <- c("variance","kurtosis","ER.index")

  ##return the result
  return(attr.dist)
}

reshape.output <- function(model.output,var=c("var","kur","ER")) {

  if (var == "var") {
    colnum <- 1
  } else if (var == "kur") {
    colnum <- 2
  } else if (var == "ER") {
    colnum <- 3
  } else { stop("please select valid variable to process") }

  dat <- sapply(1:100, function(i) return(model.output[[i]][,colnum]))
  rownames(dat) <- rownames(model.output[[1]])
  colnames(dat) <- paste0("sim",1:length(model.output))
  return(dat)
}


## multiple plots in one page
print.multiplot <- function(mean=mean,LLCI=LLCI,ULCI=ULCI,title=NULL) {

plot.data <- data.frame(tick=rep(1:timestep,6),
                            mean=mean,
                            LLCI=LLCI,
                            ULCI=ULCI,
                            model=factor(rep(1:6,each=timestep)))

p1.range <- 1:timestep
p1 <- ggplot(plot.data[p1.range,],aes(x=tick,y=mean)) + geom_line() +
  #expand_limits(y=c(floor(min(LLCI[1:300])),ceiling(max(ULCI[1:300])))) +
  annotate("text",x=10, y=ceiling(max(plot.data[p1.range,'ULCI'])), label="Model 1") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()


p2.range <- (timestep+1):(2*timestep)
p2 <- ggplot(plot.data[p2.range,],aes(x=tick,y=mean)) + geom_line() +
  annotate("text",x=10, y=ceiling(max(plot.data[p2.range,'ULCI'])), label="Model 2") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()


p3.range <- (2*timestep+1):(3*timestep)
p3 <- ggplot(plot.data[p3.range,],aes(x=tick,y=mean)) + geom_line() +
  annotate("text",x=10, y=ceiling(max(plot.data[p3.range,'ULCI'])), label="Model 3") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()


p4.range <- (3*timestep+1):(4*timestep)
p4 <- ggplot(plot.data[p4.range,],aes(x=tick,y=mean)) + geom_line() +
  annotate("text",x=10, y=ceiling(max(plot.data[p4.range,'ULCI'])), label="Model 4") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()


p5.range <- (4*timestep+1):(5*timestep)
p5 <- ggplot(plot.data[p5.range,],aes(x=tick,y=mean)) + geom_line() +
  annotate("text",x=10, y=ceiling(max(plot.data[p5.range,'ULCI'])), label="Model 5") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()

p6.range <- (5*timestep+1):(6*timestep)
p6 <- ggplot(plot.data[p6.range,],aes(x=tick,y=mean)) + geom_line() +
  annotate("text",x=10, y=ceiling(max(plot.data[p6.range,'ULCI'])), label="Model 6") +
  geom_ribbon(aes(ymin=LLCI,ymax=ULCI),alpha=0.2) + xlab("") + ylab("") + theme_minimal()

all.plot <- multiplot(p1,p2,p3,p4,p5,p6,layout=matrix(c(1,2,3,4,5,6),nrow=3,byrow=TRUE),title=title)

return(all.plot)

}

## finally, processing the output into plot directly
## input arguments takes the list of model outputs and the variable to plot
print.ts.plot <- function(model.outputs=
  list(result.par.model1,result.par.model2,result.par.model3,result.par.model4,
       result.par.model5,result.par.model6),var=c("var","kur","ER")) {

  if (var == "var") {
    title <- "Variance of attitude distribution"
  } else if (var == "kur") {
    title <- "Kurtosis of attitude distribution"
  } else if (var == "ER") {
    title <- "Esteban & Ray's polarization index"
  } else { stop("please select valid variable to process") }




  ## mean of attitude distribution variance over 100 replication and its 95% CIs
  result.par.model1 <- reshape.output(result.par.model1,var)
  result.par.model2 <- reshape.output(result.par.model2,var)
  result.par.model3 <- reshape.output(result.par.model3,var)
  result.par.model4 <- reshape.output(result.par.model4,var)
  result.par.model5 <- reshape.output(result.par.model5,var)
  result.par.model6 <- reshape.output(result.par.model6,var)

  mean <- unlist(
    sapply(1:6, function(x) {
      apply(get(paste0("result.par.model",x)),1,mean,na.rm = T)} ## get "result.par.modelx.var" and calculate mean 6 times
      ,simplify=FALSE))  ## return a list -> unlist to make a vector (long format)
  LLCI <- unlist(
    sapply(1:6, function(x) {
      apply(get(paste0("result.par.model",x)),1,quantile_95)[1,]}
      ,simplify=FALSE))
  ULCI <- unlist(
    sapply(1:6, function(x) {
      apply(get(paste0("result.par.model",x)),1,quantile_95)[2,]}
      ,simplify=FALSE))

  ## check with ggplot
  combine.plot <- print.multiplot(mean,LLCI,ULCI,title=title)
  return(combine.plot)

}

