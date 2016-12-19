
## ANALYSIS AS OF Dec 20th (JoC 1st Revise and Resubmit).
setwd("/Users/songh95/Dropbox/GitHub/ABM")
require(ggplot2)
require(grid)
require(gridExtra)
Sys.setenv(LANG = "en")

## loading required pacakage and start up netlogo instances

require(parallel)

# modify following path where appropriate

if( Sys.info()[['sysname']] == 'Windows' ) {
   ## Windows 10 with the most recent Java JDK (1.8.0_112) 
   nl.path <-  "C:/Program Files/NetLogo 5.3.1/app"
   model.path.1 <- "C:/Users/Hyunjin/Dropbox/GitHub/ABM/Model/Model 6_strong attitudes.nlogo"
   model.path.2 <- "C:/Users/Hyunjin/Dropbox/GitHub/ABM/Model/Model 6.nlogo"
   model.path.3 <- "C:/Users/Hyunjin/Dropbox/GitHub/ABM/Model/Model 6_information decay.nlogo"

 } else if ( Sys.info()[['sysname']] == "Darwin") {
   ## OS X version 10.12.2 (16C67) with Java JDK 1.8.0_74-b02
   Sys.setenv(LANG = "en")
   
   ## loading required pacakage and start up netlogo instances
   ## It seems that netlogo instance requires FULL PATH of Java and models
   ## DO NOT USE SHORTCUT FOR USER DIRECTORY (e.g., ~/Dropbox)
   nl.path <-  "/Users/songh95/Dropbox/GitHub/ABM/NetLogo 5.3.1/Java"
   model.path.1 <- "/Users/songh95/Dropbox/GitHub/ABM/Model/Model 6_strong attitudes.nlogo"
   model.path.2 <- "/Users/songh95/Dropbox/GitHub/ABM/Model/Model 6.nlogo"
   model.path.3 <- "/Users/songh95/Dropbox/GitHub/ABM/Model/Model 6_information decay.nlogo"
   Sys.setenv(NOAWT=1)
   setwd("/Users/songh95/Dropbox/GitHub/ABM/NetLogo 5.3.1/Java")
   library(RNetLogo)

}

## check Java version
require(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")


gui <- F ## change to T if you want to see graphics
source("dev/helper-functions.R") ## load helper functions

## set number of replicated simulation (e.g., 100),
## timestep, and custom-seed as a vector length of k
nsim <- 100
timestep <- 1095 ## 3 years (365*3)
set.seed(12345)
rand.seed <- sort(sample(1:47822,nsim,replace = F))
## rand.seed later will be fed into netlogo
## in order to ensure identical seeds for reproducibility,
## line 23 should be executed right after set.seed(12345)
## double check your result with this key table

# [1,]    55 3762  8971 15545 19241 24349 30808 34859 38358 44306
# [2,]   287 3838 10177 15620 20508 24364 31272 34990 38576 45195
# [3,]   413 4565 10457 16035 20803 24525 32585 35174 39414 45307
# [4,]   917 6405 10824 17282 21690 24560 32871 35792 39522 45493
# [5,]  1652 6899 11300 17687 21829 26004 33388 36148 41517 45906
# [6,]  2077 7138 12356 17808 22111 28670 33817 36391 41557 46075
# [7,]  2304 7167 12408 18556 23159 28682 34116 36945 41881 46147
# [8,]  2631 7286 14562 18633 23848 29504 34369 37375 42374 46793
# [9,]  2876 7956 15049 18703 23980 29553 34476 37807 43206 47091
# [10,] 2895 8556 15348 18986 24140 29885 34795 37900 44294 47323


## detect the no. of cores and create cluster workers
processors <- detectCores()
cl <- makeCluster(processors, type="SOCK")
clusterExport(cl, list=ls())

## load Netlogo in each processor/core
parLapply(cl, 1:processors, pre_process, 
          gui = gui, nl.path = nl.path, model.path = model.path.1)

## model estimation for additional models
results.par.model4.strong.attitudes <- parLapply(cl, rand.seed, sim_model4_strong_attitudes)
results.par.model4.strong.attitudes <- lapply(1:100, function(k) { process.output(results.par.model4.strong.attitudes[[k]]) })

results.par.model6.strong.attitudes <- parLapply(cl, rand.seed, sim_model6_strong_attitudes)
results.par.model6.strong.attitudes <- lapply(1:100, function(k) { process.output(results.par.model6.strong.attitudes[[k]]) })

# change the model by stopping the cluster and recreate cluster workers
parLapply(cl, 1:processors, postpro)
stopCluster(cl)
cl <- makeCluster(processors, type="SOCK")
clusterExport(cl, list=ls())
parLapply(cl, 1:processors, pre_process, 
          gui = gui, nl.path = nl.path, model.path = model.path.2)

results.par.model4.indirect.exposure <- parLapply(cl, rand.seed, sim_model4_indirect_exposure)
results.par.model4.indirect.exposure <- lapply(1:100, function(k) { process.output(results.par.model4.indirect.exposure[[k]]) })

results.par.model6.indirect.exposure <- parLapply(cl, rand.seed, sim_model6_indirect_exposure)
results.par.model6.indirect.exposure <- lapply(1:100, function(k) { process.output(results.par.model6.indirect.exposure[[k]]) })

results.par.model4.european.cases <- parLapply(cl, rand.seed, sim_model4_european_cases)
results.par.model4.european.cases <- lapply(1:100, function(k) { process.output(results.par.model4.european.cases[[k]]) })

results.par.model6.european.cases <- parLapply(cl, rand.seed, sim_model6_european_cases)
results.par.model6.european.cases <- lapply(1:100, function(k) { process.output(results.par.model6.european.cases[[k]]) })

results.par.model4.interest.pro.avoid.interaction <- parLapply(cl, rand.seed, sim_model4_pro_interest_interaction_and_avoidance)
results.par.model4.interest.pro.avoid.interaction <- lapply(1:100, function(k) { process.output(results.par.model4.interest.pro.avoid.interaction[[k]]) })

results.par.model6.interest.pro.avoid.interaction <- parLapply(cl, rand.seed, sim_model6_pro_interest_interaction_and_avoidance)
results.par.model6.interest.pro.avoid.interaction <- lapply(1:100, function(k) { process.output(results.par.model6.interest.pro.avoid.interaction[[k]]) })

# change the model by stopping the cluster and recreate cluster workers
parLapply(cl, 1:processors, postpro)
stopCluster(cl)
cl <- makeCluster(processors, type="SOCK")
clusterExport(cl, list=ls())
parLapply(cl, 1:processors, pre_process, 
          gui = gui, nl.path = nl.path, model.path = model.path.3)

results.par.model4.information.decay <- parLapply(cl, rand.seed, sim_model4_information_decay)
results.par.model4.information.decay <- lapply(1:100, function(k) { process.output(results.par.model4.information.decay[[k]]) })
save(results.par.model4.information.decay, file = "results.par.model4.information.decay.Rdata")

## save raw output file for later use
## cf. this output file stores information as the list [nsim] -- list [timestep]
save(results.par.model4.strong.attitudes,
     results.par.model6.strong.attitudes,
     results.par.model4.indirect.exposure,
     results.par.model6.indirect.exposure,
     results.par.model4.european.cases,
     results.par.model6.european.cases,
     results.par.model4.interest.pro.avoid.interaction, 
     results.par.model6.interest.pro.avoid.interaction,
     results.par.model4.information.decay,
     file="output.161212.additional.model.results.Appendix.Rdata")


## derive plots and over-time trends from additional models
R2.models <- list(results.par.model4.strong.attitudes, results.par.model6.strong.attitudes,
                  results.par.model4.european.cases, results.par.model6.european.cases,
                  results.par.model4.interest.pro.avoid.interaction, 
                  results.par.model6.interest.pro.avoid.interaction)

## cf. data structure
## output.data -
# - model #1 to #6
# - simluation #1 to #100
# - data.frame # dim = c(1095,3), where row = time, col = variance, kurtosis, ER

## mean of attitude distribution variance and kurtosis over 100 replication and its 95% CIs
## check with ggplot -- Figure 2 (variance and kurtosis combined)

# first, process additional models for R2's comment.
## Figure 1A. Variance and kurtosis distributions
mean_var <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(R2.models[[i]], "var"), 1, mean,na.rm = T)} ## get "result.par.modelx.var" and calculate mean 6 times
    ,simplify = FALSE))  ## return a list -> unlist to make a vector (long format)
LLCI_var <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "var"), 1, quantile_95)[1,]}, simplify = FALSE))
ULCI_var <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "var"), 1, quantile_95)[2,]}, simplify = FALSE))

mean_kur <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "kur"), 1, mean,na.rm = T)}, simplify = FALSE))
LLCI_kur <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "kur"), 1, quantile_95)[1,]}, simplify=FALSE))
ULCI_kur <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "kur"), 1, quantile_95)[2,]}, simplify=FALSE))

mean_ER <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "ER"), 1, mean,na.rm = T)}, simplify=FALSE))
LLCI_ER <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "ER"), 1, quantile_95)[1,]}, simplify=FALSE))
ULCI_ER <- unlist(sapply(1:6, function(i) {apply(reshape.output(R2.models[[i]], "ER"), 1, quantile_95)[2,]}, simplify=FALSE))

plot.data.R2 <- data.frame(tick = c(rep(1:timestep,6), rep(1:timestep,6)),
                        mean=c(mean_var,mean_kur),
                        LLCI=c(LLCI_var,LLCI_kur),
                        ULCI=c(ULCI_var,ULCI_kur),
                        model=factor(c(rep(1:6,each=timestep), rep(1:6,each=timestep))),
                        variable=factor(rep(c("Variance","Kurtosis"), each=6*timestep))
                        )
levels(plot.data.R2$variable) <- c("Variance", "Kurtosis")

plot.data.R2.ER <- data.frame(tick = rep(1:timestep, 6),
                           mean = mean_ER,
                           LLCI = LLCI_ER,
                           ULCI = ULCI_ER,
                           model = factor(rep(1:6, each = timestep)))

## prepare model labels
models <- paste0("Model ", rep(c(4,6), times = 3), rep(c("A","B","C"), each = 2))

## get combined legend
data <- plot.data.R2[plot.data.R2$model==1,]
p1 <- ggplot(data,aes(x=tick,y=mean, linetype=variable)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin=LLCI, ymax=ULCI, linetype=variable, fill=variable),alpha=0.5) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.direction="horizontal") +
  scale_fill_manual(values=c("#999999","#666666"))
combined.legend <- get_legend(p1)

data <- plot.data.R2.ER[plot.data.R2.ER$model==1,]
data$variable <- "ER-index"
p2 <- ggplot(data,aes(x=tick,y=mean, linetype=variable)) + 
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin=LLCI, ymax=ULCI),alpha=0.3) +
  xlab("") + ylab("") + theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.direction="horizontal")
combined.legend.2 <- get_legend(p2)

p <- lapply(1:6, function(i) {
  data <- plot.data.R2[plot.data.R2$model==i,]
  p <- ggplot(data,aes(x=tick,y=mean, linetype=variable)) +
    geom_line(size=0.7) +
    annotate("text",x=20, y=ceiling(max(data[,'ULCI'])), label=models[i]) +
    geom_ribbon(aes(ymin=LLCI, ymax=ULCI, linetype=variable, fill=variable),alpha=0.5) +
    xlab("") + ylab("") + theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none") +
    scale_fill_manual(values=c("#999999","#666666"))
  
  return(p)
})

p[[1]] <- p[[1]] + theme(axis.text.x = element_blank())
p[[3]] <- p[[3]] + theme(axis.text.x = element_blank())
p[[5]] <- p[[5]] + theme(axis.text.x = element_blank())

p2 <- lapply(1:6, function(i) {
  data <- plot.data.R2.ER[plot.data.R2.ER$model==i,]
  p <- ggplot(data,aes(x=tick,y=mean)) + geom_line(size=0.7) +
    annotate("text",x=10, y=ceiling(max(data[,'ULCI'])), label=models[i]) +
    geom_ribbon(aes(ymin=LLCI, ymax=ULCI),alpha=0.3) +
    xlab("") + ylab("") + theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none")
  
  return(p)
})

p2[[1]] <- p2[[1]] + theme(axis.text.x = element_blank())
p2[[3]] <- p2[[3]] + theme(axis.text.x = element_blank())
p2[[5]] <- p2[[5]] + theme(axis.text.x = element_blank())

base <- grid.arrange(p[[1]],p2[[1]],p[[3]],p2[[3]],p[[5]],p2[[5]],
                     ncol = 2, nrow = 3)
combined.legend <- grid.arrange(combined.legend, combined.legend.2,
                     ncol = 2, nrow = 1)
grid.arrange(base, combined.legend , ncol = 1, nrow = 2, heights = c(7,0.5))



## create 3-dim array (time = 1095, replication = 100, model = 6)
## out.array[[1]] == variance, out.array[[2]] == kurtosis, out.array[[3]] == ERindex
out.array.R2 <- lapply(1:3, function(k) {
  val <- lapply(1:6,function(i) {
    val <- array((do.call("rbind", R2.models[[i]]))[,k], dim = c(1095,100))
    })
  val <- do.call("cbind", val)
  val <- array(val, dim = c(1095,100,6))
  return(val)
})

names(out.array.R2) <- c("variance","kurtosis","ER")
dimnames(out.array.R2[[1]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0("Model.", rep(c(4,6), times = 3), rep(c("A","B","C"), each = 2)))
dimnames(out.array.R2[[2]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0("Model.", rep(c(4,6), times = 3), rep(c("A","B","C"), each = 2)))
dimnames(out.array.R2[[3]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0("Model.", rep(c(4,6), times = 3), rep(c("A","B","C"), each = 2)))


## load main results for comparison
load("output.160723.Rdata")
## process the data 
output.data <- list(result.par.model1,result.par.model2,result.par.model3,result.par.model4,
                    result.par.model5,result.par.model6)
out.array <- lapply(1:3, function(k) {
  val <- lapply(1:6,function(i) {
    val <- array((do.call("rbind",output.data[[i]]))[,k],dim=c(1095,100))
  })
  val <- do.call("cbind",val)
  val <- array(val,dim=c(1095,100,6))
  return(val)
})

names(out.array) <- c("variance","kurtosis","ER")
dimnames(out.array[[1]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0('model',1:6))
dimnames(out.array[[2]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0('model',1:6))
dimnames(out.array[[3]]) <- list(paste0("t",1:1095),
                                 paste0("sim",1:100),
                                 paste0('model',1:6))

## simulation results, quantities reported in memo
## during-election period descriptive
table.1.appendix <- matrix(
  # start of the data
  c(# variance over time
    mean(out.array[['variance']]["t550",,"model4"]), mean(out.array.R2[['variance']]["t550",,"Model.4B"]),
    sd(out.array[['variance']]["t550",,"model4"]), sd(out.array.R2[['variance']]["t550",,"Model.4B"]),
    max(out.array[['variance']]["t550",,"model4"]), max(out.array.R2[['variance']]["t550",,"Model.4B"]), 
    # Excessive Kurtosis over time
    mean(out.array[['kurtosis']]["t550",,"model4"]), mean(out.array.R2[['kurtosis']]["t550",,"Model.4B"]),
    sd(out.array[['kurtosis']]["t550",,"model4"]), sd(out.array.R2[['kurtosis']]["t550",,"Model.4B"]),
    max(out.array[['kurtosis']]["t550",,"model4"]), max(out.array.R2[['kurtosis']]["t550",,"Model.4B"]),
    # ER-index over time
    mean(out.array[['ER']]["t550",,"model4"]), mean(out.array.R2[['ER']]["t550",,"Model.4B"]),
    sd(out.array[['ER']]["t550",,"model4"]), sd(out.array.R2[['ER']]["t550",,"Model.4B"]),
    max(out.array[['ER']]["t550",,"model4"]), max(out.array.R2[['ER']]["t550",,"Model.4B"])), 
  # end data
  nrow = 9, ncol = 2, byrow = T)

colnames(table.1.appendix) <- c("Model 4", "Model4B")
rownames(table.1.appendix) <- c("var_M", "Var_SD", "Var_max", 
                                "Kur_M", "Kur_SD", "Kur_max",  
                                "ER_M", "ER_SD", "ER_max")

# Table 1 in page XX of the memo.
table.1.appendix


## R3's comments
R3.models <- list(results.par.model4.information.decay, results.par.model4.indirect.exposure)

mean_var <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "var"), 1, mean,na.rm = T)}, simplify = FALSE))
LLCI_var <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "var"), 1, quantile_95)[1,]}, simplify = FALSE))
ULCI_var <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "var"), 1, quantile_95)[2,]}, simplify = FALSE))

mean_kur <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "kur"), 1, mean,na.rm = T)}, simplify = FALSE))
LLCI_kur <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "kur"), 1, quantile_95)[1,]}, simplify=FALSE))
ULCI_kur <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "kur"), 1, quantile_95)[2,]}, simplify=FALSE))

mean_ER <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "ER"), 1, mean,na.rm = T)}, simplify=FALSE))
LLCI_ER <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "ER"), 1, quantile_95)[1,]}, simplify=FALSE))
ULCI_ER <- unlist(sapply(1:2, function(i) {apply(reshape.output(R3.models[[i]], "ER"), 1, quantile_95)[2,]}, simplify=FALSE))

mapply(paste, 
       rep(x <- paste0("model", 1:2), times = 3), 
       rep(y <- c("Variance", "Kurtosis", "ER"), each = 2), 
       sep = ".")

plot.data.R3 <- data.frame(tick = rep(rep(1:timestep, 2), 3),
                           mean = c(mean_var, mean_kur, mean_ER),
                           LLCI = c(LLCI_var, LLCI_kur, LLCI_ER),
                           ULCI = c(ULCI_var, ULCI_kur, ULCI_ER),
                           model = rep(1:6, each = timestep))

models <- mapply(paste, rep(x <- paste0("model 4", c("D","E")), times = 3),
                        rep(y <- c("Variance", "Kurtosis", "ER"), each = 2), 
                 sep = " - ")

limits <- list(c(0,10), c(0,10), c(-2,6), c(-2,6), c(0.1,1), c(0.1,1))

p3 <- lapply(1:6, function(i) {

  data <- plot.data.R3[plot.data.R3$model==i,]
  p <- ggplot(data,aes(x=tick,y=mean)) + ylim(limits[[i]]) +
    geom_line(size=0.7) +
    annotate("text", x = 150, y=ceiling(max(limits[[i]])), label = models[i]) +
    geom_ribbon(aes(ymin=LLCI, ymax=ULCI),alpha=0.5) +
    xlab("") + ylab("") + theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none")
  
  return(p)
})

for (i in 1:6) {p3[[i]] <- p3[[i]] + theme(axis.text.x = element_blank())}
grid.arrange(p3[[1]],p3[[2]],p3[[3]],p3[[4]],p3[[5]],p3[[6]], ncol = 2, nrow = 3)

