
## ANALYSIS AS OF July 29th.
require(ggplot2)
require(grid)
require(gridExtra)
Sys.setenv(LANG = "en")

## loading required pacakage and start up netlogo instances
library(RNetLogo)
require(parallel)

# modify following path where appropriate
nl.path <-  "C:/Program Files/NetLogo 5.3.1/app"
model.path <- "C:/Users/Hyunjin/Dropbox/GitHub/ABM/Model/Model 5.nlogo"
gui <- F ## change to T if you want to see graphics
source("helper-functions.R") ## load helper functions

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


## detect the no. of cores and creat cluster
processors <- detectCores()
cl <- makeCluster(processors, type="SOCK")
clusterExport(cl,c("nsim","timestep"))

## load Netlogo in each processor/core
parLapply(cl, 1:processors, pre_process, gui=gui,nl.path=nl.path, model.path=model.path)


## model estimation
result.par.model1 <- parLapply(cl, rand.seed, sim_model1)
result.par.model1 <- lapply(1:100, function(k) { process.output(result.par.model1[[k]]) })

result.par.model2 <- parLapply(cl, rand.seed, sim_model2)
result.par.model2 <- lapply(1:100, function(k) { process.output(result.par.model2[[k]]) })

result.par.model3 <- parLapply(cl, rand.seed, sim_model3)
result.par.model3 <- lapply(1:100, function(k) { process.output(result.par.model3[[k]]) })

result.par.model4 <- parLapply(cl, rand.seed, sim_model4)
result.par.model4 <- lapply(1:100, function(k) { process.output(result.par.model4[[k]]) })

result.par.model5 <- parLapply(cl, rand.seed, sim_model5)
result.par.model5 <- lapply(1:100, function(k) { process.output(result.par.model5[[k]]) })

result.par.model6 <- parLapply(cl, rand.seed, sim_model6)
result.par.model6 <- lapply(1:100, function(k) { process.output(result.par.model6[[k]]) })

# Kill child processes when they are no longer needed
parLapply(cl, 1:processors, postpro)
stopCluster(cl)

## save raw output file for later use
## cf. this output file stores information as the list [nsim] -- list [timestep]
save(result.par.model1,result.par.model2,result.par.model3,
     result.par.model4,result.par.model5,result.par.model6,file="output.160723.Rdata")

output.data <- list(result.par.model1,result.par.model2,result.par.model3,result.par.model4,
                    result.par.model5,result.par.model6)

## cf. data structure
## output.data -
   # - model #1 to #6
   # - simluation #1 to #100
   # - data.frame # dim = c(1095,3), where row = time, col = variance, kurtosis, ER

## mean of attitude distribution variance and kurtosis over 100 replication and its 95% CIs
## check with ggplot -- Figure 2 (variance and kurtosis combined)

mean_var <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"var"),1,mean,na.rm = T)} ## get "result.par.modelx.var" and calculate mean 6 times
    ,simplify=FALSE))  ## return a list -> unlist to make a vector (long format)
LLCI_var <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"var"),1,quantile_95)[1,]}
    ,simplify=FALSE))
ULCI_var <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"var"),1,quantile_95)[2,]}
    ,simplify=FALSE))

mean_kur <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"kur"),1,mean,na.rm = T)} ## get "result.par.modelx.var" and calculate mean 6 times
    ,simplify=FALSE))  ## return a list -> unlist to make a vector (long format)
LLCI_kur <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"kur"),1,quantile_95)[1,]}
    ,simplify=FALSE))
ULCI_kur <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"kur"),1,quantile_95)[2,]}
    ,simplify=FALSE))

plot.data <- data.frame(tick=c(rep(1:timestep,6),rep(1:timestep,6)),
                        mean=c(mean_var,mean_kur),
                        LLCI=c(LLCI_var,LLCI_kur),
                        ULCI=c(ULCI_var,ULCI_kur),
                        model=factor(
                          c(rep(1:6,each=timestep),rep(1:6,each=timestep))
                          ),
                        variable=factor(rep(c("Variance","Kurtosis"),each=6*timestep)))
levels(plot.data$variable) <- c("Variance", "Kurtosis")
levels(plot.data$variable)

models <- paste0("Model ",1:6)
data <- plot.data[plot.data$model==1,]
p1 <- ggplot(data,aes(x=tick,y=mean, linetype=variable)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin=LLCI, ymax=ULCI, linetype=variable, fill=variable),alpha=0.5) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.direction="horizontal") +
  scale_fill_manual(values=c("#999999","#666666"))
combined.legend <- get_legend(p1)

p <- lapply(1:6, function(i) {
  data <- plot.data[plot.data$model==i,]
  p <- ggplot(data,aes(x=tick,y=mean, linetype=variable)) +
    geom_line(size=0.7) +
    annotate("text",x=10, y=ceiling(max(data[,'ULCI'])), label=models[i]) +
    geom_ribbon(aes(ymin=LLCI, ymax=ULCI, linetype=variable, fill=variable),alpha=0.5) +
    xlab("") + ylab("") + theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none") +
    scale_fill_manual(values=c("#999999","#666666"))

  return(p)
})

p[[1]] <- p[[1]] + theme(axis.text.x = element_blank())
p[[2]] <- p[[2]] + theme(axis.text.x = element_blank())
p[[3]] <- p[[3]] + theme(axis.text.x = element_blank())
p[[4]] <- p[[4]] + theme(axis.text.x = element_blank())

base <- grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],
                     ncol=2, nrow = 3)
grid.arrange(base,combined.legend,ncol=1,nrow=2,heights = c(7,0.5))





## Figure 3. ER-index plot

mean <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"ER"),1,mean,na.rm = T)} ## get "result.par.modelx.var" and calculate mean 6 times
    ,simplify=FALSE))  ## return a list -> unlist to make a vector (long format)
LLCI <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"ER"),1,quantile_95)[1,]}
    ,simplify=FALSE))
ULCI <- unlist(
  sapply(1:6, function(i) {
    apply(reshape.output(output.data[[i]],"ER"),1,quantile_95)[2,]}
    ,simplify=FALSE))

plot.data2 <- data.frame(tick=rep(1:timestep,6),
                        mean=mean,
                        LLCI=LLCI,
                        ULCI=ULCI,
                        model=factor(rep(1:6,each=timestep)))

p2 <- lapply(1:6, function(i) {
  data <- plot.data2[plot.data2$model==i,]
  p <- ggplot(data,aes(x=tick,y=mean)) + geom_line(size=0.7) +
    annotate("text",x=10, y=ceiling(max(data[,'ULCI'])), label=models[i]) +
    geom_ribbon(aes(ymin=LLCI, ymax=ULCI),alpha=0.3) +
    xlab("") + ylab("") + theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none")

  return(p)
})

p2[[1]] <- p2[[1]] + theme(axis.text.x = element_blank())
p2[[2]] <- p2[[2]] + theme(axis.text.x = element_blank())
p2[[3]] <- p2[[3]] + theme(axis.text.x = element_blank())
p2[[4]] <- p2[[4]] + theme(axis.text.x = element_blank())

grid.arrange(p2[[1]],p2[[2]],p2[[3]],p2[[4]],p2[[5]],p2[[6]],
                     ncol=2, nrow = 3)


variance.plot <- print.ts.plot(output.data,"var")


## create 3-dim array (time = 1095, replication = 100, model = 6)
## out.array[[1]] == variance, out.array[[2]] == kurtosis, out.array[[3]] == ERindex
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


## simulation results, quantities reported in text
mean(out.array[['variance']][,,"model1"])
sd(out.array[['variance']][,,"model1"])
mean(out.array[['kurtosis']][,,"model1"])
sd(out.array[['kurtosis']][,,"model1"])

mean(out.array[['variance']][,,"model2"])
sd(out.array[['variance']][,,"model2"])
median(out.array[['kurtosis']][,,"model2"])
sd(out.array[['kurtosis']][,,"model2"])

mean(out.array[['ER']][,,"model1"])
sd(out.array[['ER']][,,"model1"])
mean(out.array[['ER']][,,"model2"])
sd(out.array[['ER']][,,"model2"])



mean(out.array[['variance']][,,"model3"])
sd(out.array[['variance']][,,"model3"])
mean(out.array[['kurtosis']][,,"model3"])
sd(out.array[['kurtosis']][,,"model3"])



