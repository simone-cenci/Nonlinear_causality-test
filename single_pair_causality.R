rm(list = ls())
suppressMessages(library(rEDM))
suppressMessages(library(tikzDevice))
suppressMessages(library(fractal))
source('fun.r')
#### Perform a causality test on a multivariate time series comparing the results with
#### a null expectation using surrogate time series
#### Data are taken from http://www.pnas.org/content/112/20/6389
method.for.surrogate = c('phase', 'ce', 'aaft', 'dh')
file.to.read = 'average_by_month.txt'
d = create_time_series_population(file.to.read, surr = FALSE, method.for.surrogate, 0)
variable.names = colnames(d)
####
variabili = variable.names
#### number of realizations for the surrogate time series
num_surr = 20
### choose the two variable to cross map
dest = 'Mussels'
pred.spec = 'Barnacles'
save.plt = F
if(save.plt == TRUE){
  nome.file = paste(dest, '_', pred.spec, '.pdf', sep = '')
  pdf(nome.file,width=6,height=4,paper='special') 
}
legend.names = c(paste(dest, 'xmap', pred.spec, sep = ' '),
                 paste(pred.spec, 'xmap', dest, sep = ' '))

ts.sub = d[,c(dest,pred.spec)]
ts.sub = Standardizza(ts.sub)
siz.lib = seq(1, 240, by = 2)
### For plot
opposite.direction = seq(240,1,by = -2)
###
n.samp = 50
emb = 2
dest_xmap_pred <- ccm(as.data.frame(ts.sub), E = emb, lib_column = dest, 
                      target_column = pred.spec, lib_sizes = siz.lib, num_samples = n.samp, 
                      random_libs = TRUE, replace = TRUE)
pred_xmap_dest <- ccm(as.data.frame(ts.sub), E = emb, lib_column = pred.spec, 
                      target_column = dest, lib_sizes = siz.lib, num_samples = n.samp, 
                      random_libs = TRUE, replace = TRUE)

t_xmap_pr_means <- ccm_means(dest_xmap_pred)
pr_xmap_t_means <- ccm_means(pred_xmap_dest)

y1 <- pmax(0, t_xmap_pr_means$rho)
y2 <- pmax(0, pr_xmap_t_means$rho)
## Look at causality with surrogate time series
surrogate_causality = surrogate_time_series_causality(file.to.read, num_surr, method.for.surrogate, 
                                                      3, emb, n.samp, dest, pred.spec,siz.lib)
## Now take the 5th and 95th percentile of the surrogate data
surrogate.quantile.y1 = take.quantile(surrogate_causality$y1_)
new_surrogate.vector.y1_05 = surrogate.quantile.y1$surrogate_05
new_surrogate.vector.y1_95 = surrogate.quantile.y1$surrogate_95
##
surrogate.quantile.y2 = take.quantile(surrogate_causality$y2_)
new_surrogate.vector.y2_05 = surrogate.quantile.y2$surrogate_05
new_surrogate.vector.y2_95 = surrogate.quantile.y2$surrogate_95
##### Plot in thick the true causality and in shaded area the surrogate
plot(t_xmap_pr_means$lib_size, y1, type = "l", lwd = 2, col = "red", xlab = "Library Size", 
     ylab = "rho", ylim = c(0, 1))
lines(pr_xmap_t_means$lib_size, y2, col = "blue", lwd = 2)
### Surrogate plots
redtrans <- rgb(255, 0, 0, 95, maxColorValue=255) 
bluetrans <- rgb(0, 0, 255, 95, maxColorValue=255) 
polygon(c(siz.lib,opposite.direction),
        c(new_surrogate.vector.y1_05 ,rev(new_surrogate.vector.y1_95)),col=redtrans,border=NA)
polygon(c(siz.lib,opposite.direction),
        c(new_surrogate.vector.y2_05,rev(new_surrogate.vector.y2_95)),col=bluetrans,border=NA)
legend(x = "topleft", legend = legend.names, 
       col = c("red", "blue"), lwd = 2, bty = "n", inset = 0.02, cex = 1)
if(save.plt == TRUE){
dev.off()
}