create_time_series_population <- function(file.to.read, surr = FALSE, method.for.surrogate, which.surrogate){
  d = as.matrix(read.table(file.to.read, header = T))
  if(surr == TRUE){
    number.of.observations = nrow(d)
    number.of.species = ncol(d)
    species.name = colnames(d)
    new_surr = c()
    for (s in 1:ncol(d)){
      s1 = surrogate(d[,s], method = method.for.surrogate[which.surrogate])
      new_surr = cbind(new_surr,s1)
    }
    d = matrix(as.numeric(new_surr), number.of.observations, number.of.species)
    colnames(d) = species.name
  }
  return(d)
}
Standardizza <- function(X){
  ### This return y = (x-meanx)/stdx
  for(i in 1:ncol(X)){
    X[,i] = (X[,i]- mean(X[,i]))/sd(X[,i])
  }
  return(X)
}
Standardizza.test <- function(X, Y){
  ### X = test set
  ### Y = training set
  ### This return y = (x-meanY)/stdY
  for(i in 1:ncol(X)){
    X[,i] = (X[,i]- mean(Y[,i]))/sd(Y[,i])
  }
  return(X)
}
choose_focal_species <- function(focal_species, lista_variables){
  
  combinations = list()
  idx = which(lista_variables == focal_species)
  new.lista.variables = lista_variables[-idx]
  for(lst in 1:length(new.lista.variables)){
    combinations[[lst]] = c(lista_variables[idx], new.lista.variables[lst])
  }
  return(combinations)
  
}
surrogate_time_series_causality <- function(file.to.read, number_surr, method.for.surrogate, which.surrogate, emb, n.samp,
                                            dest, pred.spec,siz.lib){

  y1_ = y2_ = list()
  for(surr_ in 1:number_surr){
    surr_d = create_time_series_population(file.to.read, surr = TRUE, method.for.surrogate, which.surrogate)
    ts.sub = surr_d[,c(dest,pred.spec)]
    ts.sub = Standardizza(ts.sub)

    dest_xmap_pred <- ccm(as.data.frame(ts.sub), E = emb, lib_column = dest, 
                          target_column = pred.spec, lib_sizes = siz.lib, num_samples = n.samp, 
                          random_libs = TRUE, replace = TRUE)
    pred_xmap_dest <- ccm(as.data.frame(ts.sub), E = emb, lib_column = pred.spec, 
                          target_column = dest, lib_sizes = siz.lib, num_samples = n.samp, 
                          random_libs = TRUE, replace = TRUE)
    t_xmap_pr_means <- ccm_means(dest_xmap_pred)
    pr_xmap_t_means <- ccm_means(pred_xmap_dest)
    y1_[[surr_]] <- pmax(0, t_xmap_pr_means$rho)
    y2_[[surr_]] <- pmax(0, pr_xmap_t_means$rho)
  }
  return(list(y1_ = y1_, y2_ = y2_))
}
take.quantile <- function(X){
  ### X = surrogate ensemble
  surrogate.vector = t(sapply(X, FUN = cbind))
  new_surrogate.vector = sapply(as.data.frame(surrogate.vector), FUN = quantile, probs = seq(0,1,0.05), na.rm = T)
  return(list(surrogate_05 = new_surrogate.vector[2,], surrogate_95 = new_surrogate.vector[20,]))
}