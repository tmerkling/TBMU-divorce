#brms functions for model selection on list of models

# function creating a WAIC table when models are stored in a list
waic_wrapper <- function(...) {
  model_list <- list(...)
  if(!"x" %in% names(model_list)) {
    names(model_list)[1] <- "x"
    args <- c(model_list)
  }
  do.call(brms::WAIC, args)
}

# function calculating WAIC weights when models are stored in a list
weights_wrapper <- function(...) {
  model_list <- list(...)
  if (!"x" %in% names(model_list)) {
    names(model_list)[1] <- "x"
    args <- c(model_list, weights = "waic")
  }
  do.call(brms::model_weights, args)
}

# function creating a table combining WAIC, deltaWAIC and WAIC weights in decreasing order
waic_table <- function(x) {
  WAIC_results <- invoke(waic_wrapper, .x = x$model, model_names = x$name)
  min_WAIC <-  min(sapply(WAIC_results, "[[", 5)[-length(WAIC_results)]) # extracting WAIC value from the WAIC wrapper output
  deltas <- sapply(WAIC_results, "[[", 5)[-length(WAIC_results)] - min_WAIC # calculating deltaWAIC to order models
  modsel_table <- data.frame(
    Model =  names(WAIC_results)[-length(WAIC_results)], # removing the last item because it's the difference between models
    WAIC =  sapply(WAIC_results, "[[", 5)[-length(WAIC_results)],
    delta_WAIC =  sapply(WAIC_results, "[[", 5)[-length(WAIC_results)] - min_WAIC,
    WAIC_weights =  invoke(weights_wrapper, .x = x$model, model_names = x$name), 
    row.names = NULL
  )[order(deltas),]
  modsel_table$WAIC_cumsum <-  cumsum(modsel_table$WAIC_weights)
  print(modsel_table)
}


#function to subset the 95% confidence set and get the variables present in those models
sub_modvar <- function(x,y) { # x being tibble with all models, y being output of WAIC_table
  list(sub_mod <-  x[x$name %in% y$Model[1:which.min(abs(y$WAIC_cumsum - 0.95))],],
  sub_tab <-  tibble(
    name = sub_mod$name,
    variables = lapply(sub_mod$model, get_variables),
    fixed = lapply(variables, grep, pattern = "^b_.*", value = TRUE)),
  sub_var <-  unique(unlist(sub_tab$fixed)))
}

nam_fixef <- function(x){row.names(fixef(x))} # extracts coeff names from model list and

# subsetting model set depending on which variable is present to get model averaged estimates with no shrinkage
par_vec <-  function(z){ # z is output from sub_modvar function
        unique(unlist(lapply(z[1][[1]]$model, nam_fixef)))
  } #  gives a vector of parameters present in at least one model


post_avg_wrapper <- function(...) { # function to make posterior_average function work with lists of models
  model_list <- list(...)
  if (!"x" %in% names(model_list)) {
    names(model_list)[1] <- "x"
    args <- c(model_list, weights = "waic")
  }
  do.call(brms::posterior_average, args)
}



brm_mod_avg <- function(z) { # z is output from sub_modvar function
      parm <- par_vec(z)
      
      # creates a matrix of zero and one showing which parameters are in which model
      mod_tab <- data.frame(
      apply(sapply(parm, 
        function(y){as.numeric(grepl(y, lapply(z[1][[1]]$model, nam_fixef)))}) - 
        # substracts models with interactions for main effects, because they don't have the same meaning
      sapply(parm,
        function(y) {as.numeric(grepl(paste0(y,":"), lapply(z[1][[1]]$model, nam_fixef)))}) -
      sapply(parm,
        function(y) {as.numeric(grepl(paste0(":", y), lapply(z[1][[1]]$model, nam_fixef)))}),
            2, as.logical))
      
      list_mod <- apply(mod_tab,2,function(x) {z[1][[1]][x,]}) # makes subset of models for each variables to select only those with variable present
      names(list_mod) <- gsub("\\.",":",names(list_mod))
      
      tab_modavg <- data.frame(matrix(rep(0,length(z[3][[1]]) * 4), ncol = 4, dimnames = list(gsub("b_","",z[3][[1]]),c("Estimate","Est_Error","Q2.5","Q97.5")))) 
     
      for(i in 1:length(z[3][[1]]))
      {
        brms_parm = z[3][[1]][i]
        mod_parm = gsub("b_","", brms_parm)
        mod_set = list_mod[i][[1]]$model
        mod_names = list_mod[i][[1]]$name
        
        # calculates posterior average for variables appearing in more than one model
        if(length(mod_set) > 1){
          parm_modavg = posterior_summary(invoke(post_avg_wrapper, .x = mod_set, pars = brms_parm, model_names = mod_names))} 
        else {
          # extracts estimates for variables appearing in only one model
          mod_summ = fixef(mod_set[[1]])
          parm_modavg = mod_summ[which(row.names(mod_summ) == mod_parm),]  
        }
        
        tab_modavg[i,] <- parm_modavg
      }
      
      print(tab_modavg)
  }






