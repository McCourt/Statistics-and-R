one_prop_test = function(data, success = NULL, null = NULL, alt = "not equal", 
                         nsim = 15000, seed = NULL, print_summ = TRUE, print_plot = TRUE)
{  
  ############################
  ##### Check for Errors #####
  ############################
  
  # errors associated with null hypothesis
  if (null < 0 | null > 1 | !is.numeric(null)) 
    stop("Null value should be a numeric value between 0 and 1.") 
  if (is.null(null)) 
    stop("Missing null value.")
  
  # errors associated with alternative hypothesis
  if (!(alt %in% c("greater", "less", "not equal")))
    stop("Alternative hypothesis not specified properly, should be less, greater, or not equal.")
  
  # errors associated with data format
  if (is.null(data))
    stop("Missing data.")
  if (is.data.frame(data))
    stop("Data should be a vector, not a data frame.")
  
  
  ############################
  #####       Setup      #####
  ############################
  
  # load ggplot2 quietly
  suppressMessages(library(ggplot2, quietly = TRUE))
  
  # remove NAs
  data = data[!is.na(data)]
  
  # set seed, if provided
  if (!is.null(seed)) { set.seed(seed) }
  
  
  ############################
  ##### Calc Sample Stat #####
  ############################
  
  # set sample size
  n = length(data)
  
  # calculate observed number of successes
  n_suc = sum(data == success)
  
  # calculate proportion of successes
  stat = sum(data == success) / n 
  
  # set outcomes to sample from
  outcomes = levels(as.factor(data))
  # error if data has more than 2 levels
  if (length(outcomes) > 2) { stop("Input data has more than two levels.") }
  
  # set probability with which to sample
  if (which(outcomes == success) == 1) { p = c(null, 1-null) }
  if (which(outcomes == success) == 2) { p = c(1-null, null) }
  
  
  ############################
  #####   Simulate Null  #####
  ############################
  
  null_dist = data.frame(stat = rep(NA, nsim))
  for(i in 1:nsim){
    sim_sample = sample(outcomes, size = n, prob = p, replace = TRUE)
    null_dist$stat[i] = sum(sim_sample == success) / n
  }
  
  
  #############################
  ##### Calculate P-Value #####
  #############################
  
  # calculate number of simulated p-hats at least as extreme as observed p-hat
  if (alt  == "greater") { 
    nsim_extreme = sum(null_dist$stat > stat) 
  } else if (alt  == "less") { 
    nsim_extreme = sum(null_dist$stat < stat) 
  } else if (alt  == "not equal") {
    d = abs(stat-null)
    nsim_extreme = sum(null_dist$stat < stat-d) + sum(null_dist$stat > stat+d)
  }
  
  # calculate p-value
  p_value = nsim_extreme / nsim
  
  
  ############################
  #####  Print Summary   #####
  ############################
  
  if (print_summ){
    # print null hypothesis
    cat(paste("H0: p =", null, "\n"))
    
    # set alternative hypothesis sign
    if (alt == "not equal") { alt_sign = "!=" }
    if (alt == "greater")   { alt_sign = ">"  }
    if (alt == "less")      { alt_sign = "<"  }
    
    # print alternative hypothesis
    cat(paste("HA: p", alt_sign, null, "\n")) 
    
    # print summary statistics
    cat(paste("Summary stats: n =", n, ", number of successes =", n_suc, ", p-hat =", round(stat, 4), "\n"))
    
    # print p-value
    if (round(p_value, 4) == 0) { cat(paste("p-value < 0.0001\n")) }
    if (round(p_value, 4) >  0) { cat(paste("p-value =", round(p_value, 4), "\n")) }
  }
  
  
  ############################
  #####  Plot Null Dist  #####
  ############################
  
  if (print_plot)
  {  
    simdist_plot = ggplot(data = null_dist, aes(x = stat))
    if (nsim <= 100) { # dot plot if low number of simulations
      simdist_plot = simdist_plot + geom_dotplot() 
    } else { # histogram if high number of simulations
      simdist_plot = simdist_plot + geom_bar() 
    }
    suppressWarnings( suppressMessages( print( simdist_plot ) ) ) 
  }
  
  
  ############################
  #####      Return      #####
  ############################
  
  return(p_value)  
}
