# Compute Mann-Whitney Estimand from PMFs
mw_from_pmfs <-
  function(
    pmf_1,
    pmf_0,
    reverse_scale = FALSE
  ){
    joint_pmf_0_1 <- kronecker(X = pmf_0, Y = t(pmf_1))
    
    mw <-
      sum(joint_pmf_0_1[upper.tri(x = joint_pmf_0_1, diag = FALSE)]) +
      0.5*sum(diag(joint_pmf_0_1))
    
    return(
      ifelse(test = reverse_scale, yes = 1 - mw, no = mw)
    )
  }




# Convert Information into Power
information_to_power <-
  function(
    information,
    delta,
    delta_0 = 0,
    alpha = 0.05,
    sides = 2,
    mann_whitney = FALSE
  ){
    if(mann_whitney) {
      delta_0 <- 0.5
    }
    
    pnorm((delta - delta_0)*sqrt(information) - qnorm(p = 1 - (alpha/sides)))
  }




# Required information for fixed sample size design based on effect size
required_information_uninflated <-
  function(
    delta,
    delta_0 = 0,
    alpha = 0.05,
    sides = 2,
    power = 0.8
  ) {
    return(
      ((qnorm(p = 1 - (alpha/sides)) + qnorm(p = power))/(delta - delta_0))^2
    )
  }



# Required information: Fixed sample size with ordinal outcome
required_information_uninflated_mann_whitney <-
  function(
    mw,
    alpha = 0.05,
    sides = 2,
    power = 0.8
  ) {
    return(
      required_information_uninflated(
        delta = mw,
        delta_0 = 0.5,
        sides = sides,
        power = power
      )
    )
  }




# Adjust information for multiplicity using a group-sequential design
required_information_inflated <-
  function(
    information_fixed,
    gsd_design_specification
  ) {
    inflation_factor <-
      rpact::getDesignCharacteristics(gsd_design_specification)$inflationFactor
    return(information_fixed*inflation_factor)
  }




# Compute asymptotic information from sample size for difference in means
asymptotic_information_difference_means <-
  function(
    n_0,
    sigma_0,
    n_1,
    sigma_1
  ) {
    return(1/((sigma_0^2/n_0) + (sigma_1^2/n_1)))
  }




# Compute asymptotic information from sample size for difference in proportions
asymptotic_information_difference_proportions <-
  function(
    n_0,
    pi_0,
    n_1,
    pi_1
  ) {
    return(1/((1/n_0)*pi_0*(1 - pi_0) + (1/n_1)*pi_1*(1 - pi_1)))
  }




# Compute asymptotic information from sample size for M-W using
# Fay & Malinovsky (2018)
# DOI: 10.1002/sim.7890
asymptotic_information_mann_whitney_fm <-
  function(
    n_1,
    n_0,
    mw = NULL,
    pmf_1 = NULL,
    pmf_0 = NULL,
    adjust = TRUE
  ) {
    if(is.null(mw)){
      if(is.null(pmf_1) & is.null(pmf_0)){
        stop("Both `pmf_1` and `pmf_0` or `mw` must be supplied.")
      } else{
        mw <-
          mw_from_pmfs(
            pmf_1 = pmf_1,
            pmf_0 = pmf_0
          )
      }
    }
    
    if(adjust){
      n <- n_0 + n_1
      t <- rep(NA, times = length(n))
      ties <- list()
      for(i in 1:length(n)){
        ties[[i]] <- n_0[i]*pmf_0 + n_1[i]*pmf_1
        t[i] <- 1 - sum(ties[[i]]^3 - ties[[i]])/(n[i]^3 - n[i])
      }
    } else {
      t <- 1
    }
    
    var_mw <-
      t*(mw*(1 - mw)/(n_0*n_1))*
      (1 + (1/2)*(n_0 + n_1 - 2)*((mw/(1 + mw)) + ((1 - mw)/(2 - mw))))
    return(1/var_mw)
  }




# Compute asymptotic information from sample size for M-W using
# Zhao, Rahardja, & Qu (2008)
# DOI: 10.1002/sim.2912
asymptotic_information_mann_whitney_zrq <-
  function(
    n_0,
    n_1,
    pmf_1,
    pmf_0
  ) {
    n <- (n_0 + n_1)
    var_mw <- (1/(3*n))*(1 - sum(((pmf_0 + pmf_1)/2)^3))
    return(1/var_mw)
  }




# Compute asymptotic information from sample size for M-W using
# Wang, Chen, & Chow (2003)
# DOI: 10.1081/BIP-120024206
asymptotic_information_mann_whitney_wcc <-
  function(
    n_0,
    n_1,
    pmf_1,
    pmf_0
  ){
    mw_probs <-
      mw_probs_from_pmfs_wcc(
        pmf_1 = pmf_1,
        pmf_0 = pmf_0
      )
    
    return(
      with(as.list(mw_probs),
           n_0*n_1/(p1*(1 - p1) + (n_0 - 1)*(p2 - p1^2) +
                      (n_1 - 1)*(p3 - p1^2)))
    )
  }




# Compute asymptotic information from sample size for M-W using
# DeLong, DeLong, & Clarke-Pearson (1988)
# https://www.jstor.org/stable/2531595
asymptotic_information_mann_whitney_ddcp <-
  function(
    n_0,
    n_1,
    pmf_1,
    pmf_0,
    adjust = TRUE
  ) {
    stopifnot(
      identical(x = length(pmf_1), y = length(pmf_0))
    )
    
    categories <- 1:length(pmf_1)
    
    mw <-
      mw_from_pmfs(
        pmf_1 = pmf_1,
        pmf_0 = pmf_0
      )
    
    mw_probs <-
      expand.grid(
        y_t_1 = categories,
        y_t_2 = categories,
        y_c_1 = categories,
        y_c_2 = categories
      )
    
    mw_probs$joint_probability <-
      with(
        mw_probs,
        pmf_1[y_t_1]*pmf_1[y_t_2]*
          pmf_0[y_c_1]*pmf_0[y_c_2]
      )
    
    mw_probs$psi_t1_c1 <-
      with(mw_probs, 1*(y_t_1 > y_c_1) + 0.5*(y_t_1 == y_c_1))
    
    mw_probs$psi_t1_c2 <-
      with(mw_probs, 1*(y_t_1 > y_c_2) + 0.5*(y_t_1 == y_c_2))
    
    mw_probs$psi_t2_c1 <-
      with(mw_probs, 1*(y_t_2 > y_c_1) + 0.5*(y_t_2 == y_c_1))
    
    mw_probs$psi_t1_c1_x_psi_t1_c2 <-
      with(mw_probs, joint_probability*(psi_t1_c1 * psi_t1_c2))
    
    mw_probs$psi_t1_c1_x_psi_t2_c1 <-
      with(mw_probs, joint_probability*(psi_t1_c1 * psi_t2_c1))
    
    mw_probs$psi_t1_c1_squared <-
      with(mw_probs, joint_probability*(psi_t1_c1)^2)
    
    xi_10 <- sum(mw_probs$psi_t1_c1_x_psi_t1_c2) - mw^2
    xi_01 <- sum(mw_probs$psi_t1_c1_x_psi_t2_c1) - mw^2
    xi_11 <- sum(mw_probs$psi_t1_c1_squared) - mw^2
    
    # Adjustment: https://doi.org/10.1016/S1076-6332(97)80161-4
    # Hanley & Hajian-Tilaki (1997)
    adjustment <-
      ifelse(test = adjust, yes = mw*(1 - mw), no = 0)
    
    return((n_0*n_1)/((n_0 - 1)*xi_10 + (n_1 - 1)*xi_01 + xi_11 + adjustment))
  }




# Compute asymptotic information from sample size for M-W using
# Bamber (1975)
# DOI: 10.1016/0022-2496(75)90001-2
asymptotic_information_mann_whitney_bamber <-
  function(
    n_0,
    n_1,
    pmf_1,
    pmf_0,
    adjust = TRUE
  ) {
    stopifnot(
      identical(x = length(pmf_1), y = length(pmf_0))
    )
    
    categories <- 1:length(pmf_1)
    
    mw <-
      mw_from_pmfs(
        pmf_1 = pmf_1,
        pmf_0 = pmf_0
      )
    
    mw_probs <-
      expand.grid(
        y_t_1 = categories,
        y_t_2 = categories,
        y_c_1 = categories,
        y_c_2 = categories
      )
    
    mw_probs$joint_probability <-
      with(
        mw_probs,
        pmf_1[y_t_1]*pmf_1[y_t_2]*
          pmf_0[y_c_1]*pmf_0[y_c_2]
      )
    
    pr_t_neq_c <- 1 - sum(pmf_0*pmf_1)
    
    mw_probs$b_cct_1 <-
      with(mw_probs, joint_probability*((y_c_1 < y_t_1)&(y_c_2 < y_t_1)))
    mw_probs$b_cct_2 <-
      with(mw_probs, joint_probability*((y_t_1 < y_c_1)&(y_t_1 < y_c_2)))
    mw_probs$b_cct_3 <-
      with(mw_probs, joint_probability*((y_c_1 < y_t_1)&(y_t_1 < y_c_2)))
    mw_probs$b_cct_4 <-
      with(mw_probs, joint_probability*((y_c_2 < y_t_1)&(y_t_1 < y_c_1)))
    
    b_cct <- 
      sum(mw_probs$b_cct_1) + sum(mw_probs$b_cct_2) -
      sum(mw_probs$b_cct_3) - sum(mw_probs$b_cct_4)
    
    mw_probs$b_ttc_1 <-
      with(mw_probs, joint_probability*((y_t_1 < y_c_1)&(y_t_2 < y_c_1)))
    mw_probs$b_ttc_2 <-
      with(mw_probs, joint_probability*((y_c_1 < y_t_1)&(y_c_1 < y_t_2)))
    mw_probs$b_ttc_3 <-
      with(mw_probs, joint_probability*((y_t_1 < y_c_1)&(y_c_1 < y_t_2)))
    mw_probs$b_ttc_4 <-
      with(mw_probs, joint_probability*((y_t_2 < y_c_1)&(y_c_1 < y_t_1)))
    
    b_ttc <- 
      sum(mw_probs$b_ttc_1) + sum(mw_probs$b_ttc_2) -
      sum(mw_probs$b_ttc_3) - sum(mw_probs$b_ttc_4)
    
    var_mw <-
      (1/(4*n_0*n_1))*(
        pr_t_neq_c + (n_1 - 1)*b_ttc + (n_0 - 1)*b_cct -
          4*(n_1 + n_0 - 1)*(mw - 1/2)^2
      )
    
    return(1/var_mw)
  }




# Convert information to required sample size based on variances
information_to_n_continuous_1_to_1 <-
  function(
    information,
    sigma_0,
    sigma_1,
    round_up = TRUE
  ) {
    if(length(sigma_0) != length(sigma_1)){
      stop("`sigma_0` and `sigma_1` must have the same length.")
    }
    
    n_per_arm <- information*((sigma_0^2) + (sigma_1^2))
    if(round_up) n_per_arm <- ceiling(n_per_arm)
    return(data.frame("n_per_arm" = n_per_arm, "n_total" = 2*n_per_arm))
  }




# Convert information to required sample size based on proportions
information_to_n_binary_1_to_1 <-
  function(
    information,
    pi_0 = NULL,
    pi_1 = NULL,
    delta = NULL,
    round_up = TRUE
  ) {
    
    null_params <- is.null(pi_0) + is.null(pi_1) + is.null(delta)
    
    if(null_params != 1)
      stop("Only two of the following should be specified: ",
           "`pi_0` = ", pi_0, ", `pi_1` = ", pi_1, ", `delta` = ", delta)
    
    if(is.null(delta)) {
      delta <- (pi_1 - pi_0)
    } else if(is.null(pi_0)) {
      pi_0 <- (pi_1 - delta)
    } else if(is.null(pi_1)) {
      pi_1 <- (pi_0 + delta)
    }
    
    if(max(c(pi_0, pi_1)) >= 1 | min(c(pi_0, pi_1)) >= 1 |
       any(pi_0 == pi_1)) {
      stop("`pi_0` and `pi_1` must distinct, greater than 0, and less than 1.")
    }
    
    if(length(pi_0) != length(pi_1)){
      stop("`pi_0` and `pi_1` must have the same length.")
    }
    
    n_per_arm <-
      # ceiling(ceiling(2*information*((pi_0*(1 - pi_0)) + (pi_1*(1 - pi_1))))/2)
      2*information*((pi_0*(1 - pi_0)) + (pi_1*(1 - pi_1)))/2
    
    if(round_up) n_per_arm <- ceiling(n_per_arm)
    
    n_total <- 2*n_per_arm
    
    return(data.frame("n_per_arm" = n_per_arm, "n_total" = n_total))
  }




# Convert information to required sample size based on M-W
information_to_n_mann_whitney_1_to_1 <-
  function(
    information,
    mw = NULL,
    pmf_1 = NULL,
    pmf_0 = NULL,
    method = c("fm", "zrq", "wcc", "ddcp")[1],
    min_n = 1,
    max_n = 50000,
    adjust = TRUE,
    round_up = TRUE
  ){
    method <- tolower(unique(method))
    
    if(any(!method %in% c("fm", "zrq", "wcc", "ddcp"))){
      stop('Methods available: "fm", "zrq", "wcc", or "ddcp"')
    }
    
    if(any(c("zrq", "wcc", "ddcp") %in% method) & 
       (is.null(pmf_1) | is.null(pmf_0))){
      stop('`pmf_1` and `pmf_0` must be supplied for methods "zrq", "wcc", and "ddcp"')
    } else {
      if(is.null(mw) & (!(is.null(pmf_1) | is.null(pmf_0)))){
        mw <-
          mw_from_pmfs(
            pmf_1 = pmf_1,
            pmf_0 = pmf_0
          )
      }
    }
    
    sample_size <-
      merge(
        x = 
          data.frame(
            mw = mw,
            information = information),
        y = 
          expand.grid(
            method = method,
            n_per_arm = NA_real_,
            n_total = NA_real_,
            stringsAsFactors = FALSE
          )
      )
    
    for(i in 1:nrow(sample_size)){
      wrapper_kernel <- 
        switch(
          EXPR = sample_size$method[i],
          fm = asymptotic_information_mann_whitney_fm,
          wcc = asymptotic_information_mann_whitney_wcc,
          zrq = asymptotic_information_mann_whitney_zrq,
          ddcp = asymptotic_information_mann_whitney_ddcp
        )
      
      wrapper_args <-
        switch(
          EXPR = sample_size$method[i],
          fm = list(adjust = adjust, mw = sample_size$mw[i]),
          ddcp = list(adjust = adjust, pmf_1 = pmf_1, pmf_0 = pmf_0),
          list(pmf_1 = pmf_1, pmf_0 = pmf_0)
        )
      
      sample_size_i <-
        stats::uniroot(
          f = 
            function(n, info_target, kernel, ...){
              args <- list(...)[[1]]
              
              do.call(
                what = kernel,
                args = c(n_1 = n, n_0 = n, args)
              ) - info_target
            },
          interval = c(min_n, max_n),
          wrapper_args,
          info_target = sample_size$information[i],
          kernel = wrapper_kernel,
          wrapper_args
        )$root
      
      if(round_up) sample_size_i <- ceiling(sample_size_i)
      
      sample_size[i, c("n_per_arm", "n_total")] <-
        c(sample_size_i, 2*sample_size_i)
    }
    
    return(sample_size)
  }