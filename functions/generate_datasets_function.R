# function requires values for N, repetetitions, and co_occur_matrices inputs 
# N (sample size) = number of 'subjects' (i.e. individual people, or patches, or animals etc...) 

generate_dataframe <- function(N, repetitions, co_occur_matrices) {

  #### Simulating General Parameters (Step 1) ####
  
  # environmental conditions as standard normal variables (such as if you were to standardise a continuous variable to unit variance)
  temp <- rnorm(N) # create a vector for a temperature variable that is randomly drawn from normal distribution that is the length of the sample size
  humidity <- rnorm(N) # create a vector for a humidity variable that is randomly drawn from a normal distribution that is the length of the sample size
  
  # number of speciesites, species, OTUs etc...
  N_species <- 2 # number of pathogens (the outcome/independent variables variables)
  
  # species average prevalences
  alphas <- rnorm(N_species, mean = -0.5, sd = 0.5) # alpha values (representing the average prevalence values for each parasite) drawn from a random distribution with a mean of -0.75 and a standard deviation of 0.5. 
  
  # species average associations with changes in temperature temp_betas
   beta_temp <- rnorm(N_species, mean = 0.3, sd = 0.1) # the beta coefficient (i.e. the effect) of temperature on each parasite occurrence
  
   # species average associations with changes in humidity humidity_betas
  beta_hum <- rnorm(N_species, mean = -0.3, sd = 0.1) # the beta coefficient (i.e. the effect) of humidity on each parasite occurrence
  
  # create empty matrices/lists that will be filled during colonisation events in next steps
  species_probs <- matrix(NA, nrow = N, ncol = N_species)
  species_occurrences <- matrix(NA, nrow = N, ncol = N_species)
  state_init <- matrix(NA, nrow = N, ncol = 1)
  prob_dat <- matrix(NA, nrow = N, ncol = N_species)
  colnames(prob_dat) <- c("finalprob_sp1", "finalprob_sp2")
  
  results <- list()
  
  # feed in the co-occurrence matrices for each community and transform diagonals to 0. 
  for (i in 1:repetitions) {
    co_occur_direct <- co_occur_matrices[[i]] # i represents each community 
    diag(co_occur_direct) <- rep(0, N_species) #transforms the diagonal values on the matrix to 0 as the one parasite cannot be associated with itself
    
 # loop over all patches / individuals, allow one species to attempt a colonisation first 
 # and then allow all other species the opportunity to colonise (priority effects)
    for (j in 1:N) {  # Changed loop variable from i to j. j represents each observation. 
      # probability of independent colonisation for each species
      col_probs <- boot::inv.logit(alphas + beta_temp * temp[j] + beta_hum * humidity[j])
      
      # calculate the probability that the patch / individual will not be colonised after 
      # the first round of 'exposure'
      empty_prob <- prod(1 - col_probs)
      
      # create the full probability vector for each possible state
      prob_vector <- c(col_probs, empty_prob)
      
      # generate a state vector of length = N_species + 1 (+1 is for the uncolonised state)
      state_matrix <- rep(0, length(prob_vector))
      
      # sample a categorical distribution based on prob_vector probabilities
      state <- sample(1:length(prob_vector), size = 1, prob = prob_vector)
      
      # track what the initial state was for use as a colonisation history variable later on
      state_init[j,] <- state
      
      # fill in the state vector
      state_matrix[state] <- 1
      
      # create final occurrence vector by running a second round of 'exposure'; here, if 
      # species 1 colonised after round one, then the probability that other species will
      # colonise will depend on their particular co-occurrence associations with species 1, as well
      # as their own environmental affinities
      final_probs <- rep(0, length(prob_vector))
      final_probs[1:N_species] <- boot::inv.logit((alphas + beta_temp * temp[j] +
                                                     beta_hum * humidity[j]) + 
                                                    (state_matrix[-c(N_species+1)] %*% 
                                                       co_occur_direct))
      
      species_probs[j, ] <- final_probs[1:N_species]
      
      # ensure that any species that did colonise after round one is still present after 
      # round two
      final_probs[state] <- 1
      final_probs <- final_probs[1:N_species] 
      
      # Bernoulli draws of the final probabilities for each species
      species_occurrences[j, ] <- rbinom(N_species, 1, final_probs)
      
      #extract final probabilities for comparison
      prob_dat[j, "finalprob_sp1"] <- final_probs[1]
      prob_dat[j, "finalprob_sp2"] <- final_probs[2]
      
    }
    
    # convert to dataframe for JSDM / regression modelling
    model_data <- data.frame(species_occurrences)
    colnames(model_data) <- paste0('species', 1:N_species)
    
    # add in original predictor variables
    model_data$temp <- temp
    model_data$hum <- humidity
    
    # add in an indicator of the initial state, setting the 'empty' state
    # as the baseline level
    model_data$init <- state_init[,1]+1 # add information in about which pathogen was present first 
    model_data$init[model_data$init == (N_species + 2)] <- 1
    
    model_data <- cbind(model_data, prob_dat)
    
    # convert to model matrix form so that certain levels of the factor can be dropped
    library(dplyr)
    init_matrix <- as.data.frame(model.matrix(~as.factor(model_data$init) - 1))
    colnames(init_matrix) <- paste0('state', 1:length(unique(model_data$init)))
    model_data %>%
      dplyr::select(-init) %>%
      dplyr::bind_cols(init_matrix) -> model_data
 
    # GLMs with priority effects
    # In mod1, we are interested in the contrast between the
    # site being unoccupied after round 1 vs species 2 being present 
    # after found 1)
    mod1 <- glm(species1 ~ state3 + temp + hum, 
                data = model_data %>%
                  dplyr::filter(state2 == 0), 
                family = 'binomial')
    # In mod2, we are interested in the contrast between the
    # site being unoccupied after round 1 vs species 1 being present 
    # after found 1)
    mod2 <- glm(species2 ~ state2 + temp + hum, 
                data = model_data %>%
                  dplyr::filter(state3 == 0), 
                family = 'binomial')
    
    # GLMs without priority effects, which don't know anything
    # about the state information
    mod1b <- glm(species1 ~ as.factor(species2) + 
                   temp + hum, data = model_data, 
                 family = 'binomial')
    mod2b <- glm(species2 ~ as.factor(species1) + 
                   temp + hum, data = model_data, 
                 family = 'binomial')
    
    # the true interaction coefficients from the co-occurrence matrix 
    true_sp1_coef <- co_occur_direct[2, 1]
    true_sp2_coef <- co_occur_direct[1, 2]
    
    #predicted interaction coefficients from the two models 
    statemod_sp1_coef <- coef(mod1)[2]
    statemod_sp2_coef <- coef(mod2)[2]
    nullmod_sp1_coef <- coef(mod1b)[2] 
    nullmod_sp2_coef <- coef(mod2b)[2]
    
    # model fitted alpha values used to generate predictions
    statemod_sp1_alpha <- coef(mod1)[1]
    statemod_sp2_alpha <- coef(mod2)[1]
    nullmod_sp1_alpha <- coef(mod1b)[1] 
    nullmod_sp2_alpha <- coef(mod2b)[1]
    
    # extract the true final probabilities from the dataset and group them based on colonisation state in event 1 
    finalprob_sp1_state3 <- model_data$finalprob_sp1[model_data$state3 == 1]
    finalprob_sp1_state1 <- model_data$finalprob_sp1[model_data$state1 == 1]
    finalprob_sp2_state2 <- model_data$finalprob_sp2[model_data$state2 == 1]
    finalprob_sp2_state1 <- model_data$finalprob_sp2[model_data$state1 == 1]
    
    # specify length based on the length of the final probability observations, so that we can generate the same number of predictions (to compare with brier scores)
    length_sp1_state3 <- length(finalprob_sp1_state3)
    length_sp1_state1 <- length(finalprob_sp1_state1)
    length_sp2_state2 <- length(finalprob_sp2_state2)
    length_sp2_state1 <- length(finalprob_sp2_state1)
    
    # specify parameters for predictions
    pred_sp1_state3_dat <- data.frame(state1 = 0, state3 = 1, temp = rnorm(length_sp1_state3), hum = rnorm(length_sp1_state3))
    pred_sp1_state1_dat <- data.frame(state1 = 1, state3 = 0, temp = rnorm(length_sp1_state1), hum = rnorm(length_sp1_state1))
    
    pred_sp2_state2_dat <- data.frame(state1 = 0, state2 = 1, temp = rnorm(length_sp2_state2), hum = rnorm(length_sp2_state2))
    pred_sp2_state1_dat <- data.frame(state1 = 1, state2 = 0, temp = rnorm(length_sp2_state1), hum = rnorm(length_sp2_state1))
    
    pred_sp1_pres_dat <- data.frame(species2 = 1, temp = rnorm(length_sp1_state3), hum = rnorm(length_sp1_state3))
    pred_sp1_abs_dat <- data.frame(species2 = 0, temp = rnorm(length_sp1_state1), hum = rnorm(length_sp1_state1))
    
    pred_sp2_pres_dat <- data.frame(species1 = 1, temp = rnorm(length_sp2_state2), hum = rnorm(length_sp2_state2))
    pred_sp2_abs_dat <- data.frame(species1 = 0, temp = rnorm(length_sp2_state1), hum = rnorm(length_sp2_state1))

    # generate predictions usign the specified parameters and the fitted glm models 
    statemod_sp1_prob_state3 <- boot::inv.logit(predict(mod1, newdata = pred_sp1_state3_dat))
    statemod_sp1_prob_state1 <- boot::inv.logit(predict(mod1, newdata = pred_sp1_state1_dat))
    
    statemod_sp2_prob_state2 <- boot::inv.logit(predict(mod2, newdata = pred_sp2_state2_dat))
    statemod_sp2_prob_state1 <- boot::inv.logit(predict(mod2, newdata = pred_sp2_state1_dat))
    
    nullmod_sp1_prob_pres <-   boot::inv.logit(predict(mod1b, newdata = pred_sp1_pres_dat))
    nullmod_sp1_prob_abs <-   boot::inv.logit(predict(mod1b, newdata = pred_sp1_abs_dat))
    
    nullmod_sp2_prob_pres <- boot::inv.logit(predict(mod2b, newdata = pred_sp2_pres_dat))
    nullmod_sp2_prob_abs <- boot::inv.logit(predict(mod2b, newdata = pred_sp2_abs_dat))
    
    # combine the final probabilities into a vector 
    true_probs <- c(finalprob_sp1_state3, finalprob_sp1_state1, finalprob_sp2_state2, finalprob_sp2_state1)
    
    # combine the predicted probabilities from both models into vectors 
    statemod_preds <- c(statemod_sp1_prob_state3, statemod_sp1_prob_state1, statemod_sp2_prob_state2, statemod_sp2_prob_state1)
    nullmod_preds <- c(nullmod_sp1_prob_pres, nullmod_sp1_prob_abs, nullmod_sp2_prob_pres, nullmod_sp2_prob_abs)

    # calculate brier scores for each prediction vs. true probability and take the mean (this is a summary for the community)
    statemod_brier <- (statemod_preds - true_probs)^2
    mean_statemod_brier <- mean(statemod_brier)
    
    nullmod_brier <- (nullmod_preds - true_probs)^2
    mean_nullmod_brier <- mean(nullmod_brier)
    
    # combine results into a dataframe to be used for further visualisation and analysis 
    results[[i]] <- data.frame(
    true_sp1_coef = true_sp1_coef,
     true_sp2_coef = true_sp2_coef,
      statemod_sp1_coef = statemod_sp1_coef,
      statemod_sp2_coef = statemod_sp2_coef,
      nullmod_sp1_coef = nullmod_sp1_coef,
      nullmod_sp2_coef = nullmod_sp2_coef,
      mean_statemod_brier = mean_statemod_brier, 
      mean_nullmod_brier = mean_nullmod_brier,
      statemod_sp1_alpha = statemod_sp1_alpha, 
      statemod_sp2_alpha = statemod_sp2_alpha, 
      nullmod_sp1_alpha = nullmod_sp1_alpha, 
      nullmod_sp2_alpha = nullmod_sp2_alpha 
    )
  
  }  
  
  final_results <- do.call(rbind, results)
  rownames(final_results) <- 1:nrow(final_results)
  
  return(final_results)
}




