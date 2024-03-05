library(dplyr)


setwd("C:/Users/uqfpowe1/OneDrive - The University of Queensland/Documents/PhD/Writing Documents/Study_3-Perspectives_Study/r_scripts/clean_rscripts")

# Generating datasets with different degrees of interactions between pathogens 

N_species <- 2
source("functions/generate_datasets_function.R")

###### 1. Positive Symmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_1 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, 1.25, 2), ncol = N_species, nrow = N_species) #generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct[1, 2] <- co_occur_direct[2, 1] #ensuring or this matrix that co-occurrence is positive and symmetrical
  co_occur_direct
}, simplify = FALSE)

pos_sym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_1)

save(pos_sym_dat, file = "data/pos_sym_dat.rda")

###### 2. Negative Symmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_2 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, -2, -1.25), ncol = N_species, nrow = N_species) # generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct[1, 2] <- co_occur_direct[2, 1] #ensuring or this matrix that co-occurrence is negative and symmetrical
  co_occur_direct
}, simplify = FALSE)


neg_sym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_2)

save(neg_sym_dat, file = "data/neg_sym_dat.rda")

###### 3. Positive Asymmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_3 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, 1.25, 2), ncol = N_species, nrow = N_species) # generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct 
}, simplify = FALSE)


pos_asym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_3)

save(pos_asym_dat, file = "data/pos_asym_dat.rda")

###### 4. Negative Asymmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_4 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, -2, -1.25), ncol = N_species, nrow = N_species) # generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct
}, simplify = FALSE)

neg_asym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_4)

save(neg_asym_dat, file = "data/neg_asym_dat.rda")

###### 5. Small Opposing Asymmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_5 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, 0.25, 1), ncol = N_species, nrow = N_species) #generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct[1,2] <- co_occur_direct[2,1]* runif(1, -1, -0.5)
  co_occur_direct
}, simplify = FALSE)


sml_opp_asym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_5)

save(sml_opp_asym_dat, file = "data/sml_opp_asym_dat.rda")

###### 6. Large Opposing Asymmetric ######

# co-occurence effects as a square matrix. replicated 1000 times to generate 1000 different co-occurrence matrices for each community
co_occur_matrix_6 <- replicate(1000, {
  co_occur_direct <- matrix(runif(N_species*2, 1.25, 2), ncol = N_species, nrow = N_species) #generates a 2 by 2 matrix of associations randomly drawn from a normal distribution between the two parasites that signifies the assymmetric associations between each parasite occurrence
  co_occur_direct[1,2] <- co_occur_direct[2,1]* runif(1, -1, -0.25)
  co_occur_direct
}, simplify = FALSE)

lge_opp_asym_dat <- generate_dataframe(1000, 1000, co_occur_matrix_6)

save(lge_opp_asym_dat, file = "data/lge_opp_asym_dat.rda")

