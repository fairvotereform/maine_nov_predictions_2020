
library(tidyverse)

multinomial_draw <- function(pA, pB, pexhaust, n){
  r <- rmultinom(1, n, prob = c(pA, pB, pexhaust))
  return(tibble(nA = r[1, 1], nB = r[2, 1], nExhaust = r[3, 1]))
}

senate_transfer <- function(df){
  
  n_samples <- 5e3
  
  # order: exhaust, gideon, collins
  linn_params <- 
    df %>%
    filter(first_choice == "Max Linn") %>%
    arrange(finalist_choice) %>%
    mutate(param = model_n + 1) %>% 
    pull(param)
  
  savage_params <- 
    df %>%
    filter(first_choice == "Lisa Savage") %>%
    arrange(finalist_choice) %>%
    mutate(param = model_n + 1) %>% 
    pull(param)
  
  s <- 
    data.frame(MCMCpack::rdirichlet(n_samples, linn_params),
               MCMCpack::rdirichlet(n_samples, savage_params)) %>%
    rename(p_linn_to_exhaust = X1, p_linn_to_gideon = X2, 
           p_linn_to_collins = X3,
           p_savage_to_exhaust = X1.1, p_savage_to_gideon = X2.1, 
           p_savage_to_collins = X3.1)
  
  return(s)
}

predict_senate <- function(df, 
                           collins_first_round, 
                           gideon_first_round,
                           linn_first_round,
                           savage_first_round){
  
  pred <- 
    senate_transfer(df) %>%
    mutate(
      collins_first_round = collins_first_round,
      gideon_first_round = gideon_first_round,
      linn_first_round = linn_first_round,
      savage_first_round = savage_first_round,
      sample_data_linn = purrr::pmap(
        .l = list(pA = p_linn_to_gideon, pB = p_linn_to_collins, 
                  pexhaust = p_linn_to_exhaust, n = linn_first_round),
        .f = multinomial_draw),
      sample_data_savage = purrr::pmap(
        .l = list(pA = p_savage_to_gideon, pB = p_savage_to_collins,
                  pexhaust = p_savage_to_exhaust, n = savage_first_round),
        .f = multinomial_draw)) %>%
    unnest(sample_data_linn) %>%
    rename(n_linn_to_gideon = nA, n_linn_to_collins = nB,
           n_linn_to_exhaust = nExhaust) %>%
    unnest(sample_data_savage) %>%
    rename(n_savage_to_gideon = nA, n_savage_to_collins = nB,
           n_savage_to_exhaust = nExhaust) %>%
    mutate(
      pred_gideon_count = gideon_first_round + n_linn_to_gideon + n_savage_to_gideon,
      pred_collins_count = collins_first_round + n_linn_to_collins + n_savage_to_collins,
      pred_gideon_perc = pred_gideon_count / (pred_gideon_count + pred_collins_count),
      pred_collins_perc = pred_collins_count / (pred_gideon_count + pred_collins_count)
    )
  
  return(pred)
}

president_transfer <- function(df){
  
  n_samples <- 5e3
  
  # order: trump, exhaust, biden
  hawkins_params <- 
    df %>%
    filter(first_choice == "Howie Hawkins") %>%
    arrange(finalist_choice) %>%
    mutate(param = model_n + 1) %>% 
    pull(param)
  
  jorgensen_params <- 
    df %>%
    filter(first_choice == "Jo Jorgensen") %>%
    arrange(finalist_choice) %>%
    mutate(param = model_n + 1) %>% 
    pull(param)
  
  delafuente_params <- 
    df %>%
    filter(first_choice == "Rocky De La Fuente") %>%
    arrange(finalist_choice) %>%
    mutate(param = model_n + 1) %>% 
    pull(param)
  
  s <-
    data.frame(MCMCpack::rdirichlet(n_samples, hawkins_params),
               MCMCpack::rdirichlet(n_samples, jorgensen_params),
               MCMCpack::rdirichlet(n_samples, delafuente_params)) %>%
    rename(p_hawkins_to_trump = X1, p_hawkins_to_exhaust = X2, 
           p_hawkins_to_biden = X3,
           p_jorgensen_to_trump = X1.1, p_jorgensen_to_exhaust = X2.1, 
           p_jorgensen_to_biden = X3.1,
           p_delafuente_to_trump = X1.2, p_delafuente_to_exhaust = X2.2, 
           p_delafuente_to_biden = X3.2)
  
  return(s)
}

predict_president <- function(df, 
                              trump_first_round, 
                              biden_first_round,
                              hawkins_first_round,
                              jorgensen_first_round,
                              delafuente_first_round){
  
  pred <-
    president_transfer(df) %>%
    mutate(
      trump_first_round = trump_first_round,
      biden_first_round = biden_first_round,
      hawkins_first_round = hawkins_first_round,
      jorgensen_first_round = jorgensen_first_round,
      delafuente_first_round = delafuente_first_round,
      sample_data_hawkins = purrr::pmap(
        .l = list(pA = p_hawkins_to_trump, pB = p_hawkins_to_biden, 
                  pexhaust = p_hawkins_to_exhaust, n = hawkins_first_round),
        .f = multinomial_draw),
      sample_data_jorgensen = purrr::pmap(
        .l = list(pA = p_jorgensen_to_trump, pB = p_jorgensen_to_biden,
                  pexhaust = p_jorgensen_to_exhaust, n = jorgensen_first_round),
        .f = multinomial_draw),
      sample_data_delafuente = purrr::pmap(
        .l = list(pA = p_delafuente_to_trump, pB = p_delafuente_to_biden,
                  pexhaust = p_delafuente_to_exhaust, n = delafuente_first_round),
        .f = multinomial_draw)) %>%
    unnest(sample_data_hawkins) %>%
    rename(n_hawkins_to_trump = nA, n_hawkins_to_biden = nB,
           n_hawkins_to_exhaust = nExhaust) %>%
    unnest(sample_data_jorgensen) %>%
    rename(n_jorgensen_to_trump = nA, n_jorgensen_to_biden = nB,
           n_jorgensen_to_exhaust = nExhaust) %>%
    unnest(sample_data_delafuente) %>%
    rename(n_delafuente_to_trump = nA, n_delafuente_to_biden = nB,
           n_delafuente_to_exhaust = nExhaust) %>%
    mutate(
      pred_biden_count = biden_first_round + n_hawkins_to_biden + 
        n_jorgensen_to_biden + n_delafuente_to_biden,
      pred_trump_count = trump_first_round + n_hawkins_to_trump + 
        n_jorgensen_to_trump + n_delafuente_to_trump,
      pred_biden_perc = pred_biden_count / (pred_biden_count + pred_trump_count),
      pred_trump_perc = pred_trump_count / (pred_biden_count + pred_trump_count)
    )

  return(pred)
}
