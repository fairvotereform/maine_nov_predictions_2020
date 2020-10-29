
library(MCMCpack)
library(tidyverse)

multinomial_draw <- function(pCollins, pGideon, pExhaust, n){
  r <- rmultinom(1, n, prob = c(pCollins, pGideon, pExhaust))
  return(tibble(nCollins = r[1, 1], nGideon = r[2, 1], nExhaust = r[3, 1]))
}

nCollins <- 3e5
nGideon <- 1e5
nLinn <- 1e4
nSavage <- 2e4

tibble(candidate = c("Susan Collins", "Sara Gideon", "Max Linn", "Lisa Savage"),
           count = c(nCollins, nGideon, nLinn, nSavage)) %>%
  write_csv("senate_first_round.csv")

pred <- 
  data.frame(rdirichlet(1e4, c(51,51,51)), rdirichlet(1e4, c(100,50,1))) %>%
  rename(p_Linn_to_Collins = X1, p_Linn_to_Gideon = X2, 
         p_Linn_to_Exhaust = X3,
         p_Savage_to_Collins = X1.1, p_Savage_to_Gideon = X2.1, 
         p_Savage_to_Exhaust = X3.1) %>%
  mutate(
    redist_Linn = purrr::pmap(
      .l = list(pCollins = p_Linn_to_Collins, pGideon = p_Linn_to_Gideon, 
                pExhaust = p_Linn_to_Exhaust, n = nLinn),
      .f = multinomial_draw),
    redist_Savage = purrr::pmap(
      .l = list(pCollins = p_Savage_to_Collins, pGideon = p_Savage_to_Gideon, 
                pExhaust = p_Savage_to_Exhaust, n = nSavage),
      .f = multinomial_draw)
  ) %>%
  unnest(redist_Linn) %>%
  rename(n_Linn_to_Collins = nCollins, n_Linn_to_Gideon = nGideon, 
         n_Linn_to_Exhaust = nExhaust) %>%
  unnest(redist_Savage) %>%
  rename(n_Savage_to_Collins = nCollins, n_Savage_to_Gideon = nGideon, 
         n_Savage_to_Exhaust = nExhaust) %>%
  mutate(
    pred_Gideon_count = nGideon + n_Linn_to_Gideon + n_Savage_to_Gideon,
    pred_Collins_count = nCollins + n_Linn_to_Collins + n_Savage_to_Collins,
    pred_Gideon_perc = pred_Gideon_count / (pred_Gideon_count + pred_Collins_count),
    pred_Collins_perc = pred_Collins_count / (pred_Gideon_count + pred_Collins_count)
  )

write_csv(pred, "senate_pred.csv")


multinomial_draw <- function(pBiden, pTrump, pExhaust, n){
  r <- rmultinom(1, n, prob = c(pBiden, pTrump, pExhaust))
  return(tibble(nBiden = r[1, 1], nTrump = r[2, 1], nExhaust = r[3, 1]))
}

nBiden <- 3e5
nTrump <- 1e5
nJorgensen <- 1e4
nHawkins <- 2e4
nDeLaFuente <- 5e3

tibble(candidate = c("Joe Biden", "Donald Trump", "Jo Jorgensen", "Howie Hawkins", "Rocky De La Fuente"),
       count = c(nBiden, nTrump, nJorgensen, nHawkins, nDeLaFuente)) %>%
  write_csv("president_first_round.csv")

pred <- 
  data.frame(rdirichlet(1e4, c(51,51,51)), 
             rdirichlet(1e4, c(100,50,1)),
             rdirichlet(1e4, c(1,50,100))) %>%
  rename(p_Jorgensen_to_Biden = X1, p_Jorgensen_to_Trump = X2, 
         p_Jorgensen_to_Exhaust = X3,
         p_Hawkins_to_Biden = X1.1, p_Hawkins_to_Trump = X2.1, 
         p_Hawkins_to_Exhaust = X3.1,
         p_DeLaFuente_to_Biden = X1.2, p_DeLaFuente_to_Trump = X2.2, 
         p_DeLaFuente_to_Exhaust = X3.2) %>%
  mutate(
    redist_Jorgensen = purrr::pmap(
      .l = list(pBiden = p_Jorgensen_to_Biden, pTrump = p_Jorgensen_to_Trump, 
                pExhaust = p_Jorgensen_to_Exhaust, n = nJorgensen),
      .f = multinomial_draw),
    redist_Hawkins = purrr::pmap(
      .l = list(pBiden = p_Hawkins_to_Biden, pTrump = p_Hawkins_to_Trump, 
                pExhaust = p_Hawkins_to_Exhaust, n = nHawkins),
      .f = multinomial_draw),
    redist_DeLaFuente = purrr::pmap(
      .l = list(pBiden = p_DeLaFuente_to_Biden, pTrump = p_DeLaFuente_to_Trump, 
                pExhaust = p_DeLaFuente_to_Exhaust, n = nDeLaFuente),
      .f = multinomial_draw)
  ) %>%
  unnest(redist_Jorgensen) %>%
  rename(n_Jorgensen_to_Biden = nBiden, n_Jorgensen_to_Trump = nTrump, 
         n_Jorgensen_to_Exhaust = nExhaust) %>%
  unnest(redist_Hawkins) %>%
  rename(n_Hawkins_to_Biden = nBiden, n_Hawkins_to_Trump = nTrump, 
         n_Hawkins_to_Exhaust = nExhaust) %>%
  unnest(redist_DeLaFuente) %>%
  rename(n_DeLaFuente_to_Biden = nBiden, n_DeLaFuente_to_Trump = nTrump, 
         n_DeLaFuente_to_Exhaust = nExhaust) %>%
  mutate(
    pred_Trump_count = nTrump + n_Jorgensen_to_Trump + n_Hawkins_to_Trump + n_DeLaFuente_to_Trump,
    pred_Biden_count = nBiden + n_Jorgensen_to_Biden + n_Hawkins_to_Biden + n_DeLaFuente_to_Biden,
    pred_Trump_perc = pred_Trump_count / (pred_Trump_count + pred_Biden_count),
    pred_Biden_perc = pred_Biden_count / (pred_Trump_count + pred_Biden_count)
  )

write_csv(pred, "president_pred_w_DeLaFuente.csv")


pred <- 
  data.frame(rdirichlet(1e4, c(51,51,51)), 
             rdirichlet(1e4, c(100,50,1))) %>%
  rename(p_Jorgensen_to_Biden = X1, p_Jorgensen_to_Trump = X2, 
         p_Jorgensen_to_Exhaust = X3,
         p_Hawkins_to_Biden = X1.1, p_Hawkins_to_Trump = X2.1, 
         p_Hawkins_to_Exhaust = X3.1) %>%
  mutate(
    redist_Jorgensen = purrr::pmap(
      .l = list(pBiden = p_Jorgensen_to_Biden, pTrump = p_Jorgensen_to_Trump, 
                pExhaust = p_Jorgensen_to_Exhaust, n = nJorgensen),
      .f = multinomial_draw),
    redist_Hawkins = purrr::pmap(
      .l = list(pBiden = p_Hawkins_to_Biden, pTrump = p_Hawkins_to_Trump, 
                pExhaust = p_Hawkins_to_Exhaust, n = nHawkins),
      .f = multinomial_draw)
  ) %>%
  unnest(redist_Jorgensen) %>%
  rename(n_Jorgensen_to_Biden = nBiden, n_Jorgensen_to_Trump = nTrump, 
         n_Jorgensen_to_Exhaust = nExhaust) %>%
  unnest(redist_Hawkins) %>%
  rename(n_Hawkins_to_Biden = nBiden, n_Hawkins_to_Trump = nTrump, 
         n_Hawkins_to_Exhaust = nExhaust) %>%
  mutate(
    pred_Trump_count = nTrump + n_Jorgensen_to_Trump + n_Hawkins_to_Trump,
    pred_Biden_count = nBiden + n_Jorgensen_to_Biden + n_Hawkins_to_Biden,
    pred_Trump_perc = pred_Trump_count / (pred_Trump_count + pred_Biden_count),
    pred_Biden_perc = pred_Biden_count / (pred_Trump_count + pred_Biden_count)
  )

write_csv(pred, "president_pred_wo_DeLaFuente.csv")








  
