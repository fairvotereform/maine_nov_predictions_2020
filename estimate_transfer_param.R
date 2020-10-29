


## parse survey and write out param estimate tables

senate_transfer <- tibble(from = rep(c("Linn", "Savage"), each = 3),
                          to = rep(c("Biden", "Trump", "Exhaust"), 2),
                          dirich_param = c(5, 10, 3, 20, 4, 12))

pres1_transfer <- tibble(from = rep(c("Linn", "Savage"), each = 3),
                          to = rep(c("Biden", "Trump", "Exhaust"), 2),
                          dirich_param = c(5, 10, 3, 20, 4, 12))
