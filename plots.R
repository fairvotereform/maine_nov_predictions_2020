
library(tidyverse)
library(forcats)
library(cowplot)
library(glue)

first_round_senate <- read_csv("senate_first_round.csv")
first_round_president <- read_csv("president_first_round.csv")

first_round_plot <- function(df, fill_pairs){
  
  fmt_tresh <- 0.1
  
  if (4 == nrow(df)) {
    candidate_label_size = 20
    percent_label_size = 8
  } else {
    candidate_label_size = 16
    percent_label_size = 6.5
  }
  
  df_edit <- 
    df %>%
    mutate(perc = count/sum(count),
           candidate = fct_reorder(candidate, perc),
           perc_str = glue("{format(round(perc * 100, 1), nsmall = 1)}%"),
           str_position = if_else(perc >= fmt_tresh, 0.07, perc + 0.05),
           str_color = if_else(perc >= fmt_tresh, "white", "black"),
           perc = if_else(perc > 0.6, 0.6, perc))
  
  plt <- 
    ggplot(df_edit) + 
    geom_col(aes(x = candidate, y = perc, fill = candidate), 
             width = 0.95) +
    geom_hline(yintercept = 0.5, linetype = "dashed", 
               color = "#a01460", size = 1) + 
    xlab("") + ylab("") + 
    scale_y_continuous(breaks = 0.5, labels = "50%", 
                       expand = c(0,0), limits = c(0, 0.61)) + 
    scale_fill_manual(values = fill_pairs) + 
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 12, color = "#a01460",
                                     face = "bold", family = "Montserrat"),
          axis.text.y = element_text(size = candidate_label_size, color = "#333333",
                                     face = "bold", family = "Montserrat"),
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
          legend.position = "none") + 
    coord_flip() + 
    geom_text(aes(y = str_position, x = candidate, 
                  label = perc_str, color = str_color), 
              size = percent_label_size, family = "Montserrat", 
              fontface = 2, hjust = 0.5) + 
    scale_colour_manual(values=c("black" = "black", "white" = "white"))
  
  return(plt)
}

senate_fill <- c("Susan Collins" = '#ff1d00', "Sara Gideon" = '#0115bc',
                 "Max Linn" = '#c297db', "Lisa Savage" = '#553285')

first_round_plot(first_round_senate, senate_fill)

ggsave("senate_first_round.png", 
       plot = first_round_plot(first_round_senate, senate_fill), 
       width = 7, height = 2, dpi = 1200)

president_fill <- c("Donald Trump" = '#ff1d00', "Joe Biden" = '#0115bc',
                 "Jo Jorgensen" = '#c297db', "Howie Hawkins" = '#553285',
                 "Rocky De La Fuente" = '#553285')

first_round_plot(first_round_president, president_fill)

ggsave("president_first_round.png", 
       plot = first_round_plot(first_round_president, president_fill), 
       width = 7, height = 2, dpi = 1200)

