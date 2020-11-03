
library(tidyverse)
library(forcats)
library(cowplot)
library(glue)

first_round_plot <- function(df, fill_pairs){
  
  fmt_tresh <- 0.20
  
  if (4 == nrow(df)) {
    candidate_label_size = 20
    percent_label_size = 8
    percent_label_big = 0.08
    percent_label_small = 0.075
  } else {
    candidate_label_size = 16
    percent_label_size = 6.5
    percent_label_big = 0.07
    percent_label_small = 0.065
  }
  
  df_edit <- 
    df %>%
    mutate(perc = count/sum(count),
           perc = if_else(is.na(perc), 0, perc),
           candidate = fct_reorder(candidate, perc),
           perc_str = if_else(perc < 0.01 & perc > 0, 
                              "< 1%",
                              paste0(format(round(perc * 100, 2), nsmall = 2), "%")),
           str_position = if_else(perc >= fmt_tresh, percent_label_big, perc + percent_label_small),
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
                                     face = "bold"),
          axis.text.y = element_text(size = candidate_label_size, color = "#333333",
                                     face = "bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
          legend.position = "none") + 
    coord_flip() + 
    geom_text(aes(y = str_position, x = candidate, 
                  label = perc_str, color = str_color), 
              size = percent_label_size,
              fontface = 2, hjust = 0.5) + 
    scale_colour_manual(values=c("black" = "black", "white" = "white"))
  
  return(plt)
}

senate_first_round_plot <- function(df){
  senate_fill <- c("Susan Collins" = '#ff1d00', "Sara Gideon" = '#0115bc',
                   "Max Linn" = '#c297db', "Lisa Savage" = '#553285')
  first_round_plot(df, senate_fill)
}

president_first_round_plot <- function(df){
  president_fill <- c("Donald Trump" = '#ff1d00', "Joe Biden" = '#0115bc',
                      "Jo Jorgensen" = '#c297db', "Howie Hawkins" = '#553285',
                      "Rocky De La Fuente" = '#333333')
  first_round_plot(df, president_fill)
}

my_labels <- function(b){
  glue("{b * 100}%")
}

final_two_plot <- function(dem_perc, gop_perc){
  
  minx <- quantile(c(dem_perc, gop_perc), probs = 0.01)
  maxx <- quantile(c(dem_perc, gop_perc), probs = 0.99)
  
  minx <- min(c(dem_perc, gop_perc))
  maxx <- max(c(dem_perc, gop_perc))
  spanx <- maxx - minx
  
  if (spanx >= 0.20){
    interval_factor <- 0.05
    binwidth <- 0.0025
  } else if (spanx >= 0.15){
    interval_factor <- 0.03
    binwidth <- 0.001
  } else {
    interval_factor <- 0.02
    binwidth <- 0.001
  }
  
  round_minx <- floor(minx/interval_factor) * interval_factor
  round_maxx <- ceiling(maxx/interval_factor) * interval_factor
  
  scale_breaks <- seq(round_minx, round_maxx, by = interval_factor)
  scale_labels <- glue("{scale_breaks*100}%")

  dem_plot <-
    ggplot() + 
    geom_histogram(aes(x = dem_perc), bins = 95, #binwidth = binwidth,
                   fill = "#0115bc", color = "white", boundary = 0) + 
    geom_vline(aes(xintercept = 0.5), linetype = "dashed",
               size = 1.2, color = "#333333") + 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = "white"),
          axis.text.x = element_text(size = 10, 
                                     angle = 30, face = "bold"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank()) + 
    scale_x_continuous(labels = my_labels, 
                       breaks = scales::pretty_breaks(7)) + 
    coord_cartesian(xlim = c(minx, maxx)) + 
    scale_y_continuous(breaks = c()) + 
    xlab("")
  
  gop_plot <-
    ggplot() + 
    geom_histogram(aes(x = gop_perc), bins = 95, #binwidth = binwidth,
                   fill = "#ff1d00", color = "white", boundary = 0) + 
    geom_vline(aes(xintercept = 0.5), linetype = "dashed",
               size = 1.2, color = "#333333") + 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = "white"),
          axis.text.x = element_text(size = 10, angle = 30, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank()) + 
    scale_x_continuous(labels = my_labels,
                       breaks = scales::pretty_breaks(7)) + 
    coord_cartesian(xlim = c(minx, maxx)) + 
    scale_y_continuous(breaks = c()) + 
    xlab(glue("% of final round vote"))
  
  return(plot_grid(dem_plot, gop_plot, ncol = 1, align = "v"))
  
}


