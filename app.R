# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# dir.create('~/.fonts')
# file.copy("www/ProximaNovaExtrabold.otf", "~/.fonts")
# file.copy("www/ProximaNovaSemibold.otf", "~/.fonts")
# system('fc-cache -f -v ~/.fonts')

library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(shinyalert)
library(showtext)

source("predictions.R")
source("plots.R")

# font_add("ProximaNova",
#         regular = "ProximaNova/ProximaNova-Semibold.otf",
#         bold = "ProximaNova/ProximaNova-Extrabold.otf")
# print(font_families())
# showtext_auto()

# read in poll summaries
senate_poll <- read_csv("poll_summary/senate.csv")
president_d1_poll <- read_csv("poll_summary/president_d1.csv")
president_d2_poll <- read_csv("poll_summary/president_d2.csv")
president_d12_poll <- read_csv("poll_summary/president_d12.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    useShinyalert(),  # Set up shinyalert
    
    # title
    titlePanel("Maine November 2020 Predictions"),
    h5("app code last updated: Nov 03, 2020 15:59"),
    fluidRow(
        column(3, wellPanel(
            h4("Senate"),
            hr(),
            h5("First Round Count:"),
            numericInput("collins_first_round", "Collins:", 1, min = 1),
            numericInput("gideon_first_round", "Gideon:", 1, min = 1),
            numericInput("linn_first_round", "Linn:", 0),
            numericInput("savage_first_round", "Savage:", 0),
            downloadButton("senate_predict", "Predict")
            )),
        column(3, wellPanel(
            h4("President - District 2"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d2", "Biden:", 1, min = 1),
            numericInput("trump_first_round_d2", "Trump:", 1, min = 1),
            numericInput("jorgensen_first_round_d2", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d2", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d2", "De La Fuente:", 0),
            downloadButton("pres_predict_d2", "Predict")
        )),
        column(3, wellPanel(
            h4("President - District 1"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d1", "Biden:", 1, min = 1),
            numericInput("trump_first_round_d1", "Trump:", 1, min = 1),
            numericInput("jorgensen_first_round_d1", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d1", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d1", "De La Fuente:", 0),
            downloadButton("pres_predict_d1", "Predict")
        )),
        column(3, wellPanel(
            h4("President - Both Districts"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d12", "Biden:", 1, min = 1),
            numericInput("trump_first_round_d12", "Trump:", 1, min = 1),
            numericInput("jorgensen_first_round_d12", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d12", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d12", "De La Fuente:", 0),
            downloadButton("pres_predict_d12", "Predict")
        ))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$senate_predict <- downloadHandler(
        #filename = function(){glue("senate_{format(Sys.time(), \"%b_%d_%Y_%H%M\")}.zip")},
        filename = function(){glue("senate.zip")},
        content = function(file) {
            
            withProgress({
                
                incProgress(1/4)
                
                senate_first_round <-
                    data.frame(candidate = c("Susan Collins", "Sara Gideon",
                                             "Max Linn", "Lisa Savage"),
                               count = c(input$collins_first_round, 
                                         input$gideon_first_round,
                                         input$linn_first_round,
                                         input$savage_first_round))
                
                senate_predictions <- predict_senate(senate_poll, 
                                                     input$collins_first_round, 
                                                     input$gideon_first_round,
                                                     input$linn_first_round,
                                                     input$savage_first_round)
                
                win_prob <- 
                    senate_predictions %>%
                    mutate(gideon_win = pred_gideon_perc > 0.5,
                           collins_win = pred_collins_perc > 0.5,
                           tie = (pred_gideon_perc == 0.5)) %>%
                    summarise(prob_gideon_win = round(mean(gideon_win), 2),
                              prob_collins_win = round(mean(collins_win), 2),
                              prob_tie = round(mean(tie), 2))
                    
                incProgress(2/4)
                
                tryCatch({
                    
                    senate_first_round_plot <- 
                        senate_first_round_plot(senate_first_round)
                    
                    senate_final_two_plot <- 
                        final_two_plot(senate_predictions$pred_gideon_perc,
                                       senate_predictions$pred_collins_perc)
                    
                    #showtext_auto()
                    
                    incProgress(3/4)
                    
                    setwd(tempdir())
                    
                    write_csv(senate_predictions, "predictions.csv")
                    write_csv(senate_first_round, "first_round.csv")
                    write_csv(win_prob, "win_probability.csv")
                    
                    # png("first_round.png", width = 7, height = 2, units = "in", res = 1200)
                    # senate_first_round_plot
                    # dev.off()
                    # 
                    # png("final_two.png", width = 6, height = 3.5, units = "in", res = 1200)
                    # senate_final_two_plot
                    # dev.off()
                    
                    ggsave("first_round.png", plot = senate_first_round_plot,
                           width = 7, height = 2, dpi = 1200, device = "png")
                    ggsave("final_two.png", plot = senate_final_two_plot,
                          width = 6, height = 3.5, dpi = 1200, device = "png")

                    #showtext_end()
                    
                }, warning = function(w) {
                    
                    print(w)
                    
                    shinyalert("Font problem", 
                               "Ignore the current file download. There was a problem rendering the right fonts. This happens sometimes. Re-click 'predict' button to re-plot.",
                               type = "warning")
    
                }, error = function(e) {
                    print(e)
                }, finally = {
                })
                
                incProgress(4/4)
                
            }, 
            message = 'making new predictions',
            detail = "senate",
            value = 0)

            zip(zipfile=file, 
                files=c("first_round.png", 
                        "win_probability.csv",
                        "final_two.png",
                        "first_round.csv",
                        "predictions.csv"))
        },
        contentType = "application/zip"
    )
    
    output$pres_predict_d1 <- downloadHandler(
        filename = function(){glue("president_district1.zip")},
        content = function(file) {
            
            withProgress({
                
                incProgress(1/4)
                
                president_first_round <-
                    data.frame(candidate = c("Joe Biden", "Donald Trump",
                                             "Jo Jorgensen", "Howie Hawkins",
                                             "Rocky De La Fuente"),
                               count = c(input$biden_first_round_d1, 
                                         input$trump_first_round_d1,
                                         input$jorgensen_first_round_d1,
                                         input$hawkins_first_round_d1,
                                         input$delafeunte_first_round_d1))
                
                president_predictions <- 
                    predict_president(president_d1_poll, 
                                      input$trump_first_round_d1, 
                                      input$biden_first_round_d1,
                                      input$hawkins_first_round_d1,
                                      input$jorgensen_first_round_d1,
                                      input$delafeunte_first_round_d1)
                
                win_prob <- 
                    president_predictions %>%
                    mutate(biden_win = pred_biden_perc > 0.5,
                           trump_win = pred_trump_perc > 0.5,
                           tie = (pred_biden_perc == 0.5)) %>%
                    summarise(prob_biden_win = round(mean(biden_win), 2),
                              prob_trump_win = round(mean(trump_win), 2),
                              prob_tie = round(mean(tie), 2))
                
                incProgress(2/4)
                
                tryCatch({
                
                    president_first_round_plot <- 
                        president_first_round_plot(president_first_round)
                    
                    president_final_two_plot <- 
                        final_two_plot(president_predictions$pred_biden_perc,
                                       president_predictions$pred_trump_perc)
                    
                    #showtext_auto()
                    
                    incProgress(3/4)
                    
                    setwd(tempdir())
                    
                    write_csv(president_predictions, "predictions.csv")
                    write_csv(president_first_round, "first_round.csv")
                    write_csv(win_prob, "win_probability.csv")
                    
                    # png("first_round.png", width = 7, height = 2, units = "in", res = 1200)
                    # president_first_round_plot
                    # dev.off()
                    # 
                    # png("final_two.png", width = 6, height = 3.5, units = "in", res = 1200)
                    # president_final_two_plot
                    # dev.off()
                    
                    ggsave("first_round.png", plot = president_first_round_plot,
                          width = 7, height = 2, dpi = 1200, device = "png")
                    ggsave("final_two.png", plot = president_final_two_plot,
                         width = 6, height = 3.5, dpi = 1200, device = "png")

                    #showtext_end()
                    
                }, warning = function(w) {
                    
                    print(w)
                    
                    shinyalert("Font problem", 
                               "Ignore the current file download. There was a problem rendering the right fonts. This happens sometimes. Re-click 'predict' button to re-plot.",
                               type = "warning")
                    
                }, error = function(e) {
                    print(e)
                }, finally = {
                })
                
                incProgress(4/4)
                
            }, 
            message = 'making new predictions',
            detail = "president - district 1",
            value = 0)
            
            zip(zipfile=file, 
                files=c("first_round.png", 
                        "win_probability.csv",
                        "final_two.png",
                        "first_round.csv",
                        "predictions.csv"))
        },
        contentType = "application/zip"
    )
    
    output$pres_predict_d2 <- downloadHandler(
        filename = function(){glue("president_district2.zip")},
        content = function(file) {
            
            withProgress({
                
                incProgress(1/4)
                
                president_first_round <-
                    data.frame(candidate = c("Joe Biden", "Donald Trump",
                                             "Jo Jorgensen", "Howie Hawkins",
                                             "Rocky De La Fuente"),
                               count = c(input$biden_first_round_d2, 
                                         input$trump_first_round_d2,
                                         input$jorgensen_first_round_d2,
                                         input$hawkins_first_round_d2,
                                         input$delafeunte_first_round_d2))
                
                president_predictions <- 
                    predict_president(president_d2_poll, 
                                      input$trump_first_round_d2, 
                                      input$biden_first_round_d2,
                                      input$hawkins_first_round_d2,
                                      input$jorgensen_first_round_d2,
                                      input$delafeunte_first_round_d2)
                
                win_prob <- 
                    president_predictions %>%
                    mutate(biden_win = pred_biden_perc > 0.5,
                           trump_win = pred_trump_perc > 0.5,
                           tie = (pred_biden_perc == 0.5)) %>%
                    summarise(prob_biden_win = round(mean(biden_win), 2),
                              prob_trump_win = round(mean(trump_win), 2),
                              prob_tie = round(mean(tie), 2))
                
                incProgress(2/4)
                
                tryCatch({
                    
                    president_first_round_plot <- 
                        president_first_round_plot(president_first_round)
                    
                    president_final_two_plot <- 
                        final_two_plot(president_predictions$pred_biden_perc,
                                       president_predictions$pred_trump_perc)
                    
                    #showtext_auto()
                    
                    incProgress(3/4)
                    
                    setwd(tempdir())
                    
                    write_csv(president_predictions, "predictions.csv")
                    write_csv(president_first_round, "first_round.csv")
                    write_csv(win_prob, "win_probability.csv")
                    
                    # png("first_round.png", width = 7, height = 2, units = "in", res = 1200)
                    # president_first_round_plot
                    # dev.off()
                    # 
                    # png("final_two.png", width = 6, height = 3.5, units = "in", res = 1200)
                    # president_final_two_plot
                    # dev.off()
                    
                    ggsave("first_round.png", plot = president_first_round_plot,
                          width = 7, height = 2, dpi = 1200, device = "png")
                    ggsave("final_two.png", plot = president_final_two_plot,
                         width = 6, height = 3.5, dpi = 1200, device = "png")

                    #showtext_end()
                    
            }, warning = function(w) {
                
                print(w)
                
                shinyalert("Font problem", 
                           "Ignore the current file download. There was a problem rendering the right fonts. This happens sometimes. Re-click 'predict' button to re-plot.",
                           type = "warning")
                
            }, error = function(e) {
                print(e)
            }, finally = {
            })
                
                incProgress(4/4)
                
            }, 
            message = 'making new predictions',
            detail = "president - district 2",
            value = 0)
            
            zip(zipfile=file, 
                files=c("first_round.png", 
                        "win_probability.csv",
                        "final_two.png",
                        "first_round.csv",
                        "predictions.csv"))
        },
        contentType = "application/zip"
    )
    
    output$pres_predict_d12 <- downloadHandler(
        filename = function(){glue("president_district12.zip")},
        content = function(file) {
            
            withProgress({
                
                incProgress(1/4)
                
                president_first_round <-
                    data.frame(candidate = c("Joe Biden", "Donald Trump",
                                             "Jo Jorgensen", "Howie Hawkins",
                                             "Rocky De La Fuente"),
                               count = c(input$biden_first_round_d12, 
                                         input$trump_first_round_d12,
                                         input$jorgensen_first_round_d12,
                                         input$hawkins_first_round_d12,
                                         input$delafeunte_first_round_d12))
                
                president_predictions <- 
                    predict_president(president_d12_poll, 
                                      input$trump_first_round_d12, 
                                      input$biden_first_round_d12,
                                      input$hawkins_first_round_d12,
                                      input$jorgensen_first_round_d12,
                                      input$delafeunte_first_round_d12)
                
                win_prob <- 
                    president_predictions %>%
                    mutate(biden_win = pred_biden_perc > 0.5,
                           trump_win = pred_trump_perc > 0.5,
                           tie = (pred_biden_perc == 0.5)) %>%
                    summarise(prob_biden_win = round(mean(biden_win), 2),
                              prob_trump_win = round(mean(trump_win), 2),
                              prob_tie = round(mean(tie), 2))
                
                incProgress(2/4)
                
                tryCatch({
                    
                    president_first_round_plot <- 
                        president_first_round_plot(president_first_round)
                    
                    president_final_two_plot <- 
                        final_two_plot(president_predictions$pred_biden_perc,
                                       president_predictions$pred_trump_perc)
                    
                    #showtext_auto()
                    
                    incProgress(3/4)
                    
                    setwd(tempdir())
                    
                    write_csv(president_predictions, "predictions.csv")
                    write_csv(president_first_round, "first_round.csv")
                    write_csv(win_prob, "win_probability.csv")
                    
                    # png("first_round.png", width = 7, height = 2, units = "in", res = 1200)
                    # president_first_round_plot
                    # dev.off()
                    # 
                    # png("final_two.png", width = 6, height = 3.5, units = "in", res = 1200)
                    # president_final_two_plot
                    # dev.off()
                    
                    ggsave("first_round.png", plot = president_first_round_plot,
                          width = 7, height = 2, dpi = 1200, device = "png")
                    ggsave("final_two.png", plot = president_final_two_plot,
                         width = 6, height = 3.5, dpi = 1200, device = "png")

                    #showtext_end()
                            
                }, warning = function(w) {
                    
                    print(w)
                    
                    shinyalert("Font problem", 
                               "Ignore the current file download. There was a problem rendering the right fonts. This happens sometimes. Re-click 'predict' button to re-plot.",
                               type = "warning")
                    
                }, error = function(e) {
                    print(e)
                }, finally = {
                })
                
                incProgress(4/4)
                
            }, 
            message = 'making new predictions',
            detail = "president - district 1 + 2",
            value = 0)
            
            zip(zipfile=file, 
                files=c("first_round.png", 
                        "win_probability.csv",
                        "final_two.png",
                        "first_round.csv",
                        "predictions.csv"))
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
