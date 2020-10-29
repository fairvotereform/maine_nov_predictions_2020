# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(showtext)

font_add("Montserrat", 
         regular = "Montserrat/Montserrat-SemiBold.ttf", 
         bold = "Montserrat/Montserrat-Bold.ttf")
showtext_auto()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # title
    titlePanel("Maine November 2020 Predictions"),
    fluidRow(
        column(3, wellPanel(
            h4("Senate"),
            hr(),
            h5("First Round Count:"),
            numericInput("collins_first_round", "Collins:", 0),
            numericInput("gideon_first_round", "Gideon:", 0),
            numericInput("linn_first_round", "Linn:", 0),
            numericInput("savage_first_round", "Savage:", 0),
            actionButton("senate_predict", "Predict")
            )),
        column(3, wellPanel(
            h4("President - District 2"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d2", "Biden:", 0),
            numericInput("trump_first_round_d2", "Trump:", 0),
            numericInput("jorgensen_first_round_d2", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d2", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d2", "De La Fuente (keep as zero to exclude):", 0),
            actionButton("pres_predict_d2", "Predict")
        )),
        column(3, wellPanel(
            h4("President - District 1"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d1", "Biden:", 0),
            numericInput("trump_first_round_d1", "Trump:", 0),
            numericInput("jorgensen_first_round_d1", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d1", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d1", "De La Fuente (keep as zero to exclude):", 0),
            actionButton("pres_predict_d1", "Predict")
        )),
        column(3, wellPanel(
            h4("President - Both Districts"),
            hr(),
            h5("First Round Count:"),
            numericInput("biden_first_round_d12", "Biden:", 0),
            numericInput("trump_first_round_d12", "Trump:", 0),
            numericInput("jorgensen_first_round_d12", "Jorgensen:", 0),
            numericInput("hawkins_first_round_d12", "Hawkins:", 0),
            numericInput("delafeunte_first_round_d12", "De La Fuente (keep as zero to exclude):", 0),
            actionButton("pres_predict_d12", "Predict")
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$senate_predict, {
        
    })

    observeEvent(input$pres_predict_d1, {
        
    })
    
    observeEvent(input$pres_predict_d2, {
        
    })
        
    observeEvent(input$pres_predict_d12, {
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
