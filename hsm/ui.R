# Inspired by https://trr266.wiwi.hu-berlin.de/shiny/sposm_survey/
# https://github.com/joachim-gassen/sposm/tree/master/code/intro_survey

library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("superhero"),
          title = "Hacking (Social Media) Data Survey",
          fluidRow(
            column(width = 6,
                   div(class = "jumbotron",
                       h1("Hacking (Social Media) Data"),
                       p("This survey helps to give accurate feedback on the ideal starting point depending on your background. Please answer the following questions and make sure to not answer twice."))
            )
          ),
          uiOutput("general"),
          uiOutput("lang"),
          uiOutput("workflow"),
          uiOutput("yesno"),
          uiOutput("submit"),
          uiOutput("thanks")
          )

