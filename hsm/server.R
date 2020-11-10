library(shiny)
library(shinyjs)
library(DBI)
library(RPostgres)

shinyServer(function(input, output, session){


  # it should be sufficient to store the session token.
  # cookies auth is a more sophisticated alternative, but
  # let's not dive into js to deep for now.
  # session$token

  store_record <- function(response){
    fcon <- file(".pgpass","r")
    con <-  dbConnect(drv = Postgres(), dbname = "postgres", user = "postgres",
                       host = "34.65.173.162",
                      password = readLines(fcon, warn = FALSE))
    dbExecute(con,"SET SEARCH_PATH=h4sci")
    dbAppendTable(con, dbQuoteIdentifier(con,"hsm"), response)
    dbDisconnect(con)
  }

  submitted <- reactiveVal(FALSE)


  response <- reactive({
    dt <- data.frame(
      id = session$token,
      general = paste(input$general, collapse=","),
      l_r = input$r,
      l_python = input$python,
      l_julia = input$julia,
      l_matlab = input$matlab,
      l_sql = input$sql,
      l_cpp = input$cpp,
      l_js = input$js,
      l_web = input$web,
      w_git = input$git,
      w_markdown = input$markdown,
      w_scrum = input$scrum,
      w_kanban = input$kanban,
      learntocode = input$learntocode,
      stringsAsFactors = FALSE
    )
  })

  observeEvent(input$submit, {
    store_record(response())
    submitted(TRUE)
    # has_participated(TRUE)
    # js$setcookie("HAS_PARTICIPATED_IN_SPOSM_INTRO_SURVEY")
  })

  output$test <- renderText({
    as.character(submitted())
  })


  output$general <- renderUI(
    if(!submitted()){
      fluidRow(
        column(width = 6,
               div(class = "panel panel-primary",
                   div(class = "panel-heading",
                       h3("General")),
                   div(class = "panel-body",
                       "Please select all terms that you feel familiar with, i.e.,
              you're able to explain a rough idea of the concept to others.",
                       checkboxGroupInput("general","Technology",
                                          c("compiled vs. interpreted",
                                            "relational vs. non-relational",
                                            "melt vs. cast, long vs. wide",
                                            "containerization",
                                            "deployment",
                                            "CI/CD",
                                            "feature branch based workflow",
                                            "unit test",
                                            "parallel computing",
                                            "static website generator",
                                            "opensource licensing")
                       )
                   )
               )
        )
      )
    }
  )

  output$lang <- renderUI(
    if(!submitted()){

      fluidRow(
        column(width = 6,
               div(class = "panel panel-primary",
                   div(class = "panel-heading",
                       h3("Programming Languages")),
                   div(class = "panel-body",
                       "Please indicate your familiarity with the following languages. 1 = never heard about it, 2 = seen it but never got beyond playing around, 3 = capable of doing a project in this lang, 4 = several years of experience, 5 = extension developer and beyond.",
                       sliderInput("r","R",min = 1, max = 5, value = 3),
                       sliderInput("python","Python",min = 1, max = 5, value = 3),
                       sliderInput("julia","Julia",min = 1, max = 5, value = 3),
                       sliderInput("matlab","Matlab",min = 1, max = 5, value = 3),
                       sliderInput("sql","SQL",min = 1, max = 5, value = 3),
                       sliderInput("cpp","Cpp",min = 1, max = 5, value = 3),
                       sliderInput("js","Javascript",min = 1, max = 5, value = 3),
                       sliderInput("web","HTML", min = 1, max = 5, value = 3)
                   )

               )

        )
      )
    }

  )

  output$workflow <- renderUI(
    if(!submitted()){
      fluidRow(
        column(width = 6,
               div(class = "panel panel-primary",
                   div(class = "panel-heading",
                       h3("Developer Workflow")),
                   div(class = "panel-body",
                       "Software development is a team sport. The field has developed industry standard workflows that allow developers who never met to efficiently create software together. Please indicate your familiarity with the following tools and techniques. 1 = never heard of it, 2 = trying out status, 3 = working with it in real life projects,
              4 = several years of experience, 5 = leading groups to use it.",
                       sliderInput("git","Git Version Control",min = 1, max = 5, value = 3),
                       sliderInput("markdown","Markdown Based Publishing",min = 1, max = 5, value = 3),
                       sliderInput("scrum","SCRUM",min = 1, max = 5, value = 3),
                       sliderInput("kanban","KANBAN",min = 1, max = 5, value = 3)
                   ),

               )
        )
      )
    }
  )

  output$yesno <- renderUI(
    if(!submitted()){
      fluidRow(
        column(width = 6,
               div(class = "panel panel-primary",
                   div(class = "panel-heading",
                       h3("Learn to Code?")),
                   div(class = "panel-body",
                       "Please indicate whether you plan to learn coding at a software carpentry level within the next year.",
                       radioButtons("learntocode","Learn to Code?",
                                    choices = list("yes, definitely" = "yes",
                                                       "maybe" = "maybe",
                                                       "No way, I got other plans." = "no",
                                                       "I don't like to answer" = "NA"),
                                    selected = "NA")
                   )
               )))
    }
  )


  output$submit <- renderUI(
    if(!submitted()){
    fluidRow(
      column(width = 6,
             div(class = "panel panel-primary",
                 div(class = "panel-heading",
                     h3("Submit Your Answers!")),
                 div(class = "panel-body", align = "right",
                     actionButton('submit',"submit")
                 )
             )
      )
    )
    }
  )

  output$thanks <- renderUI(
    if(submitted()){
      fluidRow(
        column(width = 6,
               div(class = "panel panel-info",
                   div(class = "panel-heading",
                       h3("Thank You")),
                   div(class = "panel-body", align = "right",
                       "Thank you for your participation. Please take only part once."
                   )
               ))
      )
    }
  )






})








