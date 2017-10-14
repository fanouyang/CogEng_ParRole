
library(shiny)

shinyUI(fluidPage(

  titlePanel("Cognitive Engagement In Terms of Participatory Roles"),

  sidebarLayout(

    sidebarPanel(

      selectInput("code", "Choose a cognitive engagement code:",
                  c("Individual Exploration"=1,"Individual Elaboration"=2,"Individual Question Elicitation & Response"=3,
                    "Individual Practical Application"=4,"Individual MetaCognitive Learning"=5,
                    "Group Exploration"=6,"Group Elaboration"=7,"Group Question Elicitation & Response"=8,
                    "Group Practical Application"=9,"Group MetaCognitive Learning"=10, "Overall"=11),
                  helpText("This is a demo for student cognitive engagement in terms of participatory roles"
                  ),
                  width="100%"
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Cognitive Engagement In Terms of Participatory Roles", plotOutput("plot")),
        tabPanel("More info", htmlOutput("info"))




      )
    )
  )
))
