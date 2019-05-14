library(shiny)

shinyUI(fluidPage(
        titlePanel("Comparison of the most popular degrees by sex in 2017 - 2018 in Spain"),
        sidebarLayout(
                sidebarPanel(
                        sliderInput(inputId = "students",
                                    label = "Select a range of male or female students",
                                    min = 1,
                                    max = 104000,
                                    value = c(15000, 104000),
                                    step = 1000),
                        checkboxInput(inputId = "checkwomen",
                                      label = "Show women",
                                      value = TRUE),
                        checkboxInput(inputId = "checkmen",
                                      label = "Show men",
                                      value = TRUE)
                ),
                mainPanel(h3("Compare the most important degrees in the range of students selected"),
                          plotOutput("plot3")),
        )
))

