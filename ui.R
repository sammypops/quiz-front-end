ui <- function(request) {
  add_cookie_handlers(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        shinyjs::useShinyjs(),
        # Boxes need to be put in a row (or column)
        fluidRow(align = "center",
                 h2(textOutput(outputId = "name_feedback")),
                 textInput(inputId = "name", label = "", placeholder = "Change name here" , width = "100%"),
                 h3(textOutput(outputId = "question_number")),br(),
                 box(
                   numericInput(inputId="from", label="From",value=""),
                   radioButtons(inputId="from_mult", label="multiplier", choices = c("1","k","m","b"),
                                selected=1, inline=T) ),
                 
                 box(
                   numericInput(inputId="to", label="To",value=""),
                   radioButtons(inputId="to_mult", label="multiplier", choices = c("1","k","m","b"),
                                selected=1, inline=T)
                 ),
                 textOutput("feedback")
        ),
        br(),
        fluidRow(align = "centre",
                 br(),
                 actionButton(inputId="submit", label="submit", width = "100%"),
                 br(),
                 br(),
                 # bookmarkButton(),
                 br(),
                 br(),
                 textInput(inputId = "player", label = "", width = "100%")
        )
      )
    )
  )
}
