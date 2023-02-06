ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      textInput(inputId = "name", label = "", placeholder = "Name", width = "100%"),
      box(
        numericInput(inputId="from", label="From",value=""),
        radioButtons(inputId="from_mult", label="from_mult", choices = c("1","k","m","b"),
                     selected=1, inline=T) ),
      
      box(
        numericInput(inputId="to", label="To",value=""),
        radioButtons(inputId="to_mult", label="to_mult", choices = c("1","k","m","b"),
                     selected=1, inline=T)
      ),
      textOutput("feedback",inline = T)
    )
  )
)
