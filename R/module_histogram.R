#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
ui_histogram <- function(id) {
    # UI module: Namespacing is explicit: NS(id, "NAME")
    ns <- NS(id)
    tagList(
        selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
        numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
        plotOutput(NS(id, "hist"))
    )
}

#' name_of_module1 Server Functions
#'
#' @noRd
server_histogram <- function(id) {
    # Server module: Namespacing is implicit: input$NAME --> NS(id, "NAME")
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        data <- reactive(mtcars[[input$var]])
        output$hist <- renderPlot(
            {
                hist(data(), breaks = input$bins, main = input$var)
            },
            res = 96
        )
    })
}

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
