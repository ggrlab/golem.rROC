#' Input Dataset Module
#'
#' This function creates an input dataset module for a Shiny application.
#'
#' @param id A character string representing the module's ID.
#' @param filter An optional filter to apply to the dataset. Default is NULL.
#'
#' @return A Shiny module for dataset input.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @examples
#' input_dataset("dataset1")
#' input_dataset("dataset2", filter = some_filter_function)
#'
#' @export
selectVarInput <- function(id) {
    selectInput(NS(id, "var"), "Variable", choices = NULL)
}


#' name_of_module1 Server Functions
#'
#' @noRd
selectVarServer <- function(id, data, filter = is.numeric) {
    stopifnot(is.reactive(data))
    # stopifnot(!is.reactive(filter))
    stopifnot(is.reactive(filter))

    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(session, "var", choices = find_vars(data(), filter()))
        })

        list(
            name = reactive(input$var),
            value = reactive(data()[[input$var]])
        )
    })
}
