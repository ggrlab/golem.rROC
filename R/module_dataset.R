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
datasetInput <- function(id, filter = NULL) {
    names <- ls("package:datasets")
    if (!is.null(filter)) {
        data <- lapply(names, get, "package:datasets")
        names <- names[vapply(data, filter, logical(1))]
    }

    selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}
#' name_of_module1 Server Functions
#'
#' @noRd
datasetServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(get(input$dataset, "package:datasets"))
    })
}
#' @export
selectFilterServer <- function(id){
    moduleServer(id, function(input, output, session){
        reactive(get(input$filter))
    })
}
#' @export
selectFilterInput <- function(id){
    selectInput(NS(id, "filter"), "Filter", choices = c("is.numeric", "is.character", "is.logical"))
}