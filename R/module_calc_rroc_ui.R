#' @title Preview data
#' @description This module provides a preview of the first 6 columns of a dataset.
#' @param id A character string representing the module's ID.
#' @return A Shiny module for previewing data.
calc_rroc_ui <- function(id) {
    ns <- NS(id)
    list(
        uiOutput(ns("ui_dvs")),
        actionButton(ns("dv_DEselect_all"), "De/select all DV"),
        uiOutput(ns("ui_ivs")),
        actionButton(ns("iv_DEselect_all"), "De/select all IV"),
        numericInput(
            inputId = ns("n_permutations"),
            label = "Number of permutations:",
            value = 4, min = 0, max = 1000, step = 1
        ),
        uiOutput(ns("ui_positive_labels")),
        actionButton(ns("run_rroc"), "Run restriction", icon = icon("play", verify_fa = FALSE)),
        checkboxInput(ns("recalculate_rroc"), "Recalculate?", value = FALSE, width = NULL)
    )
}
#' @noRd
#' @export
#' @examples
#' previewDataUI("preview")
calc_rroc_server <- function(id, data00, rroc_results) {
    stopifnot(is.reactive(data00))
    stopifnot(is.reactive(rroc_results))
    all_cols <- reactive({
        names(data00())
    })
    moduleServer(id, function(input, output, session) {
        # Selector UI
        possible_positive_labels <- reactive({
            if (length(input$dependent_vars) != 1) {
                return(NULL)
            } else {
                return(unique(data00()[[input$dependent_vars]]))
            }
        })
        rroc_ui_selection(id, input, output, all_cols, possible_positive_labels)


        dv_selector <- reactiveVal(value = TRUE)
        iv_selector <- reactiveVal(value = TRUE)
        observeEvent(input$dv_DEselect_all, {
            # invert dv_selector
            dv_selector(!dv_selector())
            if (dv_selector() == "FALSE") {
                updateSelectInput(session, NS(id, "dependent_vars"), selected = character(0))
            } else {
                updateSelectInput(session, NS(id, "dependent_vars"), selected = all_cols())
            }
        })
        observeEvent(input$iv_DEselect_all, {
            iv_selector(!iv_selector())
            if (iv_selector() == "FALSE") {
                updateSelectInput(session, NS(id, "independent_vars"),
                    selected = character(0)
                )
            } else {
                updateSelectInput(session, NS(id, "independent_vars"),
                    selected = all_cols()[!all_cols() %in% input$dependent_vars]
                )
            }
        })
    })
}

rroc_ui_selection <- function(id, input, output, all_cols, possible_positive_labels) {
    stopifnot(is.reactive(all_cols))
    stopifnot(is.reactive(possible_positive_labels))

    output$ui_dvs <- shiny::renderUI({
        selectInput(
            inputId = NS(id, "dependent_vars"),
            label = "Dependent variables:",
            choices = all_cols(),
            # selected = input$dependent_vars,
            multiple = TRUE,
            size = min(10, length(all_cols())),
            selectize = FALSE
        )
    })
    output$ui_ivs <- shiny::renderUI({
        selectInput(
            inputId = NS(id, "independent_vars"),
            label = "Independent variables:",
            choices = all_cols()[!all_cols() %in% input$dependent_vars],
            selected = all_cols()[!all_cols() %in% input$dependent_vars],
            multiple = TRUE,
            size = min(10, length(all_cols())),
            selectize = FALSE
        )
    })
    output$ui_positive_labels <- shiny::renderUI({
        selectInput(
            inputId = NS(id, "positive_label"),
            label = "Positive label:",
            choices = possible_positive_labels(),
            selected = "group_A"
        )
    })
}
