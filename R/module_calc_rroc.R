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

        observe_rroc_calculation(input, output, data00, rroc_results, possible_positive_labels)
        first_iv_and_dv <- reactive({
            list(
                "dv" = input$dependent_vars[1],
                "iv" = input$independent_vars[1]
            )
            # The debounce is necessary because:
            # If the user selects a new dependent variable, all independent variables are updated
            # --> the first independent variable is selected
            # --> Second invalidation of first_iv_and_dv
            # Debounce prevents the second invalidation because it happens in less than 50ms
        }) |> shiny::debounce(50)

        return(list(
            listen_iv_dv_first = first_iv_and_dv,
            positive_label = reactive({
                input$positive_label
            })
        ))
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

observe_rroc_calculation <- function(input, output, data00, rroc_results, possible_positive_labels) {
    stopifnot(is.reactive(data00))
    stopifnot(is.reactive(rroc_results))
    stopifnot(is.reactive(possible_positive_labels))

    observeEvent(input$run_rroc, {
        shiny::withProgress(
            message = "Calculating restriction...",
            detail = "",
            value = 0,
            {
                dvs <- input$dependent_vars
                ivs <- input$independent_vars
                if (length(dvs) == 0) {
                    warning("No dependent variable selected")
                    return()
                }
                if (length(ivs) == 0) {
                    warning("No independent variable selected")
                    return()
                }
                pos_label <- 1
                if (input$positive_label != "") {
                    pos_label <- input$positive_label
                }
                future::future(
                    {
                        calc_rroc_helper(
                            dvs,
                            ivs,
                            pos_label,
                            rroc_results,
                            data00,
                            input$n_permutations,
                            input$recalculate_rroc
                        )
                    },
                    seed = TRUE
                )
            }
        )
    })
}


calc_rroc_helper <- function(dvs, ivs, positive_label, rroc_results, data00, n_permutations, recalculate_rroc) {
    stopifnot(is.character(dvs))
    stopifnot(is.character(ivs))
    stopifnot(is.character(positive_label))
    stopifnot(is.reactive(rroc_results))
    stopifnot(is.reactive(data00))
    stopifnot(is.numeric(n_permutations))
    stopifnot(is.logical(recalculate_rroc))
    if (is.null(rroc_results())) {
        rroc_res_tmp <- rroc_secure(
            df = data00(),
            dependent_vars = dvs,
            independent_vars = ivs,
            do_plots = TRUE,
            n_permutations = max(n_permutations, 0),
            positive_label = positive_label,
            parallel_permutations = FALSE
        )
    } else {
        if (recalculate_rroc) {
            new_dv_iv <- sapply(dvs, function(x) ivs, simplify = FALSE)
        } else {
            dvs_ivs_existing <- lapply(rroc_results(), names)
            new_dv_iv <- sapply(dvs, function(dv_x) {
                ivs[!ivs %in% dvs_ivs_existing[[dv_x]]]
            }, simplify = FALSE)
        }
        rroc_res_tmp <- sapply(names(new_dv_iv), function(dv_x) {
            if (length(new_dv_iv[[dv_x]]) == 0) {
                return(NULL)
            }
            tmp <- sapply(new_dv_iv[[dv_x]], function(iv_x) {
                return(
                    rroc_secure(
                        df = data00(),
                        dependent_vars = dv_x,
                        independent_vars = iv_x,
                        do_plots = TRUE,
                        n_permutations = max(n_permutations, 0),
                        positive_label = positive_label,
                        parallel_permutations = FALSE
                    )[[dv_x]][[iv_x]]
                )
            }, simplify = FALSE)
            return(tmp)
        }, simplify = FALSE)
        rroc_res_tmp <- rroc_res_tmp[!all(is.null(rroc_res_tmp))]
    }

    if (is.null(rroc_results())) {
        rroc_results(rroc_res_tmp)
    } else if (all(is.null(rroc_res_tmp)) || all(sapply(rroc_res_tmp, is.null))) {
        print("All results have been calculated before already")
    } else {
        new_rroc <- rroc_results()
        for (dv_x in names(rroc_res_tmp)) {
            if (!dv_x %in% names(rroc_results())) {
                new_rroc[[dv_x]] <- rroc_res_tmp[[dv_x]]
            } else {
                for (iv_x in names(rroc_res_tmp[[dv_x]])) {
                    new_rroc[[dv_x]][[iv_x]] <- rroc_res_tmp[[dv_x]][[iv_x]]
                }
            }
        }
        rroc_results(new_rroc)
    }
}
