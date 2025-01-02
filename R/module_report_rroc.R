#' @title Preview data
#' @description This module provides a preview of the first 6 columns of a dataset.
#' @param id A character string representing the module's ID.
#' @return A Shiny module for previewing data.
report_rroc_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Plot",
        h2("Restriction"),
        plotOutput(ns("rroc_plot")),
        # Make a downloadable DT table
        DT::DTOutput(ns("restriction_performances")),
        downloadButton(ns("download_rroc"), "Download")
    )
}
#' @noRd
#' @export
#' @examples
#' previewDataUI("preview")
report_rroc_server <- function(id, data00, rroc_results, listen_iv_dv_first, positive_label) {
    stopifnot(is.reactive(listen_iv_dv_first))
    stopifnot(is.reactive(rroc_results))
    stopifnot(is.reactive(data00))
    stopifnot(is.reactive(positive_label))

    redo_plot <- reactiveVal(list("dv" = "", "iv" = ""))

    moduleServer(id, function(input, output, session) {
        restriction_perf <- reactive({
            perf_table <- restrictedROC:::summary.rROC(rroc_results())
            # If the table is not empty and the DV and IV are not NULL
            if (!is.null(perf_table) & !any(sapply(listen_iv_dv_first(), is.null))) {
                perf_table <- perf_table[order(
                    sub(listen_iv_dv_first()[[1]], "", perf_table[["level_1"]], fixed = TRUE),
                    sub(listen_iv_dv_first()[[2]], "", perf_table[["level_2"]], fixed = TRUE)
                ), ]
            }
            return(perf_table)
        })
        output$restriction_performances <- DT::renderDT({
            restriction_perf()
        })
        output$download_rroc <- downloadHandler(
            filename = function() {
                "RestrictionPerformance.xlsx"
            },
            content = function(fname) {
                download_restriction_performance(restriction_perf, fname)
            }
        )
        observe_rroc_plot_update(rroc_results, data00, redo_plot, listen_iv_dv_first, positive_label)

        output$rroc_plot <- renderPlot({
            render_rroc_plot(redo_plot, rroc_results, data00)
        })
    })
}


#' Download restriction performance
#'
#' @param restriction_perf Reactive value for restriction performance
#' @param fname Filename for the downloaded file
#' @return A downloadable file
download_restriction_performance <- function(restriction_perf, fname) {
    if (!is.null(restriction_perf())) {
        return(writexl::write_xlsx(restriction_perf(), fname))
    } else {
        return(restriction_perf())
    }
}


#' Observe RROC plot update
#'
#' @param input Shiny input object
#' @param rroc_result Reactive value for RROC result
#' @param current_data Reactive value for current data
#' @param redo_plot Reactive value for redo plot
#' @param listen_iv_dv_first Reactive value for listening to first DV and IV
observe_rroc_plot_update <- function(rroc_result, data00, redo_plot, listen_iv_dv_first, positive_label) {
    stopifnot(is.reactive(rroc_result))
    stopifnot(is.reactive(data00))
    stopifnot(is.reactive(redo_plot))
    stopifnot(is.reactive(listen_iv_dv_first))
    stopifnot(is.reactive(positive_label))

    observeEvent(listen_iv_dv_first(), {
        dv <- listen_iv_dv_first()[["dv"]]
        iv <- listen_iv_dv_first()[["iv"]]
        if (iv == "" || dv == "" || length(dv) == 0 || length(iv) == 0) {
            # pass
        } else {
            # cat("PLOTTING", unlist(listen_iv_dv_first()), "\n")
            if (has_been_calculated(dv, iv, rroc_result) && !all(is.na(rroc_result()[[dv]][[iv]]))) {
                if (is.null(rroc_result()[[dv]][[iv]][["plots"]][["plots"]])) {
                    tmp_plot <- restrictedROC::plot_density_rROC_empirical(
                        values_grouped = split(data00()[[iv]], data00()[[dv]]),
                        positive_label = positive_label()
                    )
                    tmp_rroc_res <- rroc_result()
                    tmp_rroc_res[[dv]][[iv]][["plots"]][["plots"]] <- tmp_plot
                    rroc_result(tmp_rroc_res)
                    cat("    Restriction plot was recalculated\n")
                }
            }
            redo_plot(listen_iv_dv_first())
        }
        # })
    })
}

#' Check if RROC has been calculated
#'
#' @param dv Dependent variable
#' @param iv Independent variable
#' @param rroc_result Reactive value for RROC result
#' @return TRUE if RROC has been calculated, FALSE otherwise
has_been_calculated <- function(dv, iv, rroc_result) {
    stopifnot(is.reactive(rroc_result))
    !(is.null(rroc_result()) ||
        is.null(dv) || is.null(iv) ||
        !dv %in% names(rroc_result()) ||
        !iv %in% names(rroc_result()[[dv]]))
}



#' Render RROC plot
#'
#' @param redo_plot Reactive value for redo plot
#' @param rroc_result Reactive value for RROC result
#' @param current_data Reactive value for current data
#' @return A plot of RROC
render_rroc_plot <- function(redo_plot, rroc_result, current_data) {
    stopifnot(is.reactive(rroc_result))
    stopifnot(is.reactive(current_data))
    stopifnot(is.reactive(redo_plot))

    dv <- redo_plot()[["dv"]]
    iv <- redo_plot()[["iv"]]
    if (!has_been_calculated(dv, iv, rroc_result)) {
        return(ggplot2::ggplot() +
            ggplot2::annotate(
                "text",
                x = 0.5, y = 0.5,
                label = paste0(
                    "Restriction was not calculated for:\n",
                    "Dependent variable:  '", dv, "'\n",
                    "Independent variable:  '", iv, "'\n",
                    "Hit 'Run restriction' to calculate it."
                ),
                size = 10
            ) +
            ggplot2::theme_void())
    } else if (all(is.na(rroc_result()[[dv]][[iv]]))) {
        return(ggplot2::ggplot() +
            ggplot2::annotate(
                "text",
                x = 0.5, y = 0.5,
                label = paste0(
                    "Restriction not calculatable. \n",
                    "Did you select a dependent variable with 2 unique values and\n",
                    "an independent variable with numeric values?"
                ),
                size = 10
            ) +
            ggplot2::theme_void())
    } else {
        return(rroc_result()[[dv]][[iv]][["plots"]][["plots"]])
    }
}
