#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # https://rstudio.github.io/shiny/reference/ExtendedTask.html
    #### Initialize reactives
    current_data <- reactiveVal()
    # Set the initial value for current_data
    current_data(glehr2023_cd4_cd8_relative[, -1])

    # Restriction userinterface
    # all_cols <- reactive({
    #     names(current_data())
    # })
    # possible_positive_labels <- reactive({
    #     if (length(input$dependent_vars) != 1) {
    #         return(NULL)
    #     } else {
    #         return(unique(current_data()[[input$dependent_vars]]))
    #     }
    # })
    redo_plot <- reactiveVal(list("dv" = "", "iv" = ""))
    rroc_result <- reactiveVal()
    # Initialize rroc_result with a small example
    rroc_result(frontiers110_tcell_relative__permutation_10k_small)
    # # Reactive value for restriction performance
    # restriction_perf <- reactive({
    #     perf_table <- restrictedROC:::summary.rROC(rroc_result())
    #     if (!is.null(perf_table) & !any(sapply(listen_iv_dv_first(), is.null))) {
    #         perf_table <- perf_table[order(
    #             sub(listen_iv_dv_first()[[1]], "", perf_table[["level_1"]], fixed = TRUE),
    #             sub(listen_iv_dv_first()[[2]], "", perf_table[["level_2"]], fixed = TRUE)
    #         ), ]
    #     }
    #     return(perf_table)
    # })
    # listen_iv_dv_first <- reactive({
    #     list(
    #         "dv" = input$dependent_vars[1],
    #         "iv" = input$independent_vars[1]
    #     )
    # })

    #### Initialize shared data
    r_data <- initialize_shared_data()

    #### Variable UIs
    # # File upload UI
    # output$ui_fileUpload <- shiny::renderUI({
    #     shiny::req(input$selected_data_type)
    #     uploadfile_fun(input$selected_data_type)
    # })
    # # Clipboard UI
    # output$ui_load_clipboard <- shiny::renderUI({
    #     render_clipboard_ui()
    # })
    # # # Data UI
    # output$ui_data <- shiny::renderUI({
    #     render_data_ui("moduleNewdata")
    # })
    # # Data preview
    # output$data_preview <- DT::renderDT({
    #     render_data_preview(input, current_data_table)
    # })
    # # Full data preview
    # output$data_preview_full <- DT::renderDT({
    #     render_data_preview_full(input, current_data_table)
    # })
    # # Restriction UI
    # output$ui_dvs <- shiny::renderUI({
    #     render_dvs_ui(all_cols)
    # })
    # output$ui_ivs <- shiny::renderUI({
    #     render_ivs_ui(all_cols, input)
    # })
    # output$ui_positive_labels <- shiny::renderUI({
    #     render_positive_labels_ui(possible_positive_labels)
    # })

    #### Plots and Tables
    # Restriction plot
    output$rroc_plot <- renderPlot({
        render_rroc_plot(redo_plot, rroc_result, current_data, input)
    })

    # # Restriction performances
    # output$restriction_performances <- DT::renderDT({
    #     restriction_perf()
    # })

    # #### Buttons
    # output$download_rroc <- downloadHandler(
    #     filename = function() {
    #         "RestrictionPerformance.xlsx"
    #     },
    #     content = function(fname) {
    #         download_restriction_performance(restriction_perf, fname)
    #     }
    # )

    # # Observers
    # observe_data_upload(input, current_data, rroc_result)
    # observe_clipboard_data(input, current_data)
    # observe_dv_iv_selection(input, session, dv_selector, iv_selector, all_cols)
    # observe_rroc_calculation(input, current_data, rroc_result, possible_positive_labels)
    # observe_rroc_plot_update(input, rroc_result, current_data, redo_plot, listen_iv_dv_first)

    # Modules
    # GIve current_data as reactive input to the module (without "evaluation" brackets)
    previewDataServer("preview", current_data)
    newDataServer("newdata", current_data)
    calc_rroc_server("calc_rroc_selector", current_data, rroc_result)
}

#' Initialize shared data
#'
#' @return A list containing shared data
initialize_shared_data <- function() {
    r_data <- list()
    r_data[["clipboard"]] <- tibble::tibble(
        "group_A" = c(1.31, 5.32, 40.2),
        "group_B" = c(1.31, 5.32, 40.2)
    )
    return(r_data)
}

# #' Generate file input UI based on selected data type
# #'
# #' @param selected_data_type The type of data selected by the user
# #' @return A file input UI element
# uploadfile_fun <- function(selected_data_type) {
#     if (selected_data_type == "csv") {
#         fileInput("uploadfile", NULL, multiple = FALSE, accept = c(
#             "text/csv",
#             "text/comma-separated-values",
#             "text/tab-separated-values",
#             "text/plain",
#             ".csv",
#             ".tsv"
#         ))
#     } else if (any(sapply(c("rda", "rds", "Rdata"), grepl, selected_data_type))) {
#         fileInput("uploadfile", NULL, multiple = FALSE, accept = c(".rda", ".rds", ".rdata"))
#     }
# }



# #' Render clipboard UI
# #'
# #' @return A UI element for clipboard input
# render_clipboard_ui <- function() {
#     shiny::tagList(
#         shiny::renderText("Copy-and-paste data below:"),
#         textAreaInput(
#             "clipboard_groupA", "Group A",
#             rows = 5, resize = "vertical", value = "",
#             placeholder = "1.31\n5.32\n40.2"
#         ),
#         textAreaInput(
#             "clipboard_groupB", "Group B",
#             rows = 5, resize = "vertical", value = "",
#             placeholder = "1.31\n5.32\n40.2"
#         ),
#         br(),
#         actionButton("loadClipData", "Paste", icon = icon("paste", verify_fa = FALSE))
#     )
# }




##### Render UIs
#' Render dependent variables UI
#'
#' @param all_cols Reactive value for all columns
#' @return A UI element for selecting dependent variables
render_dvs_ui <- function(all_cols) {
    selectInput(
        inputId = "dependent_vars",
        label = "Dependent variables:",
        choices = all_cols(),
        multiple = TRUE,
        size = min(10, length(all_cols())),
        selectize = FALSE
    )
}

#' Render independent variables UI
#'
#' @param all_cols Reactive value for all columns
#' @param input Shiny input object
#' @return A UI element for selecting independent variables
render_ivs_ui <- function(all_cols, input) {
    selectInput(
        inputId = "independent_vars",
        label = "Independent variables:",
        choices = all_cols()[!all_cols() %in% input$dependent_vars],
        selected = all_cols()[!all_cols() %in% input$dependent_vars],
        multiple = TRUE,
        size = min(10, length(all_cols())),
        selectize = FALSE
    )
}

#' Render positive labels UI
#'
#' @param possible_positive_labels Reactive value for possible positive labels
#' @return A UI element for selecting positive labels
render_positive_labels_ui <- function(possible_positive_labels) {
    selectInput(
        inputId = "positive_label",
        label = "Positive label:",
        choices = possible_positive_labels(),
        selected = "group_A"
    )
}


##### Restriction plot

#' Render RROC plot
#'
#' @param redo_plot Reactive value for redo plot
#' @param rroc_result Reactive value for RROC result
#' @param current_data Reactive value for current data
#' @param input Shiny input object
#' @return A plot of RROC
render_rroc_plot <- function(redo_plot, rroc_result, current_data, input) {
    dv <- redo_plot()[["dv"]]
    iv <- redo_plot()[["iv"]]
    if (!has_been_calculated(dv, iv, rroc_result)) {
        return(ggplot2::ggplot() +
            ggplot2::annotate(
                "text",
                x = 0.5, y = 0.5,
                label = "Restriction not calculated for this variable.\nHit 'Run restriction' to calculate it.",
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


#' Check if RROC has been calculated
#'
#' @param dv Dependent variable
#' @param iv Independent variable
#' @param rroc_result Reactive value for RROC result
#' @return TRUE if RROC has been calculated, FALSE otherwise
has_been_calculated <- function(dv, iv, rroc_result) {
    !(is.null(rroc_result()) ||
        is.null(dv) || is.null(iv) ||
        !dv %in% names(rroc_result()) ||
        !iv %in% names(rroc_result()[[dv]]))
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

# ##### Observers
# #' Observe RROC calculation
# #'
# #' @param input Shiny input object
# #' @param current_data Reactive value for current data
# #' @param rroc_result Reactive value for RROC result
# #' @param possible_positive_labels Reactive value for possible positive labels
# observe_rroc_calculation <- function(input, current_data, rroc_result, possible_positive_labels) {
#     observeEvent(input$run_rroc, {
#         shiny::withProgress(
#             message = "Calculating restriction...",
#             detail = "",
#             value = 0,
#             {
#                 dv <- input$dependent_vars
#                 iv <- input$independent_vars
#                 if (length(dv) == 0) {
#                     warning("No dependent variable selected")
#                     return()
#                 }
#                 if (length(iv) == 0) {
#                     warning("No independent variable selected")
#                     return()
#                 }
#                 pos_label <- 1
#                 if (input$positive_label != "") {
#                     pos_label <- input$positive_label
#                 }
#                 if (is.null(rroc_result())) {
#                     rroc_res_tmp <- rroc_secure(
#                         df = current_data(),
#                         dependent_vars = input$dependent_vars,
#                         independent_vars = input$independent_vars,
#                         do_plots = TRUE,
#                         n_permutations = max(input$n_permutations, 0),
#                         positive_label = pos_label,
#                         parallel_permutations = FALSE
#                     )
#                 } else {
#                     if (input$recalculate_rroc) {
#                         new_dv_iv <- sapply(dv, function(x) iv, simplify = FALSE)
#                     } else {
#                         dvs_ivs_existing <- lapply(rroc_result(), names)
#                         new_dv_iv <- sapply(input$dependent_vars, function(dv_x) {
#                             iv[!iv %in% dvs_ivs_existing[[dv_x]]]
#                         }, simplify = FALSE)
#                     }
#                     rroc_res_tmp <- sapply(names(new_dv_iv), function(dv_x) {
#                         if (length(new_dv_iv[[dv_x]]) == 0) {
#                             return(NULL)
#                         }
#                         tmp <- sapply(new_dv_iv[[dv_x]], function(iv_x) {
#                             return(
#                                 rroc_secure(
#                                     df = current_data(),
#                                     dependent_vars = dv_x,
#                                     independent_vars = iv_x,
#                                     do_plots = TRUE,
#                                     n_permutations = max(input$n_permutations, 0),
#                                     positive_label = pos_label,
#                                     parallel_permutations = FALSE
#                                 )[[dv_x]][[iv_x]]
#                             )
#                         }, simplify = FALSE)
#                         return(tmp)
#                     }, simplify = FALSE)
#                     rroc_res_tmp <- rroc_res_tmp[!all(is.null(rroc_res_tmp))]
#                 }

#                 if (is.null(rroc_result())) {
#                     rroc_result(rroc_res_tmp)
#                 } else if (all(is.null(rroc_res_tmp)) || all(sapply(rroc_res_tmp, is.null))) {
#                     print("All results have been calculated before already")
#                 } else {
#                     new_rroc <- rroc_result()
#                     for (dv_x in names(rroc_res_tmp)) {
#                         if (!dv_x %in% names(rroc_result())) {
#                             new_rroc[[dv_x]] <- rroc_res_tmp[[dv_x]]
#                         } else {
#                             for (iv_x in names(rroc_res_tmp[[dv_x]])) {
#                                 new_rroc[[dv_x]][[iv_x]] <- rroc_res_tmp[[dv_x]][[iv_x]]
#                             }
#                         }
#                     }
#                     rroc_result(new_rroc)
#                 }
#                 output$restriction_plot <- renderPlot({
#                     rroc_result()[[dv[1]]][[iv[1]]][["plots"]][["plots"]]
#                 })
#             }
#         )
#     })
# }

#' Observe RROC plot update
#'
#' @param input Shiny input object
#' @param rroc_result Reactive value for RROC result
#' @param current_data Reactive value for current data
#' @param redo_plot Reactive value for redo plot
#' @param listen_iv_dv_first Reactive value for listening to first DV and IV
observe_rroc_plot_update <- function(input, rroc_result, current_data, redo_plot, listen_iv_dv_first) {
    observeEvent(input$independent_vars, {
        dv <- listen_iv_dv_first()[["dv"]]
        iv <- listen_iv_dv_first()[["iv"]]
        if (iv == "") {
            return()
        }
        if (has_been_calculated(dv, iv, rroc_result) && !all(is.na(rroc_result()[[dv]][[iv]]))) {
            if (is.null(rroc_result()[[dv]][[iv]][["plots"]][["plots"]])) {
                tmp_plot <- restrictedROC::plot_density_rROC_empirical(
                    values_grouped = split(current_data()[[iv]], current_data()[[dv]]),
                    positive_label = input$positive_label
                )
                tmp_rroc_res <- rroc_result()
                tmp_rroc_res[[dv]][[iv]][["plots"]][["plots"]] <- tmp_plot
                rroc_result(tmp_rroc_res)
                cat("    Restriction plot was recalculated\n")
            }
        }
        redo_plot(listen_iv_dv_first())
    })
}
