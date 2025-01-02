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

    # #### Plots and Tables
    # # Restriction plot
    # output$rroc_plot <- renderPlot({
    #     render_rroc_plot(redo_plot, rroc_result, current_data, input)
    # })

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
    # Give current_data as reactive input to the module (without "evaluation" brackets)
    previewDataServer("preview", current_data)
    newDataServer("newdata", current_data)
    rroc_selector <- calc_rroc_server("calc_rroc_selector", current_data, rroc_result)
    report_rroc_server(
        "report_rroc",
        current_data, rroc_result,
        rroc_selector$listen_iv_dv_first, rroc_selector$positive_label
    )
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
