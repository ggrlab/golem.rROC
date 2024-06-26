#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    # In r_data you can store data that you want to share between modules
    r_data <- list()
    r_data[["clipboard"]] <- tibble::tibble(
        "group_A" = c(1.31, 5.32, 40.2),
        "group_B" = c(1.31, 5.32, 40.2)
    )
    uploadfile_fun <- function(accept) {
        fileInput("uploadfile", NULL, multiple = FALSE, accept = accept)
    }

    output$ui_fileUpload <- shiny::renderUI({
        shiny::req(input$selected_data_type)
        if (input$selected_data_type == "csv") {
            uploadfile_fun(
                accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv",
                    ".tsv"
                )
            )
        } else if (any(sapply(c("rda", "rds", "Rdata"), grepl, input$selected_data_type))) {
            uploadfile_fun(accept = c(".rda", ".rds", ".rdata"))
        } else if (input$selected_data_type == "clipboard") {
        } else if (input$selected_data_type == "glehr2023") {
            # Do nothing.
        } else if (input$selected_data_type == "qs") {
            warning("NotImplemented")
        } else {
            stop("Invalid data type")
        }
    })

    output$ui_load_clipboard <- shiny::renderUI({
        shiny::tagList(
            shiny::renderText("Copy-and-paste data below:"),
            textAreaInput(
                "clipboard_groupA", "Group A",
                rows = 5, resize = "vertical", value = "",
                placeholder = "1.31\n5.32\n40.2"
            ),
            textAreaInput(
                "clipboard_groupB", "Group B",
                rows = 5, resize = "vertical", value = "",
                placeholder = "1.31\n5.32\n40.2"
            ),
            br(),
            actionButton("loadClipData", "Paste", icon = icon("paste", verify_fa = FALSE))
        )
    })


    #### Data userinterface
    output$ui_data <- shiny::renderUI({
        # possible_data_types <- c("qs", "rds/rda/rData", "csv", "clipboard")
        possible_data_types <- c("csv", "clipboard", "glehr2023")
        shiny::wellPanel(
            shiny::selectInput("selected_data_type", label = "Load data of type:", possible_data_types, selected = "clipboard"),
            shiny::conditionalPanel(
                condition = "input.selected_data_type != 'clipboard'",
                shiny::conditionalPanel(
                    "input.selected_data_type == 'csv'",
                    with(tags, table(
                        td(shiny::selectInput("csv_sep", "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), ",", width = "100%")),
                        td(shiny::selectInput("csv_dec", "Decimal:", c(Period = ".", Comma = ","), ".", width = "100%")),
                        width = "100%"
                    )),
                    shiny::numericInput(
                        "data_n_max",
                        label = "Maximum rows to read:",
                        value = 2000, max = Inf, step = 1000
                    )
                ),
                shiny::uiOutput("ui_fileUpload"),
                br(),
                actionButton("reload_data", "Reload", icon = icon("upload", verify_fa = FALSE))
            ),
            shiny::conditionalPanel(
                condition = "input.selected_data_type == 'clipboard'",
                shiny::uiOutput("ui_load_clipboard")
            ),
            shiny::conditionalPanel(
                condition = "input.selected_data_type == 'glehr2023'",
                br(),
                actionButton("reload_data", "Reload", icon = icon("upload", verify_fa = FALSE))
            )
        )
    })

    current_data <- reactiveVal()
    # Set the initial value for current_data
    current_data(glehr2023_cd4_cd8_relative[, -1])
    # Result of rroc calculations
    rroc_result <- reactiveVal()
    rroc_result(frontiers110_tcell_relative__permutation_10k_small)
    output$restriction_performances <- DT::renderDT(restriction_perf())


    toListen <- reactive({
        list(
            input$uploadfile,
            input$reload_data
        )
    })
    observeEvent(toListen(), {
        if (all(is.null(input$uploadfile))) {
            return()
        }
        current_data(NULL)
        rroc_result(NULL)


        if (input$selected_data_type == "glehr2023") {
            current_data(glehr2023_cd4_cd8_relative[, -1])
            rroc_result(frontiers110_tcell_relative__permutation_10k)
            return()
        } else {
            current_data(
                data.table::fread(
                    input$uploadfile$datapath,
                    sep = input$csv_sep, dec = input$csv_dec,
                    nrows = ifelse(is.numeric(input$data_n_max), input$data_n_max, Inf)
                ) |>
                    tibble::as_tibble()
            )
        }
    })
    observeEvent(input$loadClipData, {
        if (input$clipboard_groupA == "" || input$clipboard_groupB == "") {
            # If empty content, return
            return()
        }
        if (!any(grepl("\n", input$clipboard_groupA)) || !any(grepl("\n", input$clipboard_groupB))) {
            # If only one element in any group, return
            return()
        }
        data_A <- readr::read_csv(input$clipboard_groupA, show_col_types = FALSE, col_names = FALSE)
        data_B <- readr::read_csv(input$clipboard_groupB, show_col_types = FALSE, col_names = FALSE)

        df <- list(data_A, data_B)
        names(df) <- c("group_A", "group_B")
        df_long <- data.table::rbindlist(df, idcol = "group")
        colnames(df_long) <- c("group", "value")
        current_data(df_long)
    })

    current_data_table <- reactive({
        if (is.null(current_data())) {
            return(NULL)
        }
        if (is.data.frame(current_data())) {
            return(current_data())
        } else if (is.list(current_data())) {
            df_long <- data.table::rbindlist(current_data(), idcol = "group")
            colnames(df_long) <- c("group", "value")
            return(df_long)
        }
    })
    output$data_preview <- DT::renderDT({
        if (length(input$selected_data_type) == 0) {
            return()
        }
        current_data_table()[, seq_len(min(ncol(current_data_table()), 6))]
    })
    output$data_preview_full <- DT::renderDT({
        if (length(input$selected_data_type) == 0) {
            return()
        }
        current_data_table()
    })

    #### Restriction userinterface
    all_cols <- reactive({
        names(current_data())
    })
    output$ui_dvs <- shiny::renderUI({
        selectInput(
            inputId = "dependent_vars",
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
            inputId = "independent_vars",
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
            inputId = "positive_label",
            label = "Positive label:",
            choices = possible_positive_labels(),
            selected = "group_A"
        )
    })
    dv_selector <- reactiveVal(value = TRUE)
    iv_selector <- reactiveVal(value = TRUE)
    observeEvent(input$dv_DEselect_all, {
        # invert dv_selector
        dv_selector(!dv_selector())
        if (dv_selector() == "FALSE") {
            updateSelectInput(session, "dependent_vars", selected = character(0))
        } else {
            updateSelectInput(session, "dependent_vars", selected = all_cols())
        }
    })
    observeEvent(input$iv_DEselect_all, {
        iv_selector(!iv_selector())
        if (iv_selector() == "FALSE") {
            updateSelectInput(session, "independent_vars", selected = character(0))
        } else {
            updateSelectInput(session, "independent_vars", selected = all_cols()[!all_cols() %in% input$dependent_vars])
        }
    })
    listen_iv_dv_first <- reactive({
        list(
            "dv" = input$dependent_vars[1],
            "iv" = input$independent_vars[1]
        )
    })

    has_been_calculated <- function(dv, iv) {
        !(is.null(
            # No calculation at all
            rroc_result()
        ) ||
            # Neither dv nor iv selected
            is.null(dv) || is.null(iv) ||
            # dv not in rroc_result
            !dv %in% names(rroc_result()) ||
            # iv not in rroc_result
            !iv %in% names(rroc_result()[[dv]]))
    }
    redo_plot <- reactiveVal(list("dv" = "", "iv" = ""))
    rroc_plot <- observeEvent(input$independent_vars, {
        dv <- listen_iv_dv_first()[["dv"]]
        iv <- listen_iv_dv_first()[["iv"]]
        if(iv == ""){return()}
        if (has_been_calculated(dv, iv) && !all(is.na(rroc_result()[[dv]][[iv]]))) {
            if (is.null(rroc_result()[[dv]][[iv]][["plots"]][["plots"]])) {
                # Then it was probably a result of glehr2023. This is pre-calculated
                # but the plots are not stored. So we need to recalculate it.
                tmp_plot <- restrictedROC::plot_density_rROC_empirical(
                    values_grouped = split(current_data()[[iv]], current_data()[[dv]]),
                    positive_label = input$positive_label,
                )
                tmp_rroc_res <- rroc_result()
                tmp_rroc_res[[dv]][[iv]][["plots"]][["plots"]] <- tmp_plot
                rroc_result(tmp_rroc_res)
                cat("    Restriction plot was recalculated\n")
            }
        }
        # reupdate listen_iv_dv_first
        redo_plot(listen_iv_dv_first())
    })
    output$rroc_plot <- renderPlot({
        dv <- redo_plot()[["dv"]]
        iv <- redo_plot()[["iv"]]
        print(paste0("Plotting ", dv, ": ", iv))
        if (!has_been_calculated(dv, iv)) {
            # Then return a plot that says "No data"
            print("    Plotting no data")
            return(ggplot2::ggplot() +
                ggplot2::annotate(
                    "text",
                    x = 0.5, y = 0.5,
                    label = "Restriction not calculated for this variable.\nHit 'Run restriction' to calculate it.",
                    size = 10
                ) +
                ggplot2::theme_void())
        } else if (all(is.na(rroc_result()[[dv]][[iv]]))) {
            print("    Plotting not calculatable")
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
            print("    Plotting restriction")
            return(rroc_result()[[dv]][[iv]][["plots"]][["plots"]])
        }
    })
    # observeEvent(input$dependent_vars, {
    #     print(input$dependent_vars)
    # })
    possible_positive_labels <- reactive({
        if (length(input$dependent_vars) != 1) {
            return(NULL)
        } else {
            return(unique(current_data()[[input$dependent_vars]]))
        }
    })
    observeEvent(input$run_rroc, {
        shiny::withProgress(
            message = "Calculating restriction...",
            detail = "",
            value = 0,
            {
                dv <- input$dependent_vars
                iv <- input$independent_vars
                if (length(dv) == 0) {
                    warning("No dependent variable selected")
                    return()
                }
                if (length(iv) == 0) {
                    warning("No independent variable selected")
                    return()
                }
                pos_label <- 1
                if (input$positive_label != "") {
                    pos_label <- input$positive_label
                }
                if (is.null(rroc_result())) {
                    # If the reactive value is NULL, then calculate the ROC (First time)
                    rroc_res_tmp <- rroc_secure(
                        df = current_data(),
                        dependent_vars = input$dependent_vars,
                        independent_vars = input$independent_vars,
                        do_plots = TRUE,
                        n_permutations = max(input$n_permutations, 0),
                        positive_label = pos_label,
                        parallel_permutations = FALSE
                    )
                } else {
                    # Otherwise: If recalculation should be done, then recalculate all dvs and ivs
                    if (input$recalculate_rroc) {
                        new_dv_iv <- sapply(dv, function(x) iv, simplify = FALSE)
                    } else {
                        # Otherwise, only recalculate the dvs and ivs which have not been calculated yet
                        dvs_ivs_existing <- lapply(rroc_result(), names)
                        new_dv_iv <- sapply(input$dependent_vars, function(dv_x) {
                            iv[!iv %in% dvs_ivs_existing[[dv_x]]]
                        }, simplify = FALSE)
                    }
                    rroc_res_tmp <- sapply(names(new_dv_iv), function(dv_x) {
                        if (length(new_dv_iv[[dv_x]]) == 0) {
                            return(NULL)
                        }
                        tmp <- sapply(new_dv_iv[[dv_x]], function(iv_x) {
                            return(
                                rroc_secure(
                                    df = current_data(),
                                    dependent_vars = dv_x,
                                    independent_vars = iv_x,
                                    do_plots = TRUE,
                                    n_permutations = max(input$n_permutations, 0),
                                    positive_label = pos_label,
                                    parallel_permutations = FALSE
                                )[[dv_x]][[iv_x]]
                            )
                        }, simplify = FALSE)
                    }, simplify = FALSE)
                    rroc_res_tmp <- rroc_res_tmp[!all(is.null(rroc_res_tmp))]
                }

                ## Update (reactive) rroc_result
                if (is.null(rroc_result())) {
                    rroc_result(rroc_res_tmp)
                } else if (all(is.null(rroc_res_tmp)) || all(sapply(rroc_res_tmp, is.null))) {
                    # Then all results have been calculated before already
                    print("All results have been calculated before already")
                } else {
                    new_rroc <- rroc_result()
                    for (dv_x in names(rroc_res_tmp)) {
                        if (!dv_x %in% names(rroc_result())) {
                            new_rroc[[dv_x]] <- rroc_res_tmp[[dv_x]]
                        } else {
                            for (iv_x in names(rroc_res_tmp[[dv_x]])) {
                                new_rroc[[dv_x]][[iv_x]] <- rroc_res_tmp[[dv_x]][[iv_x]]
                            }
                        }
                    }
                    # Update the reactive value
                    rroc_result(new_rroc)
                }
                output$restriction_plot <- renderPlot({
                    rroc_result()[[dv[1]]][[iv[1]]][["plots"]][["plots"]]
                })
            }
        )
    })
    output$restriction_performances <- DT::renderDT(restriction_perf())
    restriction_perf <- reactive({
        perf_table <- restrictedROC:::summary.rROC(rroc_result())
        if (!is.null(perf_table) & !any(sapply(listen_iv_dv_first(), is.null))) {
            perf_table <- perf_table[order(
                # First, order by the dependent variable
                sub(listen_iv_dv_first()[[1]], "", perf_table[["level_1"]], fixed = TRUE),
                # Second, order by the independent variable
                sub(listen_iv_dv_first()[[2]], "", perf_table[["level_2"]], fixed = TRUE)
            ), ]
        }
        return(perf_table)
    })
    output$download_rroc <- downloadHandler(
        filename = function() {
            "RestrictionPerformance.xlsx"
        },
        content = function(fname) {
            if (!is.null(restriction_perf())) {
                return(writexl::write_xlsx(restriction_perf(), fname))
            } else {
                return(restriction_perf())
            }
        }
    )
}
