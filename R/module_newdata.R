newDataUI <- function(id) {
    uiOutput(NS(id, "ui_data"))
    # uiOutput(ns("controls"))
}
newDataServer <- function(id, data00, possible_data_types = c("csv", "clipboard", "glehr2023")) {
    stopifnot(shiny::is.reactive(data00))
    moduleServer(
        id,
        function(input, output, session) {
            ##### UIs
            # File upload UI
            uploadfile_fun <- function(accept) {
                fileInput(NS(id, "uploadfile"), NULL, multiple = FALSE, accept = accept)
            }
            output$ui_fileUpload <- shiny::renderUI({
                ns <- session$ns
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
                    warning("NotImplemented for security reasons")
                    # uploadfile_fun(accept = c(".rda", ".rds", ".rdata"))
                } else if (input$selected_data_type == "clipboard") {
                } else if (input$selected_data_type == "glehr2023") {
                    # Do nothing.
                } else if (input$selected_data_type == "qs") {
                    warning("NotImplemented for security reasons")
                } else {
                    stop("Invalid data type")
                }
            })
            # Clipboard UI
            output$ui_load_clipboard <- shiny::renderUI({
                shiny::tagList(
                    shiny::renderText("Copy-and-paste data below:"),
                    textAreaInput(
                        NS(id, "clipboard_groupA"), "Group A",
                        rows = 5, resize = "vertical", value = "",
                        placeholder = "1.31\n5.32\n40.2"
                    ),
                    textAreaInput(
                        NS(id, "clipboard_groupB"), "Group B",
                        rows = 5, resize = "vertical", value = "",
                        placeholder = "1.31\n5.32\n40.2"
                    ),
                    br(),
                    actionButton(NS(id, "loadClipData"), "Paste", icon = icon("paste", verify_fa = FALSE))
                )
            })
            output$ui_data <- shiny::renderUI({
                render_data_ui(id, possible_data_types)
            })

            # ##### Observers

            # toListen <- reactive({
            #     list(
            #         input$uploadfile,
            #         input$reload_data
            #     )
            # })
            # observeEvent(toListen(), {
            #     if (all(is.null(input$uploadfile))) {
            #         return()
            #     }
            #     data00(NULL)
            #     # rroc_result(NULL)


            #     if (input$selected_data_type == "glehr2023") {
            #         data00(glehr2023_cd4_cd8_relative[, -1])
            #         # rroc_result(frontiers110_tcell_relative__permutation_10k)
            #         return()
            #     } else {
            #         data00(
            #             data.table::fread(
            #                 input$uploadfile$datapath,
            #                 sep = input$csv_sep, dec = input$csv_dec,
            #                 nrows = ifelse(is.numeric(input$data_n_max), input$data_n_max, Inf)
            #             ) |>
            #                 tibble::as_tibble()
            #         )
            #     }
            # })
            # observeEvent(input$loadClipData, {
            #     if (input$clipboard_groupA == "" || input$clipboard_groupB == "") {
            #         # If empty content, return
            #         return()
            #     }
            #     if (!any(grepl("\n", input$clipboard_groupA)) || !any(grepl("\n", input$clipboard_groupB))) {
            #         # If only one element in any group, return
            #         return()
            #     }
            #     data_A <- readr::read_csv(input$clipboard_groupA, show_col_types = FALSE, col_names = FALSE)
            #     data_B <- readr::read_csv(input$clipboard_groupB, show_col_types = FALSE, col_names = FALSE)

            #     df <- list(data_A, data_B)
            #     names(df) <- c("group_A", "group_B")
            #     df_long <- data.table::rbindlist(df, idcol = "group")
            #     colnames(df_long) <- c("group", "value")
            #     data00(df_long)
            # })
        }
    )
}


#' Render data UI
#'
#' @return A UI element for data input
render_data_ui <- function(id, possible_data_types = c("csv", "clipboard", "glehr2023")) {
    ns <- NS(id)
    list(
        shiny::selectInput(
            ns("selected_data_type"),
            label = "Load data of type:",
            possible_data_types, selected = "clipboard"
        ),
        shiny::conditionalPanel(
            condition = "input.selected_data_type != 'clipboard'",
            # If there is a CSV
            shiny::conditionalPanel(
                "input.selected_data_type == 'csv'",
                with(shiny::tags, table(
                    td(shiny::selectInput(
                        "csv_sep", "Separator:",
                        c(Comma = ",", Semicolon = ";", Tab = "\t"), ",",
                        width = "100%"
                    )),
                    td(shiny::selectInput(
                        "csv_dec", "Decimal:",
                        c(Period = ".", Comma = ","), ".",
                        width = "100%"
                    )),
                    width = "100%"
                )),
                shiny::numericInput(
                    ns("data_n_max"),
                    label = "Maximum rows to read:",
                    value = 2000, max = Inf, step = 1000
                ),
                ns = ns
            ),
            # Otherwise, if there is a file upload
            shiny::uiOutput(ns("ui_fileUpload")),
            br(),
            actionButton(ns("reload_data"), "Reload", icon = shiny::icon("upload", verify_fa = FALSE)),
            ns = ns
        ),
        shiny::conditionalPanel(
            condition = "input.selected_data_type == 'clipboard'",
            shiny::uiOutput(ns("ui_load_clipboard")),
            ns = ns
        )
    )
}



#' Render clipboard UI
#'
#' @return A UI element for clipboard input
render_clipboard_ui <- function(id) {
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
        actionButton(NS(id, "loadClipData"), "Paste", icon = icon("paste", verify_fa = FALSE))
    )
}
