#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # https://rstudio.github.io/shiny/reference/ExtendedTask.html


    # Initialize shared data
    r_data <- initialize_shared_data()

    # File upload UI
    output$ui_fileUpload <- shiny::renderUI({
        shiny::req(input$selected_data_type)
        uploadfile_fun(input$selected_data_type)
    })

    # Clipboard UI
    output$ui_load_clipboard <- shiny::renderUI({
        render_clipboard_ui()
    })

    # Data UI
    output$ui_data <- shiny::renderUI({
        render_data_ui()
    })


    # Data preview
    output$data_preview <- DT::renderDT({
        render_data_preview(input, current_data_table)
    })

    # Full data preview
    output$data_preview_full <- DT::renderDT({
        render_data_preview_full(input, current_data_table)
    })
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

#' Generate file input UI based on selected data type
#'
#' @param selected_data_type The type of data selected by the user
#' @return A file input UI element
uploadfile_fun <- function(selected_data_type) {
    if (selected_data_type == "csv") {
        fileInput("uploadfile", NULL, multiple = FALSE, accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv",
            ".tsv"
        ))
    } else if (any(sapply(c("rda", "rds", "Rdata"), grepl, selected_data_type))) {
        fileInput("uploadfile", NULL, multiple = FALSE, accept = c(".rda", ".rds", ".rdata"))
    }
}



#' Render clipboard UI
#'
#' @return A UI element for clipboard input
render_clipboard_ui <- function() {
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
}

#' Render data UI
#'
#' @return A UI element for data input
render_data_ui <- function() {
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
}


#' Reactive value for current data table
#'
#' @return A reactive value for the current data table
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
current_data <- reactiveVal()


#' Render data preview
#'
#' @param input Shiny input object
#' @param current_data_table Reactive value for current data table
#' @return A data table preview
render_data_preview <- function(input, current_data_table) {
    if (length(input$selected_data_type) == 0) {
        return()
    }
    current_data_table()[, seq_len(min(ncol(current_data_table()), 6))]
}

#' Render full data preview
#'
#' @param input Shiny input object
#' @param current_data_table Reactive value for current data table
#' @return A full data table preview
render_data_preview_full <- function(input, current_data_table) {
    if (length(input$selected_data_type) == 0) {
        return()
    }
    current_data_table()
}
