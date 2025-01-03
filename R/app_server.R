#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    options(progressr.enable = TRUE)

    # https://rstudio.github.io/shiny/reference/ExtendedTask.html
    #### Initialize reactives
    current_data <- reactiveVal()
    # Set the initial value for current_data
    current_data(glehr2023_cd4_cd8_relative[, -1])

    rroc_result <- reactiveVal()
    # # Initialize rroc_result with a small example
    # rroc_result(frontiers110_tcell_relative__permutation_10k_small)

    #### Initialize shared data
    r_data <- initialize_shared_data()

    # Modules
    # Give current_data as reactive input to the module (without "evaluation" brackets)
    previewDataServer("preview", current_data)
    newDataServer(
        "newdata",
        current_data,
        rroc_result,
        possible_data_types = c("csv", "clipboard", "glehr2023")
    )

    rroc_selector <- calc_rroc_server("calc_rroc_selector", current_data, rroc_result)
    report_rroc_server(
        "report_rroc",
        current_data,
        rroc_result,
        rroc_selector$listen_iv_dv_first,
        rroc_selector$positive_label
    )
    a <- reactiveVal()
    b <- reactiveVal()
    simpleET_server("simpleET", a, b)
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
