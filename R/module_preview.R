#' @title Preview data
#' @description This module provides a preview of the first 6 columns of a dataset.
#' @param id A character string representing the module's ID.
#' @return A Shiny module for previewing data.
previewDataUI <- function(id) {
    tabsetPanel(
        id = "tabs_data",
        tabPanel(
            "View first 6 columns",
            DT::DTOutput(NS(id, "data_preview"))
        ),
        tabPanel(
            "View full",
            DT::DTOutput(NS(id, "data_preview_full"))
        )
    )
}
#' @noRd
#' @export
#' @examples
#' previewDataUI("preview")
previewDataServer <- function(id, data) {
    stopifnot(is.reactive(data))
    moduleServer(id, function(input, output, session) {
        current_data_table <- reactive({
            if (is.null(data())) {
                return(NULL)
            }
            if (is.data.frame(data())) {
                return(data())
            } else if (is.list(data())) {
                df_long <- data.table::rbindlist(data(), idcol = "group")
                colnames(df_long) <- c("group", "value")
                return(df_long)
            }
        })

        output$data_preview <- DT::renderDT({
            data()[, seq_len(min(ncol(data()), 6))]
        })
        output$data_preview_full <- DT::renderDT(
            {
                data()
            },
            filter = "top"
        )
    })
}
