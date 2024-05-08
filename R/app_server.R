#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic

    output$data_table <- DT::renderDT({
        shinipsum::random_table(10, 5)
    })
    output$image <- renderImage(
        {
            shinipsum::random_image()
        },
        # deleteFile:
        #   Generally speaking, if the image is a temp file generated within func,
        #   then this should be TRUE ; if the image is not a temp file, this should be FALSE
        deleteFile = TRUE
    )
    output$plot <- shiny::renderPlot({
        shinipsum::random_ggplot()
    })
    output$print <- renderPrint({
        shinipsum::random_print("model")
    })
    output$table <- renderTable({
        shinipsum::random_table(10, 5)
    })
    output$text <- renderText({
        shinipsum::random_text(nwords = 50)
    })
}
