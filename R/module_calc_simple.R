#' @title Preview data
#' @description This module provides a preview of the first 6 columns of a dataset.
#' @param id A character string representing the module's ID.
#' @return A Shiny module for previewing data.
simpleET_ui <- function(id) {
    ns <- NS(id)
    list(
        bslib::input_task_button(ns("recalculate"), "Recalculate"),
        textOutput(ns("out_a")),
        textOutput(ns("out_b")),
        textOutput(ns("output_et"))
    )
}
#' @noRd
#' @export
#' @examples
#' previewDataUI("preview")
simpleET_server <- function(id, a, b) {
    stopifnot(is.reactive(a))
    stopifnot(is.reactive(b))

    rand_task <- ExtendedTask$new(function(f_a, f_b, ...) {
        # Get all arguments and ensure that they are NOT reactive. Necessary for ExtendedTask
        argg <- c(as.list(environment()), list(...))
        lapply(argg, function(x) {
            stopifnot(!is.reactive(x))
        })

        future::future(
            {
                print(f_a, f_b)
                # Slow operation goes here
                Sys.sleep(2)
                sample(1:100, 1)
            },
            seed = TRUE
        )
    })
    bslib::bind_task_button(rand_task, NS(id, "recalculate"))
    moduleServer(id, function(input, output, session) {
        observe_simple(input, output, rand_task, a, b)
        output$out_a <- shiny::renderText({
            paste0("A is '", a(), "'")
        })
        output$out_b <- renderText({
            paste0("B is '", b(), "'")
        })
        output$output_et <- renderText({
            paste0("Output is '", rand_task$result(), "'")
        })
        
    })
}


observe_simple <- function(input, output, exttask, a, b) {
    stopifnot(is.reactive(a))
    stopifnot(is.reactive(b))

    observeEvent(input$recalculate, {
        # shiny::withProgress(
        #     message = "Calculating restriction...",
        #     detail = "",
        #     value = 0,
        #     {
        exttask$invoke(a(), b())
        #     })
    })
}
