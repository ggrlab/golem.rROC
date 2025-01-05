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

    # rand_task <- ExtendedTask$new(function(...) {
    #     mirai::mirai(
    #         {
    #             # Get all arguments and ensure that they are NOT reactive. Necessary for ExtendedTask
    #             argg <- c(as.list(environment()))
    #             lapply(argg, function(x) {
    #                 stopifnot(!is.reactive(x))
    #             })

    #             print(f_a, f_b)
    #             # Slow operation goes here
    #             Sys.sleep(2)
    #             sample(1:100, 1)
    #         },
    #         ...
    #     )
    # }) |> bslib::bind_task_button(NS(id, "recalculate"))
    myfunc <- function(x, y) {
        # Get all arguments and ensure that they are NOT reactive. Necessary for ExtendedTask
        argg <- c(as.list(environment()))
        lapply(argg, function(x) {
            stopifnot(!is.reactive(x))
        })

        # print(x, y)
        # Slow operation goes here
        Sys.sleep(2)
        paste0("___", x, y, sample(1:100, 1))
    }
    rand_task <- ExtendedTask$new(function(fun, x, y) {
        mirai::mirai(
            fun(
                x = x, y = y
            ),
            environment()
        )
    }) |> bslib::bind_task_button(NS(id, "recalculate"))
    moduleServer(id, function(input, output, session) {
        observe_simple(input, output, rand_task, a, b, myfunc)
        output$out_a <- shiny::renderText({
            paste0("A is '", a(), "'")
        })
        output$out_b <- renderText({
            paste0("B is '", b(), "'")
        })

        observeEvent(rand_task$status(), {
            print(Sys.time())
            cat(": ", "RROC calculation status: ")
            print(str(rand_task$status()))
            if (rand_task$status() == "success") {
                output$output_et <- renderText({
                    paste0("Output is '", rand_task$result(), "'")
                })
            } else if (rand_task$status() == "failed") {
                output$output_et <- renderText({
                    paste0("Output is '", rand_task$result(), "'")
                })
            }
            rand_task$result()
        })
    })
}


observe_simple <- function(input, output, exttask, a, b, myfunc) {
    stopifnot(is.reactive(a))
    stopifnot(is.reactive(b))
    b(0)
    observeEvent(input$recalculate, {
        # shiny::withProgress(
        #     message = "Calculating restriction...",
        #     detail = "",
        #     value = 0,
        #     {
        exttask$invoke(myfunc, x = paste0("A=", a()), y = paste0("B=", b()))
        #     })
    })
}
