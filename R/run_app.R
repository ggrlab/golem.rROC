#' Run the Shiny Application
#'
#' @param n_daemons
#' Number of daemons to run in parallel. Used by `mirai::daemons`.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    n_daemons = 3,
                    ...) {
    with(mirai::daemons(n_daemons), {
        with_golem_options(
            app = shinyApp(
                ui = app_ui,
                server = app_server,
                onStart = onStart,
                options = options,
                enableBookmarking = enableBookmarking,
                uiPattern = uiPattern
            ),
            golem_opts = list(...)
        )
    })
}
