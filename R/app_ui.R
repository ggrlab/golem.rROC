#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        # golem_add_external_resources(),
        fluidPage(
            tabsetPanel(
                id = "tabs_restriction",
                # tabPanel(
                #     "Testing ExtendedTask",
                #     simpleET_ui("simpleET")
                # ),
                tabPanel(
                    "Restriction",
                    sidebarLayout(
                        sidebarPanel(
                            calc_rroc_ui("calc_rroc_selector"),
                        ),
                        mainPanel(
                            report_rroc_ui("report_rroc")
                        )
                    )
                ),
                tabPanel(
                    "New Data",
                    sidebarLayout(
                        sidebarPanel = sidebarPanel(newDataUI("newdata"), width = 3),
                        mainPanel = mainPanel(previewDataUI("preview"))
                    )
                ),
            )
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "golem.rROC"
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
    )
}
