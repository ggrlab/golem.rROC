#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        tabPanel(
            "DT",
            h2("A Random DT"),
            DT::DTOutput("data_table")
        ),
        tabPanel(
            "Image",
            h2("A Random Image"),
            plotOutput("image")
        ),
        tabPanel(
            "Plot",
            h2("A Random Plot"),
            plotOutput("plot")
        ),
        tabPanel(
            "Print",
            h2("A Random Print"),
            verbatimTextOutput("print")
        ),
        tabPanel(
            "Table",
            h2("A Random Table"),
            tableOutput("table")
        ),
        tabPanel(
            "Text",
            h2("A Random Text"),
            tableOutput("text")
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
