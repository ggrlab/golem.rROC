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
                tabPanel(
                    "Restriction",
                    sidebarLayout(
                        sidebarPanel(
                            calc_rroc_ui("calc_rroc_selector"),
                        ),
                        mainPanel(
                            tabPanel(
                                "Plot",
                                h2("Restriction"),
                                plotOutput("rroc_plot"),
                                # Make a downloadable DT table
                                DT::DTOutput("restriction_performances"),
                                downloadButton("download_rroc", "Download")
                            )
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
                # tabPanel(
                #     "HistogramModule duplicate",
                #     ui_histogram("hist1")
                # ),
                # tabPanel(
                #     "HistogramModule NOdup",
                #     ui_histogram("hist2")
                # ),
                # tabPanel(
                #     "DatasetModule",
                #     fluidPage(
                #         datasetInput("dataset", filter = NULL),
                #         tableOutput("data")
                #     )
                # ),
                # tabPanel(
                #     "SelectVarModule",
                #     fluidPage(
                #         datasetInput("data", is.data.frame),
                #         selectVarInput("var"),
                #         verbatimTextOutput("out")
                #     )
                # ),
                # tabPanel(
                #     "NewHist",
                #     fluidPage(
                #         histogramUI("hist")
                #     )
                # ),
                # tabPanel(
                #     "asdf",
                #     fluidPage(
                #         sidebarLayout(
                #             sidebarPanel(
                #                 datasetInput("data", is.data.frame),
                #                 textOutput("n"),
                #                 filterUI("filter"),
                #             ),
                #             mainPanel(
                #                 tableOutput("table")
                #             )
                #         )
                #     )
                # )
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
