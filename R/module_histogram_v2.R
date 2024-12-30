histogramOutput <- function(id) {
    tagList(
        numericInput(NS(id, "bins"), "bins", 10, min = 1, step = 1),
        plotOutput(NS(id, "hist"))
    )
}
histogramUI <- function(id) {
    sidebarLayout(
        sidebarPanel(
            datasetInput("data", is.data.frame),
            selectVarInput("var"),
            selectFilterInput("filter"),
        ),
        mainPanel(
            histogramOutput("hist")
        )
    )
}

histogramServer <- function(id, x, title = reactive("Histogram")) {
    stopifnot(is.reactive(x))
    stopifnot(is.reactive(title))

    moduleServer(id, function(input, output, session) {
        output$hist <- renderPlot(
            {
                req(is.numeric(x()))
                main <- paste0(title(), " [", input$bins, "]")
                hist(x(), breaks = input$bins, main = main)
            },
            res = 96
        )
    })
}
