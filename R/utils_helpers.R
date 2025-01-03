#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

rroc_secure <- function(df,
                        dependent_vars,
                        independent_vars,
                        n_permutations,
                        positive_label,
                        show_progress = TRUE,
                        total_calculations = NULL,
                        ...) {
    # 1. Check that dependent_vars has two levels
    checked_dv <- sapply(dependent_vars, function(dv_x) {
        if (length(unique(df[[dv_x]][!is.na(df[[dv_x]])])) != 2) {
            warning(paste0("The dependent variable ", dv_x, " has more than two levels and is omitted."))
            return(FALSE)
        } else {
            return(TRUE)
        }
    })
    checked_dependent_vars <- dependent_vars[checked_dv]

    # 2. Check that independent_vars are numeric
    checked_iv <- sapply(independent_vars, function(iv_x) {
        if (!is.numeric(df[[iv_x]])) {
            warning(paste0("The independent variable ", iv_x, " is not numeric and is omitted."))
            return(FALSE)
        } else {
            return(TRUE)
        }
    })
    checked_independent_vars <- independent_vars[checked_iv]
    if (length(dependent_vars) == 0 || length(independent_vars) == 0) {
        warning("No valid dependent or independent variables left, no calculation done")
        return(NULL)
    }

    ### Actually calculate the ROC
    if (show_progress && is.null(total_calculations)) {
        total_calculations <- length(checked_dependent_vars) * length(checked_independent_vars)
    }

    reslist <- sapply(dependent_vars, function(dv_x) {
        sapply(independent_vars, function(iv_x) {
            if (dv_x %in% checked_dependent_vars && iv_x %in% checked_independent_vars) {
                if (show_progress) {
                    shiny::incProgress(
                        amount = 1 / total_calculations,
                        detail = paste0("Calculating ROC for ", dv_x, " and ", iv_x)
                    )
                }
                rroc_res <- restrictedROC::rROC(
                    x = df,
                    dependent_vars = dv_x,
                    independent_vars = iv_x,
                    n_permutations = max(n_permutations, 0),
                    positive_label = positive_label,
                    ...
                )[[1]][[1]]
            } else {
                rroc_res <- NA
            }
            return(rroc_res)
        }, simplify = FALSE)
    }, simplify = FALSE)
    return(reslist)
}
