#' Interpreta Evento Unitario
#' 
#' Transforma string `x` em objeto para posterior construcao dos volumes de integracao
#' 
#' @param x string no formato `"lhs (>=|<=) rhs` indicando um evento para inferencia

parse_unitary_expr <- function(x, modelo) {

    lower <- get_lower(x, modelo)
    upper <- get_upper(x, modelo)

    expr <- get_event_expr(x)

    is_multivar <- grepl("(\\+|\\-)", expr)
    if (is_multivar) {
        new_unitary_event_m(lower, upper, expr, modelo)
    } else {
        new_unitary_event_u(lower, upper, expr, modelo)
    }
}

get_event_expr <- function(x) {
    x <- sub("^[[:digit:]]* ?<= ?", "", x)
    x <- sub("^[[:digit:]]* ?>= ?", "", x)
    x <- sub(" ?<= ?[[:digit:]]*$", "", x)
    return(x)
}

get_lower <- function(x, modelo) {
    val <- regmatches(x, regexpr("([[:digit:]]+ )(?=(<=))", x, perl = TRUE))
    if (length(val) == 0) {
        val <- regmatches(x, regexpr("(?<=(>=))( ?[[:digit:]]+)", x, perl = TRUE))
    }
    val <- as.numeric(val)
    if (identical(val, numeric(0))) 0 else val
}

get_upper <- function(x, modelo) {
    val <- regmatches(x, regexpr("(?<=(<=))( ?[[:digit:]]+)", x, perl = TRUE))
    val <- as.numeric(val)
    if (identical(val, numeric(0))) 1 else val
}

# CONSTRUTORES INTERNOS ----------------------------------------------------------------------------

new_unitary_event_u <- function(lower, upper, expr, modelo) {

    ecdf <- attr(modelo, "ecdfs")[[expr]]
    lower_u <- ecdf(lower)
    upper_u <- ecdf(upper)

    new <- structure(expr, class = "unitary_event_u")
    attr(new, "bounds_x") <- c(lower, upper)
    attr(new, "bounds_u") <- c(lower_u, upper_u)

    return(new)
}

new_unitary_event_m <- function(lower, upper, expr, modelo) {
    stop("Eventos unitarios multivariados ainda nao sao suportados")
}

# METODOS ------------------------------------------------------------------------------------------

event2bounds <- function(x, ...) UseMethod("event2bounds")

event2bounds.unitary_event_u <- function(x, mode = c("x", "u"), ...) {
    mode <- match.arg(mode)
    mode <- paste0("bounds_", mode)
    attr(x, mode)
}

event2bounds.unitary_event_m <- function(x) {
    stop("Eventos unitarios multivariados ainda nao sao suportados")
}
