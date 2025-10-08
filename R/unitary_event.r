#' Interpreta Evento Unitario
#' 
#' Transforma string `x` em objeto para posterior construcao dos volumes de integracao
#' 
#' @param x string no formato `"lhs (>=|<=) rhs` indicando um evento para inferencia
#' @param modelo modelo com referencia ao qual o evento sera interpretado

parse_unitary_event <- function(x, modelo) {
    parsed <- parse_validate_math_expression(x, modelo)

    expr <- parsed[[1]]
    vars <- parsed[[2]]
    lower <- get_lower(x, modelo)
    upper <- get_upper(x, modelo)

    is_multivar <- length(vars) > 1
    if (is_multivar) {
        new_unitary_event_m(lower, upper, vars, expr, modelo)
    } else {
        new_unitary_event_u(lower, upper, vars, modelo)
    }
}

parse_validate_math_expression <- function(x, modelo) {
    expr_str <- clean_expression_string(x)

    parsed <- try(str2lang(expr_str), silent = TRUE)
    if (inherits(parsed, "try-error")) stop("Expressao com erro: ", expr_str)

    vars <- extract_variables_from_expr(parsed)
    validate_vars_exist(vars, modelo)

    out <- list(expr_str, vars)
    return(out)
}

clean_expression_string <- function(x, early = FALSE) {
    x <- gsub("^\\(", "", x)
    x <- gsub("\\)$", "", x)
    x <- gsub(" ", "", x)
    if (early) return(x)

    x <- sub("^[[:digit:]]+(\\.[[:digit:]]*)? ?<= ?", "", x)
    x <- sub(" ?>= ?[[:digit:]]+(\\.[[:digit:]]*)?$", "", x)
    x <- sub(" ?<= ?[[:digit:]]+(\\.[[:digit:]]*)?$", "", x)
    return(x)
}

extract_variables_from_expr <- function(expr) {
    if (is.symbol(expr)) {
        return(as.character(expr))
    }
    if (is.call(expr) && length(expr) > 1) {
        symbols <- character(0)
        for (i in 2:length(expr)) {
            symbols <- c(symbols, extract_variables_from_expr(expr[[i]]))
        }
        return(symbols)
    }
    return(character(0))
}

validate_vars_exist <- function(vars, modelo) {
    model_vars <- modelo$vines$names
    missing_vars <- setdiff(vars, model_vars)
    if (length(missing_vars) > 0) {
        stop("Variaveis de inferencia nao existem no modelo: ", paste(missing_vars, collapse = ", "))
    }
}

get_lower <- function(x, modelo) {
    x <- clean_expression_string(x, early = TRUE)
    # procura bound da forma b <= x (presente em expressoes do tipo a <= x <= b)
    val <- regmatches(x, regexpr("([[:digit:]]+(\\.[[:digit:]]*)?)(?=(<=))", x, perl = TRUE))
    if (length(val) == 0) {
        # caso nao ache, procura x >= b
        val <- regmatches(x, regexpr("(?<=(>=))([[:digit:]]+(\\.[[:digit:]]*)?)", x, perl = TRUE))
    }
    val <- as.numeric(val)
    if (identical(val, numeric(0))) NA else val
}

get_upper <- function(x, modelo) {
    x <- clean_expression_string(x, early = TRUE)
    val <- regmatches(x, regexpr("(?<=(<=))([[:digit:]]+(\\.[[:digit:]]*)?)", x, perl = TRUE))
    val <- as.numeric(val)
    if (identical(val, numeric(0))) NA else val
}

# CONSTRUTORES INTERNOS ----------------------------------------------------------------------------

new_unitary_event_u <- function(lower, upper, var, modelo) {

    ecdf <- attr(modelo, "ecdfs")[[var]]
    lower_u <- ecdf(lower)
    upper_u <- ecdf(upper)

    new <- structure(var, class = "unitary_event_u")
    attr(new, "bounds_x") <- c(lower, upper)
    attr(new, "bounds_u") <- c(lower_u, upper_u)

    return(new)
}

new_unitary_event_m <- function(lower, upper, vars, expr, modelo) {
    new <- structure(vars, class = "unitary_event_m")

    attr(new, "bounds_x") <- c(lower, upper)
    attr(new, "expr") <- expr

    return(new)
}

# METODOS ------------------------------------------------------------------------------------------

#' Extrai Bounds De Evento Unitario
#' 
#' @param x evento unitario
#' @param ... demais parametros de cada metodo
#' @param mode um de `c("x", "u")`, indicando a escala em que bounds serao retornados

event2bounds <- function(x, ...) UseMethod("event2bounds")

#' @rdname event2bounds

event2bounds.unitary_event_u <- function(x, mode = c("x", "u"), ...) {
    mode <- match.arg(mode)
    mode <- paste0("bounds_", mode)
    bounds <- attr(x, mode)
    if (is.na(bounds[1])) bounds[1] <- 0
    if (is.na(bounds[2])) bounds[2] <- ifelse(mode == "bounds_x", Inf, 1)
    return(bounds)
}

#' @rdname event2bounds

event2bounds.unitary_event_m <- function(x) {
    stop("Eventos unitarios multivariados ainda nao sao suportados")
}
