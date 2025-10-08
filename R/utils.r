#' Gera Funcao A Partir De String
#' 
#' Gera uma funcao com corpo baseado em `str` e `bounds` e argumentos `vars`
#' 
#' `str2function` gera funces `g(x)` para um contexto de comparacao `g(x) <= 0`. A funcao `g(x)`
#' sera montada a partir de `str`, que deve ser uma expressao valida em R, e `bound`. Se `kind` for
#' `"lower"`, a funcao sera `g(x) = bound - (str)` e, se for `"upper"`, `g(x) = (str) - bound`.
#' 
#' @param str string com expressao que sera o corpo da funcao
#' @param vars vetor de nomes de variaveis que podem ser usadas em `str`
#' @param bound valor numerico que sera usado na comparacao do corpo da funcao
#' @param kind um de `c("lower", "upper")`, indicando se a comparacao sera `f(x) >= bound` ou
#'     `f(x) <= bound`
#' 
#' @return funcao gerada

str2function <- function(str, vars, bound = NA, kind = c("lower", "upper")) {
    if (is.na(bound)) return(NULL)

    argnames <- vars_in_str(str, vars)
    argnames <- Filter(function(x) grepl(x, str), argnames)
    args  <- names2args(argnames)

    body <- str2lang(str)
    body <- add_bound_comparison(body, bound, kind)

    out_fun <- build_function(args, body)
    validate_function(out_fun, vars, str)

    return(out_fun)
}

#' Constroi Funcao A Partir De Argumentos E Corpo
#' 
#' Gera uma funcao com argumentos `args` e corpo `body`
#' 
#' @param args lista de argumentos para a funcao
#' @param body corpo da funcao, como uma expressao
#' 
#' @return funcao gerada

build_function <- function(args, body) {
    out_fun <- function(...) NULL
    formals(out_fun) <- c(args, formals(out_fun))
    body(out_fun) <- body
    return(out_fun)
}

#' Adiciona Comparacao De Bound Ao Corpo De Funcao
#' 
#' Altera corpo `body` para incluir comparacao com `bound` no formato `f(x) <= 0`
#' 
#' @param body corpo da funcao, como uma expressao
#' @param bound valor numerico que sera usado na comparacao do corpo da funcao
#' @param kind um de `c("lower", "upper")`, indicando se a comparacao sera `f(x) >= bound` ou
#'     `f(x) <= bound`
#' 
#' @return corpo alterado, como uma expressao

add_bound_comparison <- function(body, bound, kind = c("lower", "upper")) {
    body <- deparse(body)
    kind <- match.arg(kind)
    if (kind == "lower") {
        body <- paste0(bound, "-(", body, ")")
    } else {
        body <- paste0(body, "-", bound)
    }
    body <- str2lang(body)
    return(body)
}

#' Extrai Variaveis De String
#'  
#' Filtra `vars` para aquelas que aparecem em `str`
#' 
#' @param str string com expressao que sera analisada
#' @param vars vetor de nomes de variaveis para procurar
#' 
#' @return subset dos elementos de `vars` que aparecem em `str`

vars_in_str <- function(str, vars) {
    vars <- Filter(function(x) grepl(x, str), vars)
    return(vars)
}

#' Gera Lista De Argumentos De Funcao
#' 
#' Gera lista de argumentos com nomes `argnames` e valores `NULL` para composicao de funcoes
#' 
#' @param argnames vetor de nomes de argumentos
#' 
#' @return lista de argumentos com nomes `argnames` e valores `NULL`

names2args <- function(argnames) {
    names(argnames) <- argnames
    args <- lapply(argnames, function(x) NULL)
    return(args)
}

#' Valida Se `fun` Roda Adequadamente
#' 
#' Tenta executar `fun` com argumentos dummy; se falhar, retorna erro
#' 
#' @param fun funcao a ser testada
#' @param vars vetor de nomes de variaveis que podem ser usadas em `str`
#' @param str string original que deu origem a `fun`, para mensagens de erro
#' 
#' @return NULL invisivel se `fun` roda adequadamente, erro caso contrario

validate_function <- function(fun, vars, str) {
    args <- lapply(vars, function(x) 1)
    names(args) <- vars

    cc <- c(list(fun), args)
    test <- try(eval(as.call(cc), envir = new.env()), TRUE)

    if (inherits(test, "try-error")) {
        msg <- paste0("Ha um erro na expressao de inferencia: '", str, "'\n", test)
        stop(msg)
    }

    return(invisible(NULL))
}
