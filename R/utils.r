#' Gera Funcao A Partir De String
#' 
#' Gera uma funcao cujo corpo e `str` e argumentos as dimensoes de `modelo`
#' 
#' @param str string com expressao que sera o corpo da funcao
#' @param modelo modelo com referencia ao qual a funcao sera interpretada
#' 
#' @return funcao com corpo `str` e argumentos as variaveis de `modelo` presentes em `str`

str2function <- function(str, modelo) {
    body <- str2lang(str)
    argnames <- vars_in_str(str, modelo)
    argnames <- Filter(function(x) grepl(x, str), argnames)
    args  <- names2args(argnames)

    out_fun <- function(...) NULL

    formals(out_fun) <- c(args, formals(out_fun))
    body(out_fun) <- body

    null <- validate_function(out_fun, modelo, str)

    return(out_fun)
}

#' Extrai Variaveis De String
#'  
#' Extrai nomes de variaveis de `modelo` que aparecem em `str`
#' 
#' @param str string com expressao que sera analisada
#' @param modelo modelo com referencia ao qual a funcao sera interpretada
#' 
#' @return vetor de nomes de variaveis de `modelo` que aparecem em `str`

vars_in_str <- function(str, modelo) {
    vars <- modelo$vines$names
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
#' @param modelo modelo com referencia ao qual a funcao sera interpretada
#' @param str string original que deu origem a `fun`, para mensagens de erro
#' 
#' @return NULL invisivel se `fun` roda adequadamente, erro caso contrario

validate_function <- function(fun, modelo, str) {
    args <- lapply(modelo$vines$names, function(x) 1)
    names(args) <- modelo$vines$names

    cc <- c(list(fun), args)
    test <- try(eval(as.call(cc), envir = new.env()), TRUE)

    if (inherits(test, "try-error")) {
        msg <- paste0("Ha um erro na expressao de inferencia: '", str, "'\n", test)
        stop(msg)
    }

    return(invisible(NULL))
}
