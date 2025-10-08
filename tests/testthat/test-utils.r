test_that("str2function basic functionality", {
    vars <- c("ernestina_pico", "ernestina_volume", "barra_grande_pico")

    result <- str2function("ernestina_pico + ernestina_volume", vars, bound = 1000, kind = "lower")
    expect_true(is.function(result))
    expect_equal(names(formals(result)), c("ernestina_pico", "ernestina_volume", "..."))
    expect_equal(result(600, 300), 100)
    expect_equal(result(500, 500), 0)

    result <- str2function("ernestina_pico + ernestina_volume", vars, bound = 1000, kind = "upper")
    expect_true(is.function(result))
    expect_equal(names(formals(result)), c("ernestina_pico", "ernestina_volume", "..."))
    expect_equal(result(600, 300), -100)
    expect_equal(result(500, 500), 0)
})

test_that("str2function with NA bound", {
    vars <- c("ernestina_pico", "ernestina_volume")

    result <- str2function("ernestina_pico + ernestina_volume", vars, bound = NA, kind = "lower")
    expect_null(result)

    result <- str2function("ernestina_pico + ernestina_volume", vars, bound = NA, kind = "upper")
    expect_null(result)
})

test_that("str2function complex expressions", {
    vars <- c("ernestina_pico", "ernestina_volume", "barra_grande_pico")

    result <- str2function("log(ernestina_pico * ernestina_volume)", vars, bound = 5, kind = "upper")
    expect_true(is.function(result))
    expect_equal(names(formals(result)), c("ernestina_pico", "ernestina_volume", "..."))
    expected <- log(100 * 200) - 5
    expect_equal(result(100, 200), expected)

    result <- str2function("ernestina_pico / ernestina_volume", vars, bound = 2, kind = "lower")
    expect_true(is.function(result))
    expect_equal(names(formals(result)), c("ernestina_pico", "ernestina_volume", "..."))
    expected <- 2 - (400 / 200)
    expect_equal(result(400, 200), expected)
})

test_that("vars_in_str", {
    vars <- c("ernestina_pico", "ernestina_volume", "barra_grande_pico", "barra_grande_volume")

    result <- vars_in_str("ernestina_pico + ernestina_volume", vars)
    expect_equal(sort(result), c("ernestina_pico", "ernestina_volume"))

    result <- vars_in_str("log(barra_grande_pico)", vars)
    expect_equal(result, "barra_grande_pico")

    result <- vars_in_str("nonexistent_var", vars)
    expect_equal(result, character(0))
})

test_that("names2args", {
    argnames <- c("var1", "var2", "var3")

    result <- names2args(argnames)
    expect_true(is.list(result))
    expect_equal(names(result), argnames)
    expect_true(all(sapply(result, is.null)))
})

test_that("add_bound_comparison", {
    body_expr <- quote(x + y)

    result <- add_bound_comparison(body_expr, 100, "lower")
    expect_equal(deparse(result), "100 - (x + y)")

    result <- add_bound_comparison(body_expr, 100, "upper")
    expect_equal(deparse(result), "x + y - 100")
})

test_that("build_function", {
    args <- list(x = NULL, y = NULL)
    body_expr <- quote(x + y)

    result <- build_function(args, body_expr)
    expect_true(is.function(result))
    expect_equal(names(formals(result)), c("x", "y", "..."))
    expect_equal(result(10, 20), 30)
})

test_that("validate_function", {
    modelo <- fit_modelo_cheia(minicheias)

    good_fun <- function(ernestina_pico, ernestina_volume, ...) ernestina_pico + ernestina_volume
    expect_invisible(validate_function(good_fun, modelo$vines$names, "ernestina_pico + ernestina_volume"))

    bad_fun <- function(ernestina_pico, ernestina_volume, ...) stop("error")
    expect_error(validate_function(bad_fun, modelo$vines$names, "bad_expression"))
})
