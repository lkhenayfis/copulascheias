test_that("parse_validate_math_expression simple expressions", {
    modelo <- fit_modelo_cheia(minicheias)

    result <- parse_validate_math_expression("ernestina_pico <= 1", modelo)
    expect_equal(result[[1]], "ernestina_pico")
    expect_equal(result[[2]], "ernestina_pico")

    result <- parse_validate_math_expression("0 <= barra_grande_volume <= 1", modelo)
    expect_equal(result[[1]], "barra_grande_volume")
    expect_equal(result[[2]], "barra_grande_volume")

    result <- parse_validate_math_expression("ernestina_duracao >= 5.5", modelo)
    expect_equal(result[[1]], "ernestina_duracao")
    expect_equal(result[[2]], "ernestina_duracao")
})

test_that("parse_validate_math_expression mathematical expressions", {
    modelo <- fit_modelo_cheia(minicheias)

    result <- parse_validate_math_expression("ernestina_pico + ernestina_volume >= 1000", modelo)
    expect_equal(result[[1]], "ernestina_pico+ernestina_volume")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))

    result <- parse_validate_math_expression("ernestina_pico - barra_grande_pico <= 500", modelo)
    expect_equal(result[[1]], "ernestina_pico-barra_grande_pico")
    expect_equal(result[[2]], c("ernestina_pico", "barra_grande_pico"))

    result <- parse_validate_math_expression("ernestina_pico * ernestina_volume >= 800", modelo)
    expect_equal(result[[1]], "ernestina_pico*ernestina_volume")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))

    result <- parse_validate_math_expression("ernestina_pico / ernestina_volume <= 2", modelo)
    expect_equal(result[[1]], "ernestina_pico/ernestina_volume")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))
})

test_that("parse_validate_math_expression function calls", {
    modelo <- fit_modelo_cheia(minicheias)

    result <- parse_validate_math_expression("log(ernestina_pico) <= 5", modelo)
    expect_equal(result[[1]], "log(ernestina_pico)")
    expect_equal(result[[2]], "ernestina_pico")

    result <- parse_validate_math_expression("sqrt(ernestina_volume) >= 10", modelo)
    expect_equal(result[[1]], "sqrt(ernestina_volume)")
    expect_equal(result[[2]], "ernestina_volume")

    result <- parse_validate_math_expression("exp(barra_grande_pico / 100) <= 2", modelo)
    expect_equal(result[[1]], "exp(barra_grande_pico/100)")
    expect_equal(result[[2]], "barra_grande_pico")
})

test_that("parse_validate_math_expression complex combinations", {
    modelo <- fit_modelo_cheia(minicheias)

    result <- parse_validate_math_expression("log(ernestina_pico * ernestina_volume) <= 5", modelo)
    expect_equal(result[[1]], "log(ernestina_pico*ernestina_volume)")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))

    result <- parse_validate_math_expression("0 <= (ernestina_pico + ernestina_volume) / 2 <= 300", modelo)
    expect_equal(result[[1]], "(ernestina_pico+ernestina_volume)/2")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))

    result <- parse_validate_math_expression("sqrt(ernestina_pico^2 + ernestina_volume^2) >= 100", modelo)
    expect_equal(result[[1]], "sqrt(ernestina_pico^2+ernestina_volume^2)")
    expect_equal(result[[2]], c("ernestina_pico", "ernestina_volume"))
})

test_that("parse_validate_math_expression error handling", {
    modelo <- fit_modelo_cheia(minicheias)

    expect_error(parse_validate_math_expression("nonexistent_var <= 1", modelo))
    expect_error(parse_validate_math_expression("ernestina_pico + nonexistent_var >= 100", modelo))
    expect_error(parse_validate_math_expression("invalid + + syntax", modelo))
    expect_error(parse_validate_math_expression("log( >= 5", modelo))
})

test_that("get_lower|upper basic functionality", {
    str <- "ernestina_pico"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_true(is.na(lower))
    expect_true(is.na(upper))

    str <- "0 <= ernestina_pico <= 20"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

    str <- "ernestina_pico <= 1.2"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_true(is.na(lower))
    expect_equal(upper, 1.2)

    str <- "ernestina_pico >= 0.5"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.5)
    expect_true(is.na(upper))

    str <- "10.98 <= ernestina_pico"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 10.98)
    expect_true(is.na(upper))
})

test_that("get_lower|upper precision and edge cases", {
    str <- "0.54 <= ernestina_pico <= 20.546"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.54)
    expect_equal(upper, 20.546)

    str <- "100 <= ernestina_pico <= 100"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 100)
    expect_equal(upper, 100)

    str <- "0.12345 <= ernestina_pico <= 999.98765"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.12345)
    expect_equal(upper, 999.98765)

    str <- "  0  <=  ernestina_pico  <=  20  "
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

    str <- "0<=ernestina_pico<=20"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)
})

test_that("get_lower|upper mathematical expressions", {
    str <- "ernestina_pico + ernestina_volume <= 1000"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_true(is.na(lower))
    expect_equal(upper, 1000)

    str <- "1 <= ernestina_pico / ernestina_volume"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 1)
    expect_true(is.na(upper))

    str <- "5 <= log(ernestina_pico * ernestina_volume) <= 15"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 5)
    expect_equal(upper, 15)

    str <- "0.1 <= sqrt(ernestina_pico^2 + ernestina_volume^2)"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.1)
    expect_true(is.na(upper))
})

test_that("clean_expression_string", {
    expect_equal(clean_expression_string("ernestina_pico <= 1"), "ernestina_pico")
    expect_equal(clean_expression_string("0 <= ernestina_pico"), "ernestina_pico")
    expect_equal(clean_expression_string("0 <= ernestina_pico <= 1"), "ernestina_pico")
    expect_equal(clean_expression_string("  ernestina_pico + ernestina_volume  "),
        "ernestina_pico+ernestina_volume")
    expect_equal(clean_expression_string("(ernestina_pico + ernestina_volume)"),
        "ernestina_pico+ernestina_volume")
    expect_equal(clean_expression_string("1.5 <= log(ernestina_pico * ernestina_volume) <= 5.0"),
        "log(ernestina_pico*ernestina_volume)")
    expect_equal(clean_expression_string("sqrt(ernestina_pico^2 + ernestina_volume^2) >= 100"),
        "sqrt(ernestina_pico^2+ernestina_volume^2)")
})

test_that("extract_variables_from_expr", {
    modelo <- fit_modelo_cheia(minicheias)

    expr <- str2lang("ernestina_pico")
    vars <- extract_variables_from_expr(expr)
    expect_equal(vars, "ernestina_pico")

    expr <- str2lang("ernestina_pico + ernestina_volume")
    vars <- extract_variables_from_expr(expr)
    expect_equal(vars, c("ernestina_pico", "ernestina_volume"))

    expr <- str2lang("log(ernestina_pico * barra_grande_pico)")
    vars <- extract_variables_from_expr(expr)
    expect_equal(vars, c("ernestina_pico", "barra_grande_pico"))

    expr <- str2lang("sqrt(ernestina_pico^2 + ernestina_volume^2)")
    vars <- extract_variables_from_expr(expr)
    expect_equal(vars, c("ernestina_pico", "ernestina_volume"))

    expr <- str2lang("(ernestina_pico + ernestina_volume) / barra_grande_volume")
    vars <- extract_variables_from_expr(expr)
    expect_equal(vars, c("ernestina_pico", "ernestina_volume", "barra_grande_volume"))
})

test_that("parse_unitary_event univariate events", {
    modelo <- fit_modelo_cheia(minicheias)

    evt <- parse_unitary_event("barra_grande_duracao <= 10", modelo)
    expect_true(inherits(evt, "unitary_event_u"))
    bounds_x <- attr(evt, "bounds_x")
    expect_equal(bounds_x, c(NA, 10))

    evt <- parse_unitary_event("ernestina_pico >= 500", modelo)
    expect_true(inherits(evt, "unitary_event_u"))
    bounds_x <- attr(evt, "bounds_x")
    expect_equal(bounds_x, c(500, NA))

    evt <- parse_unitary_event("100 <= barra_grande_volume <= 800", modelo)
    expect_true(inherits(evt, "unitary_event_u"))
    bounds_x <- attr(evt, "bounds_x")
    expect_equal(bounds_x, c(100, 800))
})

test_that("parse_unitary_event multivariate events", {
    modelo <- fit_modelo_cheia(minicheias)

    expect_error(parse_unitary_event("ernestina_pico + ernestina_volume >= 1000", modelo),
        "Eventos unitarios multivariados ainda nao sao suportados")

    expect_error(parse_unitary_event("log(ernestina_pico * ernestina_volume) <= 5", modelo),
        "Eventos unitarios multivariados ainda nao sao suportados")

    expect_error(parse_unitary_event("ernestina_pico / barra_grande_pico >= 2", modelo),
        "Eventos unitarios multivariados ainda nao sao suportados")
})

test_that("parse_unitary_event error handling", {
    modelo <- fit_modelo_cheia(minicheias)

    expect_error(parse_unitary_event("nonexistent_var <= 100", modelo))
    expect_error(parse_unitary_event("invalid + + syntax", modelo))
    expect_error(parse_unitary_event("ernestina_pico + nonexistent_var >= 100", modelo))
})

test_that("event2bounds", {
    modelo <- fit_modelo_cheia(minicheias)
    evt <- parse_unitary_event("barra_grande_duracao <= 10", modelo)

    bounds_x <- event2bounds(evt, "x")
    expect_equal(bounds_x, c(0, 10))

    bounds_u <- event2bounds(evt, "u")
    expected_upper_u <- attr(modelo, "ecdfs")[["barra_grande_duracao"]](10)
    expect_equal(bounds_u, c(0, expected_upper_u))

    evt2 <- parse_unitary_event("barra_grande_duracao >= 5", modelo)
    bounds_x2 <- event2bounds(evt2, "x")
    expect_equal(bounds_x2, c(5, Inf))

    bounds_u2 <- event2bounds(evt2, "u")
    expected_lower_u2 <- attr(modelo, "ecdfs")[["barra_grande_duracao"]](5)
    expect_equal(bounds_u2, c(expected_lower_u2, 1))
})

test_that("NA bounds handling in unitary_event_u objects", {
    modelo <- fit_modelo_cheia(minicheias)

    evt_upper <- parse_unitary_event("barra_grande_duracao <= 10", modelo)
    raw_bounds_upper <- attr(evt_upper, "bounds_x")
    expect_true(is.na(raw_bounds_upper[1]))
    expect_equal(raw_bounds_upper[2], 10)

    evt_lower <- parse_unitary_event("barra_grande_duracao >= 5", modelo)
    raw_bounds_lower <- attr(evt_lower, "bounds_x")
    expect_equal(raw_bounds_lower[1], 5)
    expect_true(is.na(raw_bounds_lower[2]))

    evt_double <- parse_unitary_event("5 <= barra_grande_duracao <= 10", modelo)
    raw_bounds_double <- attr(evt_double, "bounds_x")
    expect_equal(raw_bounds_double[1], 5)
    expect_equal(raw_bounds_double[2], 10)
    expect_false(any(is.na(raw_bounds_double)))
})