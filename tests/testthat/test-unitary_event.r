test_that("get_event_vars", {
    str <- "var <= 1"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "var >= 1"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "0 <= var"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "0 <= var <= 1"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "var <= 1.2"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "var >= 1.2"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "0.8 <= var"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")

    str <- "0.8 <= var <= 1.2"
    evt_var <- get_event_vars(str)
    expect_equal(evt_var, "var")
})

test_that("get_lower|upper", {
    str <- "var <= 1"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 1)

    str <- "var >= 0"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, Inf)

    str <- "10 <= var"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 10)
    expect_equal(upper, Inf)

    str <- "0 <= var <= 20"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

    str <- "var <= 1.2"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 1.2)

    str <- "var >= 0.5"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.5)
    expect_equal(upper, Inf)

    str <- "10.98 <= var"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 10.98)
    expect_equal(upper, Inf)

    str <- "0.54 <= var <= 20.546"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.54)
    expect_equal(upper, 20.546)
})

test_that("validate_vars", {
    modelo <- fit_modelo_cheia(minicheias)

    expect_no_error(validate_vars("barra_grande_pico", modelo))
    expect_no_error(validate_vars("barra_grande_volume", modelo))

    expect_error(validate_vars("barra_grande_erro", modelo))
})

test_that("parse_unitary_event", {
    modelo <- fit_modelo_cheia(minicheias)
    evt <- parse_unitary_event("barra_grande_duracao <= 10", modelo)

    expect_true(inherits(evt, "unitary_event_u"))
    expect_equal(attr(evt, "bounds_x"), c(0, 10))

    bounds_u <- sapply(attr(evt, "bounds_x"),
        function(x) attr(modelo, "ecdfs")[["barra_grande_duracao"]](x))
    expect_equal(attr(evt, "bounds_u"), bounds_u)
})

test_that("event2bounds", {
    modelo <- fit_modelo_cheia(minicheias)
    evt <- parse_unitary_event("barra_grande_duracao <= 10", modelo)

    expect_equal(event2bounds(evt), attr(evt, "bounds_x"))
    expect_equal(event2bounds(evt, "x"), attr(evt, "bounds_x"))
    expect_equal(event2bounds(evt, "u"), attr(evt, "bounds_u"))
})