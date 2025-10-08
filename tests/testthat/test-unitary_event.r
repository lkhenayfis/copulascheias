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
    expect_true(is.na(lower))
    expect_equal(upper, 1)

    str <- "var >= 0"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_true(is.na(upper))

    str <- "10 <= var"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 10)
    expect_true(is.na(upper))

    str <- "0 <= var <= 20"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

    str <- "var <= 1.2"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_true(is.na(lower))
    expect_equal(upper, 1.2)

    str <- "var >= 0.5"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.5)
    expect_true(is.na(upper))

    str <- "10.98 <= var"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 10.98)
    expect_true(is.na(upper))

    str <- "0.54 <= var <= 20.546"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.54)
    expect_equal(upper, 20.546)

    str <- "100 <= ernestina_pico <= 100"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 100)
    expect_equal(upper, 100)

    str <- "0.12345 <= var <= 999.98765"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0.12345)
    expect_equal(upper, 999.98765)

    str <- "  0  <=  var  <=  20  "
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

    str <- "0<=var<=20"
    lower <- get_lower(str)
    upper <- get_upper(str)
    expect_equal(lower, 0)
    expect_equal(upper, 20)

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
    raw_bounds <- attr(evt, "bounds_x")
    expect_true(is.na(raw_bounds[1]) || raw_bounds[1] == 0)
    expect_equal(raw_bounds[2], 10)

    bounds_u <- sapply(attr(evt, "bounds_x"),
        function(x) if (is.na(x)) NA else attr(modelo, "ecdfs")[["barra_grande_duracao"]](x))
    expect_equal(attr(evt, "bounds_u"), bounds_u)
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