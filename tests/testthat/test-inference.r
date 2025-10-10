test_that("parse_inference", {
    modelo <- fit_modelo_cheia(minicheias)
    expr <- "(ernestina_volume >= 400) & (ernestina_pico >= 100)"

    inference <- parse_inference(expr, modelo)
    expect_true(inherits(inference, "simple_inference"))

    outer <- list(
        parse_unitary_event("ernestina_volume >= 400", modelo),
        parse_unitary_event("ernestina_pico >= 100", modelo)
    )
    expect_equal(unclass(inference), outer)

    expr <- "(ernestina_volume >= 400) | (ernestina_pico >= 100)"
    expect_error(parse_inference(expr, modelo))
})

test_that("inference2bounds", {
    modelo <- fit_modelo_cheia(minicheias)
    inference <- parse_inference("(ernestina_volume >= 200) & (ernestina_pico >= 100)", modelo)

    bounds <- inference2bounds(inference, modelo)
    expect_equal(
        round(bounds[[1]], 7),
        structure(c(0, 0, 0, 0.1204819, 0, 0.9397590), names = modelo$vines$names)
    )
    expect_equal(
        round(bounds[[2]], 7),
        structure(rep(1, 6), names = modelo$vines$names)
    )
})