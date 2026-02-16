# Test validator functions

test_that("v_numeric_scalar accepts single numeric values", {
    validator <- v_numeric_scalar()
    expect_silent(validator(42))
    expect_silent(validator(-3.14))
    expect_silent(validator(0))
    expect_silent(validator(Inf))
})

test_that("v_numeric_scalar rejects numeric vectors", {
    validator <- v_numeric_scalar()
    expect_error(
        validator(c(1, 2, 3)),
        "must be a single numeric value"
    )
    expect_error(
        validator(c(1.5, 2.5)),
        "must be a single numeric value"
    )
})

test_that("v_numeric_scalar rejects non-numeric values", {
    validator <- v_numeric_scalar()
    expect_error(
        validator("text"),
        "must be a single numeric value"
    )
    expect_error(
        validator(TRUE),
        "must be a single numeric value"
    )
    expect_error(
        validator(list(1)),
        "must be a single numeric value"
    )
})

test_that("v_numeric_scalar rejects empty vectors", {
    validator <- v_numeric_scalar()
    expect_error(
        validator(numeric(0)),
        "must be a single numeric value"
    )
})

test_that("v_logical_scalar accepts single logical values", {
    validator <- v_logical_scalar()
    expect_silent(validator(TRUE))
    expect_silent(validator(FALSE))
})

test_that("v_logical_scalar rejects logical vectors", {
    validator <- v_logical_scalar()
    expect_error(
        validator(c(TRUE, FALSE)),
        "must be a single logical value"
    )
    expect_error(
        validator(c(TRUE, TRUE)),
        "must be a single logical value"
    )
})

test_that("v_logical_scalar rejects non-logical values", {
    validator <- v_logical_scalar()
    expect_error(
        validator("TRUE"),
        "must be a single logical value"
    )
    expect_error(
        validator(1),
        "must be a single logical value"
    )
    expect_error(
        validator(list(TRUE)),
        "must be a single logical value"
    )
})

test_that("v_logical_scalar rejects empty vectors", {
    validator <- v_logical_scalar()
    expect_error(
        validator(logical(0)),
        "must be a single logical value"
    )
})

test_that("validators can be used with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            threshold = 0.5,
            enabled = FALSE
        ),
        validators = list(
            "threshold" = v_numeric_scalar(),
            "enabled" = v_logical_scalar()
        )
    )
    
    # Valid values should work
    expect_silent(manager$set(threshold = 1.5))
    expect_silent(manager$set(enabled = TRUE))
    
    # Invalid values should fail
    expect_error(manager$set(threshold = c(1, 2)), "must be a single numeric value")
    expect_error(manager$set(enabled = c(TRUE, FALSE)), "must be a single logical value")
})

test_that("v_xypair accepts valid xy pair lists", {
    validator <- v_xypair()
    expect_silent(validator(list(x = c(2, 30), y = c(0, 28))))
    expect_silent(validator(list(x = c(2, 30, 35), y = c(0, 28, 0))))
})

test_that("v_xypair rejects lists without x and y", {
    validator <- v_xypair()
    expect_error(
        validator(list(x = c(2, 30))),
        "must contain both 'x' and 'y'"
    )
    expect_error(
        validator(list(y = c(0, 28))),
        "must contain both 'x' and 'y'"
    )
    expect_error(
        validator(list(a = 1, b = 2)),
        "must contain both 'x' and 'y'"
    )
})

test_that("v_xypair enforces minimum length", {
    validator <- v_xypair(min_len = 3)
    
    # Too short
    expect_error(
        validator(list(x = c(2, 30), y = c(0, 28))),
        "xypair length must be >= 3"
    )
    
    # Valid length
    expect_silent(validator(list(x = c(2, 30, 35), y = c(0, 28, 0))))
})

test_that("v_xypair requires matching x and y lengths", {
    validator <- v_xypair()
    expect_error(
        validator(list(x = c(2, 30, 35), y = c(0, 28))),
        "'x' and 'y' must have the same length"
    )
    expect_error(
        validator(list(x = c(2), y = c(0, 28, 0))),
        "'x' and 'y' must have the same length"
    )
})

test_that("v_xypair rejects non-list values", {
    validator <- v_xypair()
    expect_error(
        validator(c(2, 30)),
        "xypair must be a list"
    )
    expect_error(
        validator("not a list"),
        "xypair must be a list"
    )
})

test_that("v_xypair rejects NA values", {
    validator <- v_xypair()
    expect_error(
        validator(list(x = c(2, NA), y = c(0, 28))),
        "'x' and 'y' must not contain NA values"
    )
    expect_error(
        validator(list(x = c(2, 30), y = c(0, NA))),
        "'x' and 'y' must not contain NA values"
    )
})

test_that("v_xypair rejects non-atomic vectors", {
    validator <- v_xypair()
    expect_error(
        validator(list(x = list(1, 2), y = c(0, 28))),
        "'x' and 'y' must be atomic vectors"
    )
    expect_error(
        validator(list(x = c(2, 30), y = data.frame(a = 1:2))),
        "'x' and 'y' must be atomic vectors"
    )
})

test_that("v_xypair can be used with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            thermaltime = list(x = c(2, 30, 35), y = c(0, 28, 0))
        ),
        validators = list(
            "thermaltime" = v_xypair(min_len = 2)
        )
    )
    
    # Valid update should work
    expect_silent(manager$set(thermaltime = list(x = c(5, 25), y = c(0, 20))))
    
    # Invalid update should fail
    expect_error(
        manager$set(thermaltime = list(x = c(5), y = c(0, 20))),
        "'x' and 'y' must have the same length"
    )
})

test_that("v_numeric_range accepts values within bounds", {
    validator <- v_numeric_range(min = 0, max = 1)
    expect_silent(validator(0))
    expect_silent(validator(0.5))
    expect_silent(validator(1))
})

test_that("v_numeric_range rejects values below minimum", {
    validator <- v_numeric_range(min = 0, max = 1)
    expect_error(
        validator(-0.1),
        "must be between 0 and 1"
    )
    expect_error(
        validator(-100),
        "must be between 0 and 1"
    )
})

test_that("v_numeric_range rejects values above maximum", {
    validator <- v_numeric_range(min = 0, max = 1)
    expect_error(
        validator(1.1),
        "must be between 0 and 1"
    )
    expect_error(
        validator(100),
        "must be between 0 and 1"
    )
})

test_that("v_numeric_range rejects non-numeric values", {
    validator <- v_numeric_range(min = 0, max = 1)
    expect_error(
        validator("0.5"),
        "must be a single numeric value"
    )
    expect_error(
        validator(TRUE),
        "must be a single numeric value"
    )
})

test_that("v_numeric_range rejects vectors", {
    validator <- v_numeric_range(min = 0, max = 1)
    expect_error(
        validator(c(0.5, 0.7)),
        "must be a single numeric value"
    )
})

test_that("v_numeric_range works with one-sided bounds", {
    # Only minimum
    validator_min <- v_numeric_range(min = 0)
    expect_silent(validator_min(0))
    expect_silent(validator_min(1000))
    expect_error(validator_min(-1), "must be between 0 and Inf")
    
    # Only maximum
    validator_max <- v_numeric_range(max = 100)
    expect_silent(validator_max(50))
    expect_silent(validator_max(100))
    expect_error(validator_max(101), "must be between -Inf and 100")
})

test_that("v_numeric_range works with no bounds", {
    validator <- v_numeric_range()
    expect_silent(validator(-1000))
    expect_silent(validator(0))
    expect_silent(validator(1000))
})

test_that("v_numeric_range works with negative ranges", {
    validator <- v_numeric_range(min = -10, max = -5)
    expect_silent(validator(-7))
    expect_error(validator(-4), "must be between -10 and -5")
    expect_error(validator(0), "must be between -10 and -5")
})

test_that("v_numeric_range can be used with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            learning_rate = 0.01,
            dropout = 0.5
        ),
        validators = list(
            "learning_rate" = v_numeric_range(min = 0, max = 1),
            "dropout" = v_numeric_range(min = 0, max = 1)
        )
    )
    
    # Valid updates should work
    expect_silent(manager$set(learning_rate = 0.001))
    expect_silent(manager$set(dropout = 0.3))
    
    # Invalid updates should fail
    expect_error(
        manager$set(learning_rate = 1.5),
        "must be between 0 and 1"
    )
    expect_error(
        manager$set(dropout = -0.1),
        "must be between 0 and 1"
    )
})

test_that("v_character_scalar accepts single character values", {
    validator <- v_character_scalar()
    expect_silent(validator("hello"))
    expect_silent(validator("a"))
    expect_silent(validator("multi word string"))
})

test_that("v_character_scalar rejects empty strings by default", {
    validator <- v_character_scalar()
    expect_error(
        validator(""),
        "must not be empty"
    )
})

test_that("v_character_scalar allows empty strings when non_empty = FALSE", {
    validator <- v_character_scalar()
    expect_error(validator(""))
    expect_silent(validator("hello"))
})

test_that("v_character_scalar rejects character vectors", {
    validator <- v_character_scalar()
    expect_error(
        validator(c("hello", "world")),
        "must be a single character value"
    )
})

test_that("v_character_scalar rejects non-character values", {
    validator <- v_character_scalar()
    expect_error(
        validator(123),
        "must be a single character value"
    )
    expect_error(
        validator(TRUE),
        "must be a single character value"
    )
    expect_error(
        validator(list("hello")),
        "must be a single character value"
    )
})

test_that("v_enum accepts values in choices", {
    validator <- v_enum(c("red", "green", "blue"))
    expect_silent(validator("red"))
    expect_silent(validator("green"))
    expect_silent(validator("blue"))
})

test_that("v_enum rejects values not in choices", {
    validator <- v_enum(c("red", "green", "blue"))
    expect_error(
        validator("yellow"),
        "must be one of: red, green, blue"
    )
    expect_error(
        validator("RED"),
        "must be one of: red, green, blue"
    )
})

test_that("v_enum rejects character vectors", {
    validator <- v_enum(c("red", "green", "blue"))
    expect_error(
        validator(c("red", "blue")),
        "must be a single character value"
    )
})

test_that("v_enum rejects non-character values", {
    validator <- v_enum(c("red", "green", "blue"))
    expect_error(
        validator(1),
        "must be a single character value"
    )
    expect_error(
        validator(TRUE),
        "must be a single character value"
    )
})

test_that("v_numeric_vector accepts numeric vectors", {
    validator <- v_numeric_vector()
    expect_silent(validator(1))
    expect_silent(validator(c(1, 2, 3)))
    expect_silent(validator(c(0.5, 1.5, 2.5)))
})

test_that("v_numeric_vector enforces minimum length", {
    validator <- v_numeric_vector(min_len = 3)
    expect_silent(validator(c(1, 2, 3)))
    expect_silent(validator(c(1, 2, 3, 4)))
    expect_error(
        validator(c(1, 2)),
        "length must be >= 3"
    )
    expect_error(
        validator(1),
        "length must be >= 3"
    )
})

test_that("v_numeric_vector rejects non-numeric values", {
    validator <- v_numeric_vector()
    expect_error(
        validator("not numeric"),
        "must be numeric"
    )
    expect_error(
        validator(c("1", "2", "3")),
        "must be numeric"
    )
})

test_that("v_numeric_vector rejects NA values", {
    validator <- v_numeric_vector()
    expect_error(
        validator(c(1, 2, NA))
    )
    expect_error(
        validator(NA)
    )
})

test_that("v_numeric_vector enforces finiteness by default", {
    validator <- v_numeric_vector()
    expect_error(
        validator(Inf),
        "must be finite"
    )
    expect_error(
        validator(c(1, 2, Inf)),
        "must be finite"
    )
    expect_error(
        validator(-Inf),
        "must be finite"
    )
})

test_that("v_numeric_vector allows non-finite when finite = FALSE", {
    validator <- v_numeric_vector(finite = FALSE)
    expect_silent(validator(Inf))
    expect_silent(validator(-Inf))
    expect_silent(validator(c(1, 2, Inf)))
})

test_that("v_numeric_vector still rejects NA when finite = FALSE", {
    validator <- v_numeric_vector(finite = FALSE)
    expect_error(
        validator(c(1, NA, Inf)),
        "must not contain NA"
    )
})

test_that("v_character_scalar works with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            model_name = "default_model",
            description = "A model"
        ),
        validators = list(
            "model_name" = v_character_scalar(),
            "description" = v_character_scalar()
        )
    )
    
    expect_silent(manager$set(model_name = "new_model"))
    expect_error(manager$set(description = ""))
    expect_error(
        manager$set(model_name = ""),
        "must not be empty"
    )
})

test_that("v_enum works with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            backend = "cpu"
        ),
        validators = list(
            "backend" = v_enum(c("cpu", "gpu", "tpu"))
        )
    )
    
    expect_silent(manager$set(backend = "gpu"))
    expect_silent(manager$set(backend = "tpu"))
    expect_error(
        manager$set(backend = "quantum"),
        "must be one of: cpu, gpu, tpu"
    )
})

test_that("v_numeric_vector works with create_options_manager", {
    manager <- create_options_manager(
        defaults = list(
            coefficients = c(0.1, 0.2, 0.3)
        ),
        validators = list(
            "coefficients" = v_numeric_vector(min_len = 2, finite = TRUE)
        )
    )
    
    expect_silent(manager$set(coefficients = c(0.5, 0.5)))
    expect_silent(manager$set(coefficients = c(0.1, 0.2, 0.3, 0.4)))
    expect_error(
        manager$set(coefficients = c(0.5)),
        "length must be >= 2"
    )
    expect_error(
        manager$set(coefficients = c(0.5, Inf)),
        "must be finite"
    )
})




