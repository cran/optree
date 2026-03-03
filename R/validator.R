#' Validator for Numeric Scalar Values
#'
#' `v_numeric_scalar()` returns a validator function that checks if a value
#' is a single numeric value. This is useful as a validator function for
#' options managers created with [create_options_manager()].
#'
#' @return A validator function that takes a value `x` and raises an error
#'   if `x` is not a single numeric value.
#'
#' @export
#'
#' @examples
#' # Create a validator for numeric scalars
#' validator <- v_numeric_scalar()
#'
#' # Valid input
#' validator(42)
#'
#' # Invalid inputs (would raise errors)
#' try(validator(c(1, 2, 3))) # vector, not scalar
#' try(validator("text")) # not numeric
v_numeric_scalar <- function() {
    function(x) {
        if (!is.numeric(x) || length(x) != 1) {
            stop("must be a single numeric value", call. = FALSE)
        }
    }
}

#' Validator for Logical Scalar Values
#'
#' `v_logical_scalar()` returns a validator function that checks if a value
#' is a single logical value. This is useful as a validator function for
#' options managers created with [create_options_manager()].
#'
#' @return A validator function that takes a value `x` and raises an error
#'   if `x` is not a single logical value.
#'
#' @export
#'
#' @examples
#' # Create a validator for logical scalars
#' validator <- v_logical_scalar()
#'
#' # Valid input
#' validator(TRUE)
#'
#' # Invalid inputs (would raise errors)
#' try(validator(c(TRUE, FALSE))) # vector, not scalar
#' try(validator(1)) # numeric, not logical
v_logical_scalar <- function() {
    function(x) {
        if (!is.logical(x) || length(x) != 1) {
            stop("must be a single logical value", call. = FALSE)
        }
    }
}

#' Validator for Numeric Values Within a Range
#'
#' `v_numeric_range()` returns a validator function that checks if a value
#' is a single numeric value within a specified range. This is useful as a
#' validator function for bounded numeric options in options managers created
#' with [create_options_manager()].
#'
#' @param min Minimum allowed value (inclusive). Defaults to `-Inf` (no lower bound).
#' @param max Maximum allowed value (inclusive). Defaults to `Inf` (no upper bound).
#'
#' @return A validator function that takes a value `x` and raises an error if:
#'   - `x` is not a single numeric value
#'   - `x` is less than `min` or greater than `max`
#'
#' @export
#'
#' @examples
#' # Create a validator for values between 0 and 1
#' validator <- v_numeric_range(min = 0, max = 1)
#'
#' # Valid inputs
#' validator(0.5)
#' validator(0)
#' validator(1)
#'
#' # Invalid inputs (would raise errors)
#' try(validator(-0.1)) # below minimum
#' try(validator(1.5)) # above maximum
#' try(validator(c(0.5, 0.7))) # vector, not scalar
v_numeric_range <- function(min = -Inf, max = Inf) {
    function(value) {
        if (!is.numeric(value) || length(value) != 1) {
            stop("must be a single numeric value", call. = FALSE)
        }
        if (value < min || value > max) {
            stop(sprintf("must be between %s and %s", min, max), call. = FALSE)
        }
    }
}

#' Validator for Character Scalar Values
#'
#' `v_character_scalar()` returns a validator function that checks if a value
#' is a single character value. This is useful as a validator function for
#' options managers created with [create_options_manager()].
#'
#' @return A validator function that takes a value `x` and raises an error if:
#'   - `x` is not a single character value
#'   - `x` is an empty string
#'
#' @export
#'
#' @examples
#' # Create a validator for non-empty character scalars
#' validator <- v_character_scalar()
#'
#' # Valid input
#' validator("hello")
#'
#' # Invalid inputs (would raise errors)
#' try(validator(c("hello", "world")))  # vector, not scalar
#' try(validator(123))  # numeric, not character
v_character_scalar <- function() {
    function(x) {
        if (!is.character(x) || length(x) != 1L) {
            stop("must be a single character value", call. = FALSE)
        }
        if (x == "") {
            stop("must not be empty", call. = FALSE)
        }
    }
}

#' Validator for Enumerated Character Values
#'
#' `v_enum()` returns a validator function that checks if a value is a single
#' character value matching one of a predefined set of choices. This is useful
#' for options that must be one of several allowed values.
#'
#' @param choices A character vector of allowed values.
#'
#' @return A validator function that takes a value `x` and raises an error if:
#'   - `x` is not a single character value
#'   - `x` is not in the predefined `choices`
#'
#' @export
#'
#' @examples
#' # Create a validator for one of several color choices
#' validator <- v_enum(choices = c("red", "green", "blue"))
#'
#' # Valid inputs
#' validator("red")
#' validator("blue")
#'
#' # Invalid inputs (would raise errors)
#' try(validator("yellow"))  # not in choices
#' try(validator(c("red", "blue")))  # vector, not scalar
#' try(validator(1))  # numeric, not character
v_enum <- function(choices) {
    stopifnot(is.character(choices))

    function(x) {
        if (!is.character(x) || length(x) != 1L) {
            stop("must be a single character value", call. = FALSE)
        }
        if (!x %in% choices) {
            stop(
                sprintf("must be one of: %s", paste(choices, collapse = ", ")),
                call. = FALSE
            )
        }
    }
}

#' Validator for Numeric Vectors
#'
#' `v_numeric_vector()` returns a validator function that checks if a value
#' is a numeric vector meeting specified length and finiteness requirements.
#' This is useful for options requiring numeric sequences or datasets.
#'
#' @param min_len Minimum length required for the vector. Defaults to 1.
#' @param finite If TRUE (default), rejects non-finite values (Inf, -Inf, NaN).
#'   If FALSE, non-finite values are allowed.
#'
#' @return A validator function that takes a value `x` and raises an error if:
#'   - `x` is not numeric
#'   - `x` has fewer than `min_len` elements
#'   - `x` contains NA values
#'   - `finite` is TRUE and `x` contains non-finite values
#'
#' @export
#'
#' @examples
#' # Create a validator for numeric vectors of at least length 3
#' validator <- v_numeric_vector(min_len = 3)
#'
#' # Valid input
#' validator(c(1, 2, 3))
#' validator(c(0.5, 1.5, 2.5, 3.5))
#'
#' # Invalid inputs (would raise errors)
#' try(validator(c(1, 2)))  # too short
#' try(validator(c(1, NA, 3)))  # contains NA
#' try(validator(c(1, Inf, 3)))  # contains non-finite value
#' try(validator("not numeric"))  # not numeric
v_numeric_vector <- function(min_len = 1, finite = TRUE) {
    if (!is.numeric(min_len) || length(min_len) != 1L || is.na(min_len) || !is.finite(min_len) || min_len < 0 || min_len %% 1 != 0) {
        stop("min_len must be a single non-negative whole number", call. = FALSE)
    }

    if (!is.logical(finite) || length(finite) != 1L || is.na(finite)) {
        stop("finite must be a single logical value", call. = FALSE)
    }

    function(x) {
        if (!is.numeric(x)) {
            stop("must be numeric", call. = FALSE)
        }
        if (length(x) < min_len) {
            stop(sprintf("length must be >= %d", min_len), call. = FALSE)
        }
        if (anyNA(x)) {
            stop("must not contain NA", call. = FALSE)
        }
        if (finite && any(!is.finite(x))) {
            stop("must be finite", call. = FALSE)
        }
    }
}

#' Validator for XY Pair Lists
#'
#' `v_xypair()` returns a validator function that checks if a value is a list
#' with paired `x` and `y` components of equal length. This is useful for
#' validating paired data structures in options managers created with
#' [create_options_manager()].
#'
#' @param min_len Minimum length required for the `x` and `y` vectors.
#'   Defaults to 1.
#' @param max_len Maximum length allowed for the `x` and `y` vectors.
#'   Defaults to `NULL` (no upper bound).
#'
#' @return A validator function that takes a value (typically a list with `x`
#'   and `y` components) and raises an error if:
#'   - The value is not a list
#'   - The list does not contain both `x` and `y` named elements
#'   - Either `x` or `y` is NULL
#'   - Either `x` or `y` is not an atomic vector
#'   - Either `x` or `y` contains NA values
#'   - The `x` and `y` vectors have different lengths
#'   - The vectors are shorter than `min_len`
#'   - The vectors are longer than `max_len` (when `max_len` is not `NULL`)
#'
#' @export
#'
#' @examples
#' # Create a validator for XY pairs with minimum length 2
#' validator <- v_xypair(min_len = 2)
#'
#' # Valid input
#' validator(list(x = c(1, 2, 3), y = c(10, 20, 30)))
#'
#' # Invalid inputs (would raise errors)
#' try(validator(list(x = c(1), y = c(10))))
#' try(validator(list(x = c(1, 2), y = c(10, 20, 30)))) # different lengths
#' try(validator(list(x = c(1, NA), y = c(10, 20)))) # contains NA
#' try(validator(list(x = c(1, 2)))) # missing y
v_xypair <- function(min_len = 1, max_len = NULL) {
    if (!is.numeric(min_len) || length(min_len) != 1L || is.na(min_len) || !is.finite(min_len) || min_len < 0 || min_len %% 1 != 0) {
        stop("min_len must be a single non-negative whole number", call. = FALSE)
    }

    if (!is.null(max_len) && (!is.numeric(max_len) || length(max_len) != 1L || is.na(max_len) || !is.finite(max_len) || max_len < 0 || max_len %% 1 != 0)) {
        stop("max_len must be NULL or a single non-negative whole number", call. = FALSE)
    }

    if (!is.null(max_len) && max_len < min_len) {
        stop("max_len must be greater than or equal to min_len", call. = FALSE)
    }

    function(value) {
        if (!is.list(value)) {
            stop("xypair must be a list", call. = FALSE)
        }

        if (!all(c("x", "y") %in% names(value))) {
            stop("xypair must contain both 'x' and 'y'", call. = FALSE)
        }

        x <- value$x
        y <- value$y

        if (is.null(x) || is.null(y)) {
            stop("'x' and 'y' must not be NULL", call. = FALSE)
        }

        if (!is.atomic(x) || !is.atomic(y)) {
            stop("'x' and 'y' must be atomic vectors", call. = FALSE)
        }

        if (any(is.na(x)) || any(is.na(y))) {
            stop("'x' and 'y' must not contain NA values", call. = FALSE)
        }

        if (length(x) != length(y)) {
            stop("'x' and 'y' must have the same length", call. = FALSE)
        }

        if (length(x) < min_len) {
            stop(sprintf("xypair length must be >= %d", min_len), call. = FALSE)
        }

        if (!is.null(max_len) && length(x) > max_len) {
            stop(sprintf("xypair length must be <= %d", max_len), call. = FALSE)
        }
    }
}
