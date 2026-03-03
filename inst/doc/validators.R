## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# validator <- v_numeric_scalar()
# validator(42)           # ✓ OK
# validator(c(1, 2))      # ✗ Error: must be a single numeric value
# validator("text")       # ✗ Error: must be a single numeric value

## ----eval=FALSE---------------------------------------------------------------
# validator <- v_logical_scalar()
# validator(TRUE)         # ✓ OK
# validator(c(TRUE, FALSE))  # ✗ Error: must be a single logical value
# validator(1)            # ✗ Error: must be a single logical value

## ----eval=FALSE---------------------------------------------------------------
# validator <- v_character_scalar()
# validator("hello")      # ✓ OK
# validator("")           # ✗ Error: must not be empty

## ----eval=FALSE---------------------------------------------------------------
# # Values between 0 and 1 (e.g., probability)
# validator <- v_numeric_range(min = 0, max = 1)
# validator(0.5)          # ✓ OK
# validator(-0.1)         # ✗ Error: must be between 0 and 1
# 
# # Temperature range (Celsius)
# validator <- v_numeric_range(min = -40, max = 50)
# validator(-10)          # ✓ OK

## ----eval=FALSE---------------------------------------------------------------
# # Any numeric vector
# validator <- v_numeric_vector()
# validator(1)            # ✓ OK
# validator(c(1, 2, 3))   # ✓ OK
# 
# # Vector of at least 3 elements
# validator <- v_numeric_vector(min_len = 3)
# validator(c(1, 2, 3, 4)) # ✓ OK
# validator(c(1, 2))      # ✗ Error: length must be >= 3
# 
# # Finite values only (no Inf, -Inf, NaN)
# validator <- v_numeric_vector(finite = TRUE)
# validator(c(1, 2, 3))   # ✓ OK
# validator(c(1, Inf, 3)) # ✗ Error: must be finite
# 
# # Invalid validator arguments
# v_numeric_vector(min_len = 1.5) # ✗ Error: min_len must be a single non-negative whole number
# v_numeric_vector(finite = NA)   # ✗ Error: finite must be a single logical value

## ----eval=FALSE---------------------------------------------------------------
# validator <- v_enum(c("train", "test", "validation"))
# validator("train")      # ✓ OK
# validator("predict")    # ✗ Error: must be one of: train, test, validation
# 
# # Backend choices
# validator <- v_enum(c("cpu", "gpu", "tpu"))
# validator("gpu")        # ✓ OK

## ----eval=FALSE---------------------------------------------------------------
# validator <- v_xypair()
# validator(list(x = c(1, 2, 3), y = c(10, 20, 30)))  # ✓ OK
# 
# validator <- v_xypair(min_len = 2)
# validator(list(x = c(1), y = c(10)))  # ✗ Error: xypair length must be >= 2
# 
# validator <- v_xypair(min_len = 2, max_len = 3)
# validator(list(x = c(1, 2, 3), y = c(10, 20, 30))) # ✓ OK
# validator(list(x = c(1, 2, 3, 4), y = c(10, 20, 30, 40))) # ✗ Error: xypair length must be <= 3
# 
# # Invalid validator arguments
# v_xypair(min_len = 4, max_len = 2) # ✗ Error: max_len must be greater than or equal to min_len
# 
# validator(list(x = c(1, 2), y = c(10, 20, 30)))  # ✗ Error: different lengths
# validator(list(x = c(1, 2)))  # ✗ Error: missing y

## ----eval=FALSE---------------------------------------------------------------
# library(optree)
# 
# # Create a machine learning config manager
# ml_config <- create_options_manager(
#   defaults = list(
#     model_name = "my_model",
#     learning_rate = 0.01,
#     batch_size = 32,
#     backend = "cpu",
#     epochs = 10,
#     coefficients = c(0.1, 0.2, 0.3)
#   ),
#   validators = list(
#     "model_name" = v_character_scalar(),
#     "learning_rate" = v_numeric_range(min = 0, max = 1),
#     "batch_size" = v_numeric_scalar(),
#     "backend" = v_enum(c("cpu", "gpu", "tpu")),
#     "epochs" = v_numeric_range(min = 1, max = 1000),
#     "coefficients" = v_numeric_vector(min_len = 2)
#   )
# )
# 
# # Valid updates work
# ml_config$set(learning_rate = 0.05)  # ✓ OK
# ml_config$set(backend = "gpu")       # ✓ OK
# 
# # Invalid updates fail
# ml_config$set(learning_rate = 1.5)   # ✗ Error: must be between 0 and 1
# ml_config$set(backend = "quantum")   # ✗ Error: must be one of: cpu, gpu, tpu

## ----eval=FALSE---------------------------------------------------------------
# crop_model <- create_options_manager(
#   defaults = list(
#     phenology = list(
#       thermaltime = list(
#         x = c(2, 30, 35),
#         y = c(0, 28, 0)
#       ),
#       photoperiod = list(
#         x = c(1, 2, 3),
#         y = c(0, 1, 0)
#       )
#     ),
#     soil = list(
#       ph = 7,
#       moisture = 0.5
#     )
#   ),
#   validators = list(
#     # Validate paired data structures
#     "phenology.thermaltime" = v_xypair(min_len = 3),
#     "phenology.photoperiod" = v_xypair(min_len = 3),
# 
#     # Validate scalar values
#     "soil.ph" = v_numeric_range(min = 4, max = 9),
#     "soil.moisture" = v_numeric_range(min = 0, max = 1)
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# # Custom validator: must be a positive multiple of 5
# positive_multiple_of_5 <- function(x) {
#   if (!is.numeric(x) || length(x) != 1) {
#     stop("must be a single numeric value", call. = FALSE)
#   }
#   if (x <= 0) {
#     stop("must be positive", call. = FALSE)
#   }
#   if (x %% 5 != 0) {
#     stop("must be a multiple of 5", call. = FALSE)
#   }
# }
# 
# my_config <- create_options_manager(
#   defaults = list(
#     step_size = 5
#   ),
#   validators = list(
#     "step_size" = positive_multiple_of_5
#   )
# )
# 
# my_config$set(step_size = 10)  # ✓ OK
# my_config$set(step_size = 7)   # ✗ Error: must be a multiple of 5

## ----eval=FALSE---------------------------------------------------------------
# # Validator factory: numeric value that must be a multiple of a given number
# v_multiple_of <- function(divisor) {
#   function(x) {
#     # First check it's a valid numeric scalar
#     v_numeric_scalar()(x)
# 
#     # Then check it's a multiple
#     if (x %% divisor != 0) {
#       stop(sprintf("must be a multiple of %d", divisor), call. = FALSE)
#     }
#   }
# }
# 
# validator <- v_multiple_of(10)
# validator(50)   # ✓ OK
# validator(45)   # ✗ Error: must be a multiple of 10

## ----eval=FALSE---------------------------------------------------------------
# config <- create_options_manager(
#   defaults = list(
#     x = 1,
#     y = 2
#   ),
#   validators = list(
#     "x" = v_numeric_range(min = 0, max = 10)
#   )
# )
# 
# # Valid update works
# config$set(x = 5)
# config$get("x")  # Returns 5
# 
# # Invalid update fails and rolls back
# tryCatch(
#   config$set(x = 15),  # Out of range
#   error = function(e) cat("Error caught:", e$message, "\n")
# )
# 
# config$get("x")  # Still returns 5 (rolled back)

