# Test options manager functionality
canola <- create_options_manager(
    defaults = list(
        thermaltime = list(
            x = c(2, 30, 35),
            y = c(0, 28, 0)
        ),
        frost_threshold = 0
    ),
    validators = list(
        "thermaltime" = v_xypair(),
        "frost_threshold" = v_numeric_scalar()
    )
)


test_that("get errors on unknown option path", {
    expect_error(
        canola$get("phenology.unknown"),
        "Option 'phenology.unknown' is not defined"
    )
    expect_error(
        canola$get("unknown-parameter"),
        "Option 'unknown-parameter' is not defined"
    )
})

test_that("get returns correct default values", {
    expect_equal(canola$get("thermaltime.x"), c(2, 30, 35))
    expect_equal(canola$get("thermaltime.y"), c(0, 28, 0))
    expect_equal(canola$get("frost_threshold"), 0)
})

test_that("set updates values correctly", {
    canola$set(thermaltime = list(x = c(5, 25, 40), y = c(0, 20, 0)))
    expect_equal(canola$get("thermaltime.x"), c(5, 25, 40))
    expect_equal(canola$get("thermaltime.y"), c(0, 20, 0))

    canola$set(frost_threshold = -2)
    expect_equal(canola$get("frost_threshold"), -2)
})

test_that("set enforces group validation", {
    # Missing y
    expect_no_error(
        canola$set(thermaltime = list(x = c(1, 2, 3)))
    )

    # x and y different length
    expect_error(
        canola$set(thermaltime = list(x = c(1, 2), y = c(0, 1, 2)))
    )
})

test_that("reset restores defaults", {
    canola$reset()
    expect_equal(canola$get("thermaltime.x"), c(2, 30, 35))
    expect_equal(canola$get("thermaltime.y"), c(0, 28, 0))
    expect_equal(canola$get("frost_threshold"), 0)
})

test_that("cannot set unknown top-level options", {
    expect_error(
        canola$set(nonexistent_option = 123),
        "Option 'nonexistent_option' is not defined"
    )
})

test_that("cannot set unknown nested options", {
    expect_error(
        canola$set(thermaltime = list(
            x = c(5, 25, 40),
            y = c(0, 20, 0),
            z = 999 # not defined in defaults
        )),
        "Unknown sub-option\\(s\\) for 'thermaltime': z"
    )
})


# Test more complex nested structure
# Create a CANOLA options manager
canola <- create_options_manager(
    defaults = list(
        phenology = list(
            thermaltime = list(
                x = c(2, 30, 35),
                y = c(0, 28, 0)
            )
        ),
        frost_threshold = 0
    ),
    validators = list(
        "phenology.thermaltime" = v_xypair(),
        "frost_threshold" = v_numeric_scalar()
    )
)

test_that("cannot set unknown top-level options", {
    expect_error(
        canola$set(nonexistent_option = 123),
        "Option 'nonexistent_option' is not defined"
    )
})

test_that("cannot set unknown nested options", {
    expect_error(
        canola$set(phenology = list(
            thermaltime = list(
                x = c(5, 25, 40),
                y = c(0, 20, 0),
                z = 999 # not defined in defaults
            )
        )),
        "Unknown sub-option\\(s\\) for 'phenology.thermaltime': z"
    )
})

test_that("can set only predefined nested options", {
    canola$set(phenology = list(
        thermaltime = list(
            x = c(5, 25, 40),
            y = c(0, 20, 0)
        )
    ))
    expect_equal(canola$get("phenology.thermaltime.x"), c(5, 25, 40))
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 20, 0))
})

test_that("partial nested updates respect predefined keys", {
    # only update x
    canola$set(phenology = list(
        thermaltime = list(
            x = c(1, 2, 3)
        )
    ))
    expect_equal(canola$get("phenology.thermaltime.x"), c(1, 2, 3))
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 20, 0)) # unchanged
})

# Test dot-separated path notation in set()
test_that("set accepts dot-separated paths for nested options", {
    canola$reset()
    
    # Set a deeply nested value using dot notation
    canola$set("phenology.thermaltime.x" = c(10, 20, 30))
    expect_equal(canola$get("phenology.thermaltime.x"), c(10, 20, 30))
    # y should be unchanged
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 28, 0))
    
    # Set another nested value
    canola$set("phenology.thermaltime.y" = c(1, 2, 3))
    expect_equal(canola$get("phenology.thermaltime.y"), c(1, 2, 3))
})

test_that("set can mix dot-separated paths and nested lists", {
    canola$reset()
    
    # Mix both styles in one call
    canola$set(
        "phenology.thermaltime.x" = c(5, 15, 25),
        frost_threshold = -3
    )
    
    expect_equal(canola$get("phenology.thermaltime.x"), c(5, 15, 25))
    expect_equal(canola$get("frost_threshold"), -3)
})

test_that("dot-separated paths enforce validation", {
    canola$reset()
    
    # First set x to different length
    
    expect_error(
        canola$set("phenology.thermaltime.x" = c(1, 2))
    )
    expect_equal(canola$get("phenology.thermaltime.x"), c(2, 30, 35)) # should be unchanged
    # Now y has different length, should fail validation
    expect_error(
        canola$set("phenology.thermaltime.y" = c(0, 28))
    )
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 28, 0)) # should be unchanged
})

test_that("dot-separated paths reject unknown options", {
    expect_error(
        canola$set("phenology.thermaltime.z" = 999),
        "Unknown sub-option\\(s\\) for 'phenology.thermaltime': z"
    )
    
    expect_error(
        canola$set("phenology.unknown_group.x" = 1),
        "Unknown sub-option\\(s\\) for 'phenology': unknown_group"
    )
    
    expect_error(
        canola$set("nonexistent.path" = 1),
        "Option 'nonexistent' is not defined"
    )
})

test_that("set rolls back on validation failure", {
    canola$reset()
    
    # Set valid values first
    canola$set("phenology.thermaltime.x" = c(5, 15, 25))
    canola$set("phenology.thermaltime.y" = c(0, 10, 0))
    
    original_x <- canola$get("phenology.thermaltime.x")
    original_y <- canola$get("phenology.thermaltime.y")
    
    # Try to set x with mismatched length - should fail and rollback
    expect_error(
        canola$set("phenology.thermaltime.x" = c(1, 2))
    )
    
    # Verify state is unchanged after failed validation
    expect_equal(canola$get("phenology.thermaltime.x"), original_x)
    expect_equal(canola$get("phenology.thermaltime.y"), original_y)
    
    # Try another invalid update with traditional nested list syntax
    expect_error(
        canola$set(phenology = list(thermaltime = list(x = c(1), y = c(1, 2, 3))))
    )
    
    # State should still be unchanged
    expect_equal(canola$get("phenology.thermaltime.x"), original_x)
    expect_equal(canola$get("phenology.thermaltime.y"), original_y)
})





# Check the invalid cases of parameter defined

test_that("create_options_manager rejects empty top-level names", {
    expect_error(
        create_options_manager(list()),
        "`defaults` must be a named list"
    )
})

test_that("create_options_manager rejects empty nested names", {
    expect_error(
        create_options_manager(list(
            phenology = list(123)
        )),
        "All lists must be named at path 'phenology'"
    )
})

test_that("create_options_manager rejects names containing dots", {
    expect_error(
        create_options_manager(list(
            "thermaltime.x" = 123
        )),
        "Option names must not contain '.'"
    )

    expect_error(
        create_options_manager(list(
            phenology = list("thermaltime.x" = 123)
        )),
        "Option names must not contain '.'"
    )
})

test_that("create_options_manager rejects validators that are not functions", {
    expect_error(
        create_options_manager(
            defaults = list(a = 1),
            validators = list(a = "not_a_function")
        ),
        "Validators must be functions"
    )
})

test_that("create_options_manager rejects validators with non-existent paths", {
    expect_error(
        create_options_manager(
            defaults = list(a = 1),
            validators = list("b" = function(x) x)
        ),
        "Validator path 'b' does not exist"
    )

    expect_error(
        create_options_manager(
            defaults = list(a = list(x = 1)),
            validators = list("a.y" = function(x) x)
        ),
        "Validator path 'a.y' does not exist"
    )
})
