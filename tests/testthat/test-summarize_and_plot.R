library(testthat)
library(dplyr)

# Define example data
df <- data.frame(
  group = c("A", "A", "B", "B", "C", "C"),
  value = c(10, 20, 15, 25, 30, NA)
)

test_that("summarize_and_plot returns correct output structure", {
  result <- summarize_and_plot(df, "group", "value")
  
  # Test if the output is a list
  expect_type(result, "list")
  
  # Test if the list contains summary and plot elements
  expect_named(result, c("summary", "plot"))
  
  # Test if the plot is a ggplot object
  expect_s3_class(result$plot, "ggplot")
})

test_that("summarize_and_plot correctly calculates summary statistics", {
  # Test with no NA removal
  result_na <- summarize_and_plot(df, "group", "value", na.rm = FALSE)
  
  # Extract the summary for the "C" group, which should have an NA in its mean calculation
  summary_C <- result_na$summary %>% filter(group == "C")
  
  # Check if mean is NA when na.rm = FALSE
  expect_true(is.na(summary_C$mean_value))
  
  # Test with NA removal
  result_no_na <- summarize_and_plot(df, "group", "value", na.rm = TRUE)
  summary_no_na <- result_no_na$summary %>% filter(group == "C")
  
  # Check that the mean is correctly calculated when na.rm = TRUE
  expect_equal(summary_no_na$mean_value, 30)
  expect_true(is.na(summary_no_na$sd_value))
})

test_that("summarize_and_plot handles incorrect input gracefully", {
  expect_error(summarize_and_plot(df, "missing_column", "value"), 
               "Specified grouping column not found in data")
  expect_error(summarize_and_plot(df, "group", "missing_column"), 
               "Specified numeric column not found in data")
})
