README
================
Gelareh Modara
2024-11-07

# summarizeandplot

`summarizeandplot` is an R package that provides an easy way to
calculate and visualize summary statistics for grouped data. The package
includes functions to group data by a specified column, calculate the
mean and standard deviation of a numeric variable, and generate a
`ggplot2` plot with error bars.

## Features

- Group data by a specified column.
- Calculate mean and standard deviation of numeric variables.
- Generate bar plots with error bars representing the standard
  deviation.

## Installation

You can install version `0.1.0` of `summarizeandplot` from GitHub with:

``` r
# If you haven't installed devtools, install it first
# install.packages("devtools")

# Install summarizeandplot from GitHub
devtools::install_github("stat545ubc-2024/summarizeandplot", ref = "0.1.0")
```

## Usage

Below are examples on how to use summarize_and_plot with different
datasets.

### Example 1: Using the iris dataset

This example calculates the mean and standard deviation of Sepal.Length,
grouped by Species, using the built-in iris dataset.

``` r
library(summarizeandplot)

# Summarize and plot Sepal.Length by Species in the iris dataset
result <- summarize_and_plot(iris, "Species", "Sepal.Length")
print(result$summary)
print(result$plot)
```

### Example 2: Using the mtcars dataset

Here, we calculate the mean and standard deviation of mpg (miles per
gallon), grouped by the number of cylinders (cyl), in the mtcars
dataset.

``` r
# Summarize and plot mpg by cyl in the mtcars dataset
result_mtcars <- summarize_and_plot(mtcars, "cyl", "mpg")
print(result_mtcars$summary)
print(result_mtcars$plot)
```

### Example 3: Handling Missing Columns (Expected Error)

If you try to use a column that doesn’t exist, the function will return
an error, helping to prevent mistakes in column names.

``` r
# This will produce an error as "missing_column" does not exist in the iris dataset
try(summarize_and_plot(iris, "missing_column", "Sepal.Length"))
```

### Example 4: Handling NA Values

You can control how NA values are handled by setting na.rm = TRUE or
na.rm = FALSE. Here’s an example with a sample dataset containing NA
values.

``` r
# Sample dataset with NA values
sample_data <- data.frame(
  category = c("A", "A", "B", "B", "C", "C"),
  value = c(10, NA, 15, 20, NA, 25)
)

# Summarize and plot while including NA values
result_na <- summarize_and_plot(sample_data, "category", "value", na.rm = FALSE)
print(result_na$summary)
print(result_na$plot)
```
