#' @importFrom dplyr %>%
utils::globalVariables(c(".data", "mean_value", "sd_value"))

#' Summarize and Plot Grouped Data
#'
#' This function calculates the mean and standard deviation of a numeric variable, grouped by a specified column,
#' and returns both the summary table and a plot showing the mean and standard deviation for each group.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param group_col A character string specifying the column to group by.
#' @param numeric_col A character string specifying the numeric column to summarize.
#' @param na.rm Logical; if `TRUE`, removes `NA` values before computing summary statistics.
#' @param fill_color Character string specifying the fill color of the bars in the plot.
#' @param error_bar_width Numeric; specifies the width of the error bars. Default is `0.2`.
#' @return A list containing:
#'   \item{summary}{A data frame with the mean and standard deviation of the numeric variable, grouped by the specified column.}
#'   \item{plot}{A `ggplot2` object showing the mean and standard deviation (error bars) by group.}
#' @examples
#' # Example 1: Using the iris dataset
#' result <- summarize_and_plot(iris, "Species", "Sepal.Length")
#' print(result$summary)
#' print(result$plot)
#'
#' # Example 2: Using the mtcars dataset
#' result_mtcars <- summarize_and_plot(mtcars, "cyl", "mpg")
#' print(result_mtcars$summary)
#' print(result_mtcars$plot)
#'
#' # Example 3: Handling missing columns (should produce an error)
#' \dontrun{
#' summarize_and_plot(iris, "missing_column", "Sepal.Length")
#' }
#'
#' # Example 4: Handling NA values
#' sample_data <- data.frame(
#'   category = c("A", "A", "B", "B", "C", "C"),
#'   value = c(10, NA, 15, 20, NA, 25)
#' )
#' result_na <- summarize_and_plot(sample_data, "category", "value", na.rm = FALSE)
#' print(result_na$summary)
#' print(result_na$plot)
#' 
#' @export
summarize_and_plot <- function(data, group_col, numeric_col, na.rm = TRUE, fill_color = "lightpink", error_bar_width = 0.2) {
  # Check that the data frame contains specified columns
  if (!group_col %in% names(data)) stop("Specified grouping column not found in data")
  if (!numeric_col %in% names(data)) stop("Specified numeric column not found in data")
  
  # Check that the numeric_col is numeric
  if (!is.numeric(data[[numeric_col]])) stop("Specified numeric column is not numeric")
  
  # Summarize data by group using dplyr::summarise()
  summary_data <- data %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      mean_value = mean(.data[[numeric_col]], na.rm = na.rm),
      sd_value = stats::sd(.data[[numeric_col]], na.rm = na.rm),
      .groups = 'drop'
    )
  
  # Plot the mean values with error bars for standard deviation
  plot <- ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[group_col]], y = mean_value)) +
    ggplot2::geom_col(fill = fill_color) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = error_bar_width) +
    ggplot2::labs(
      title = paste("Mean and SD of", numeric_col, "by", group_col),
      x = group_col,
      y = paste("Mean of", numeric_col)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # Return a list with the summary data and the plot
  return(list(summary = summary_data, plot = plot))
}
