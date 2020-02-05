# Creating packages


# Create numeric_summary() function
numeric_summary <- function(x, na.rm) {
  
  # Include an error if x is not numeric
  if(!is.numeric(x)){
    stop("Data must be numeric")
  }
  
  # Create data frame
  data.frame( min = min(x, na.rm = na.rm),
              median = median(x, na.rm = na.rm),
              sd = sd(x, na.rm = na.rm),
              max = max(x, na.rm = na.rm))
}

# Test numeric_summary() function
numeric_summary(airquality$Ozone, TRUE)

# adding functions to the package
## What is in the R directory before adding a function?
dir("datasummary/R")

## Use the dump() function to write the numeric_summary function
dump("numeric_summary", file = "datasummary/R/numeric_summary.R")

## Verify that the file is in the correct directory
dir("datasummary/R")


## Additional documentation
#' Summary of Numeric Columns
#' Generate specific summaries of numeric columns in a data frame
#'
#' @param x A data frame. Non-numeric columns will be removed
#' @param na.rm A logical indicating whether missing values should be removed
#' @import dplyr
#' @import purrr
#' @importFrom tidyr gather
#' @export
#' @examples
#' data_summary(iris)
#' data_summary(airquality, na.rm = FALSE)
#' 
#' @return This function returns a \code{data.frame} including columns: 
#' \itemize{
#'  \item ID
#'  \item min
#'  \item median
#'  \item sd
#'  \item max
#' }
#'
## Add in the author of the `data_summary()` function
#' @author My Name <myemail@example.com>
## List the `summary()` function (from the `base` package)
#' @seealso \link[base]{summary}
data_summary <- function(x, na.rm = TRUE){
  
  num_data <- select_if(x, .predicate = is.numeric) 
  
  map_df(num_data, .f = numeric_summary, na.rm = na.rm, .id = "ID")
  
}
