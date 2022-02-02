#' Analysis of Missingness
#'
#'
#' @param data A data.frame.
#' @param na_val Declared value for missings.
#' @return a list of dataframes.
#' @export
miss_eda = function(data, na_val = NA, visual = TRUE, warn_large_data = TRUE){
  dataset[dataset == na_val] = NA

  #does not show the plot, same code from a function works, there should be a problem with starting from packages
  if (visual){
    dev.new(noRStudioGD = TRUE)
    naniar::vis_miss(data, warn_large_data = warn_large_data) + theme(axis.text.x = element_text(angle = 80))}

  #no dependencies, worse representation of the result
  #t(apply(dat_d, 2, function(x)sum(is.na(x))))
  return(naniar::miss_var_summary(data))
}

