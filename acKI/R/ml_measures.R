#' Calculates the Accuracy of a trained model
#' @export
acc = function(trained_model, test_dat, target, n_ref = NULL){
  if(is.null(n_ref)){n_ref = nrow(test_dat)
  } else {n_ref = round(as.numeric(n_ref))}
  return(sum(predict(trained_model, test_dat[setdiff(names(test_dat), target)]) == test_dat[, target]) / n_ref)
}
