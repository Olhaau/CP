#' Reads all CSV and sas7bdat files
#'
#'
#' TODO:
#' - add Excel, ...
#' @param path A path.
#' @return a list of dataframes.
#' @export
#'
read_all_files = function(path){
  sas_files = list.files(path, pattern=".sas7bdat")
  csv_files = list.files(path, pattern=".csv")

  sas_data_list = lapply(sas_files, function(x){
    as.data.frame(haven::read_sas(paste(path, x, sep = "/")))
  })
  csv_data_list = lapply(csv_files, function(x){read.csv2(paste(path, x, sep = "/"))})

  output = c(sas_data_list, csv_data_list)


  names(output) = c(sapply(sas_files, function(x){stringr::str_remove(x, ".sas7bdat")}),
                    sapply(csv_files, function(x){stringr::str_remove(x, ".csv")}))

  return(output)
}

#' Removes variables with too many missings.
#' @export
rm_var_miss = function(data, threshold = .5){
  # remove variables with too many missings
  data[colSums(is.na(data)) <= nrow(data) * threshold]
}

#' Removes variables with low variance.
#' @export
rm_var_lowvar = function(data, threshold = 0.01){
  data[,names(which(apply(data, 2, function(x){var(as.numeric(x), na.rm=TRUE)}) > threshold))]
}

#' Imputes missing values by a constant.
#' @export
imp_const = function(data, imp_val = -1){
  data[is.na(data)] = imp_val
  return(data)}

#' Imputes missing values.
#' @export
impute = function(data, val_miss = NA, meth = "constant", imp_val = 0){
  #data[data == val_miss] = NA
  if(meth == "constant"){data[is.na(data)] = imp_val
  # TODO refactor factors
  }
  # TODO further methods, packages hmisc, mi ... (different syntax)
  # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
  return(data)
}

#' Reduces categories of target.
#' @export
reduce_target_cat = function(data, target, n, strategy = "combine"){

  n_smallest_classes = names(sort(table(data[, target]))[1:n])
  ind_in_small_class = sapply(data[, target], function(x){x %in% n_smallest_classes})
  if (strategy == "drop"){
    data = data[!ind_in_small_class,]

  } else if (strategy == "combine"){
    data[ind_in_small_class, target] = n_smallest_classes[n]

  }
  data[,target] = droplevels(data[,target])
  return(data)
}







##########################################################################################################################
# old - not needed
#' Necessary Preprocessing
#'
#' Removes constant variables and optionally variables with too many missing and imputes missing values by a constant.
#' @param dataset A dataframe.
#' @param missings_threshold Missings threshold in [0,1] (if NULL, no variable will be removed).
#' @param const_imputation Value which will replace each missing (if NULL, no value will be imputed).
#' @return A cleaned dataset.
preprocess_necessary = function(dataset, missings_threshold = 0.5, const_imputation = -1){
  # remove constant variables (chosen because of no dependencies)
  df = dataset
  df = df[apply(df, 2, function(x){length(unique(x))}) > 1]
  # alternatively:
  # use dplyr: df = (dataset %>% dplyr::select_if(~dplyr::n_distinct(.)>1))
  # 10 times faster: df[,-dataPreparation::which_are_constant(df, verbose=FALSE)]
  # see: https://stackoverflow.com/questions/15068981/removal-of-constant-columns-in-r

  # encode characters and non_numeric data
  # needed? caret does onehot in the background...

  # optionally remove variables with too many missings
  if (!is.null(missings_threshold)){
    if((0 <= missings_threshold) & (missings_threshold <= 1)){
      df = df[colSums(is.na(df)) < nrow(df) * missings_threshold]
    }else{print("missings_threshold has to be a number in [0,1] or NULL.")}
  }
  # optionally constant imputation
  if (!is.null(const_imputation)){
    df[is.na(df)] = const_imputation
  }
  return (df)
}

#' Optional Preprocessing
#'
#' can currently remove variables with low variance and small classes of the target
#'
#' TODO: Encoding, Scaling, Resample (extra step?)
#' TODO: each step as own function, combine in "preprocess_optional"
#' @param dataset A dataframe.
#' @return A cleaned dataset.
preprocess_optional = function(
  dataset, target,
  reduce_variables = NULL, variance_threshold = 1,
  reduce_target = NULL, reduce_target_n = 0
){
  # remove variables with low variance
  if (reduce_variables == "variance_threshold"){
    dataset = dataset[,c(names(which(apply(dataset[setdiff(names(dataset), c(target))], 2, function(x){var(as.numeric(x))}) > variance_threshold)), target)]
  }

  # reduce categories of target
  n_smallest_classes = names(sort(table(dataset[, target]))[1:reduce_target_n])
  ind_in_small_class = sapply(dataset[, target], function(x){x %in% n_smallest_classes})
  if (reduce_target == "drop_n_smallest"){
    dataset = dataset[!ind_in_small_class,]

  } else if (reduce_target == "combine_n_smallest"){
    dataset[ind_in_small_class, target] = n_smallest_classes[reduce_target_n]

  }
  dataset[,target] = droplevels(dataset[,target])

  return(dataset)
}

# optimize preprocessing with target output of baseline model
# auto_model

