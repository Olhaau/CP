#' Make or append a result list with  placeholders if needed.
#'
#' needed? -> yes, only appends not already calculated
#' @export
append_results = function(inc_methods, init_list = NULL){
  for (i in 1:length(init_list)){
    if(length(init_list[[i]]) == 1){init_list[[i]]['grid'] = list(NULL)}
  }
  for (i in 1:length(inc_methods)) {
    if(length(inc_methods[[i]]) == 1){inc_methods[[i]]['grid'] = list(NULL)}

    old_bool =
      (TRUE %in% (sapply(init_list, function(x){identical(
        list(x$meth, x$grid),
        list(inc_methods[[i]]$meth, inc_methods[[i]]$grid))})))

    if(!old_bool){init_list[[length(init_list) + 1]] = inc_methods[[i]]}
  }
  init_list
}

#' Custom wrapper for caret::train
#'
#' - includes choosable timeout,
#' - target is passed as name,
#' - modified standard trControl
#' - includes parallelization
#' - short output
#' - sometime if timeout, cluster is not stopped, a new run with enough time helps
#' - the timeout kills the parallelization
#'
#' TODO: multiple targets?
#' TODO: Why is the custom timeout message not shown? and the timeout does not work in every case
#' @export
train = function(data, target,
                 max_time = 2 * 60 * 60, n_kernel = 1, show_results = TRUE,...){

  # start parallelization
  max_kernel = min(parallel::detectCores(), 12)
  kernel_before = getDoParWorkers()
  if (0 <= n_kernel & n_kernel < 1)    n_kernel = as.integer(n_kernel * max_kernel)
  if (n_kernel > max_kernel) n_kernel = max_kernel
  if (kernel_before == 1){
    n_cluster = parallel::makeCluster(as.integer(n_kernel))
    doParallel::registerDoParallel(n_cluster); para  = TRUE
  } else {para = FALSE}
  if (show_results) print(paste0("Start training ML-model using ", getDoParWorkers(), " of ", max_kernel, " cores."))

  # fitting the model
  fit = NULL
  fit = R.utils::withTimeout(
    {caret::train(form = as.formula(paste0(target, "~.")), data = data,
                  trControl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 10, allowParallel = TRUE),
                  ...)
    }, timeout = max_time, onTimeout = "warning")
  if(!is.null(fit) & show_results){print(paste0(fit$modelInfo$label, ": accuracy = ",
                                  round(max(fit$results$Accuracy), 4),
                                  ", kappa = ", round(max(fit$results$Kappa), 4), " (CV); time (final/total): ",
                                  round(fit$time$final[3], 2), "/", round(fit$time$everything[3], 2), " sec."))
  }

  # end parallelization
  if(para){parallel::stopCluster(n_cluster); registerDoSEQ()}

  return(fit)}


#' Automatically trains multiple models for a fixed time
#'
#' @export
autofit = function(traindata, target_name, test_data = NULL, models,   save_path = NULL,
                   max_time = 5 * 60, n_kernel = 1, show_results = TRUE, ...){
  start_time = Sys.time()
  result_list = models
  # start parallelization
  max_kernel = parallel::detectCores()
  kernel_before = getDoParWorkers()
  if (0 <= n_kernel & n_kernel < 1)    n_kernel = as.integer(n_kernel * max_kernel)
  if (n_kernel > max_kernel) n_kernel = max_kernel
  if (kernel_before == 1){
    n_cluster = parallel::makeCluster(as.integer(n_kernel))
    doParallel::registerDoParallel(n_cluster); para  = TRUE
  } else {para = FALSE}
  if (show_results) {
    print(paste0("Start training ML-models using ", getDoParWorkers(), " of ", max_kernel, " cores."))}

  # get the indices of not already used parameters
  #ind_nottrained = sapply(1:length(result_list), function(i){if(!("fit" %in% names(result_list[[i]]))) i})
  ind_nottrained = c()
  for (i in 1:length(result_list)) if(!("fit" %in% names(result_list[[i]]))) ind_nottrained = c(ind_nottrained, i)

  j = 1
  while (j <= length(ind_nottrained) & as.numeric(Sys.time() - start_time, units = "secs") < max_time){
    i = ind_nottrained[j]
    if (length(result_list[[i]]) == 1) result_list[[i]]['grid'] = list(NULL)

    fit = NULL
    outtrycatch = tryCatch({

      fit = train(data = traindata, target = target_name, method = result_list[[i]]$method,
                  tuneGrid = result_list[[i]]$grid, show_results = FALSE, ...)

      # does not properly work?
      # test: only write a string at that spot: works
      # test: only 10 small models (e.g. CART): works
      # test: only C5.0 but 2-4x: works
      # test: append(output, new_inputs) as input: works
      # test: append the input, until it breaks, already testes: cart, cart, multinom, C5.0

      # TODO: idea: write model to disk and save only path in list
      # only test:
      #fit = NULL
      #result_list[[i]]$fit = paste0(j, "-th model")
    }, error = function(e){}, warning = function(w){}, finally = {})
    if(!(is.null(fit)) ){
      result_list[[i]]$tuneValue = fit$finalModel$tuneValue
      result_list[[i]]$times = fit$times


      if(!(is.null(test_data)) ){
        confusionmat = caret::confusionMatrix(predict(fit, test_data[setdiff(names(test_data), target_name)]), test_data[,target_name])
        result_list[[i]]$confusionMatrix = confusionmat
      }
    }


    if(!(is.null(save_path))){
      mod_path = paste0(save_path, "/models")
      if(!(file.exists(mod_path))) dir.create(mod_path)
      if(!(is.null(result_list[[i]]$grid))) {grid_string = stringr::str_replace_all(stringr::str_replace_all(stringr::str_remove_all(data2string(result_list[[i]]$grid), " "), "[,]", "_"), ";\n", "_")
      } else {grid_string = NULL}


      file_path = paste0(mod_path, "/", strftime(Sys.time(), format = "%Y%m%d_%H%M"),"_", stringr::str_remove_all(result_list[[i]]$method, "[.]")
                         , "_",
                         grid_string,".rds")
      #str_remove_all(paste0(names(result_list[[i]]$grid), collapse = "_"), "[.]")


      saveRDS(fit, file_path)
      result_list[[i]]$path = file_path
    }

    # output message
    if(!is.null(typeof(fit)) & typeof(fit) == "list" & show_results){print(paste0(
      j," / ",length(ind_nottrained), ": ",
      fit$modelInfo$label, ": accuracy = ",round(max(fit$results$Accuracy), 4),
      ", kappa = ", round(max(fit$results$Kappa), 4),
      " (CV); time (final/total): ", round(fit$time$final[3], 2), "/", round(fit$time$everything[3], 2), " sec. (remaining: ",
      round(max(max_time - as.numeric(Sys.time() - start_time, units = "secs"), 0), 2) ,")"
      ))
    }

    j = j + 1
    time = Sys.time() - start_time

    # timeout message
    if(time >= max_time) print(paste0(
      j," / ",length(ind_nottrained), ": ", result_list[[i]]$method, ": Timeout."))
  }



  # end parallelization
  if(para){parallel::stopCluster(n_cluster); registerDoSEQ()}

  return(result_list)
}


#' Prints the trainingsresults.
#' @export
print_results = function(result_list, testdata = NULL, target_name = NULL){
  out = result_list
  for(i in 1:length(out)){
    if(typeof(out[[i]]$grid) == "list") {out[[i]]$grid = data2string(out[[i]]$grid)
    } else if (out[[i]]$grid %>% is.null){out[[i]]$grid = NA}
    if (out[[i]]$fit %>% is.null){out[[i]]$fit = NA; out[[i]]$acc = NA
    } else if(typeof(out[[i]]$fit) == "list") {
      if(!is.null(testdata) & !is.null(target_name)){
        out[[i]]$acc = acc(out[[i]]$fit, testdata, target_name)
      }
      out[[i]]$fit = out[[i]]$fit$modelInfo$label
    }
  }
  do.call(rbind.data.frame, out)
}
