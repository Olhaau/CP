# Interface fuer Pipeline des Anwendungscampus KI
# Funktionen der Pipeline: Verknuepfen und Visualisieren
# Zugriff auf eigenes Paket
###############################################################################
# Clean all
rm(list = ls())
while(!is.null(dev.list())) dev.off()
###############################################################################
# Import packages
library(acKI)       # own created package
#library(caret)      # ML -> in own package used
library(ggplot2)    # plot
#library(dplyr)      # data cleaning
#library(doParallel) # parallelization -> to package
#library(stringr)

###############################################################################
## Parameter
SEED = 1

wd = 'C:/Users/Asus/Documents/01_code/20220104_CampusPipe/'
path_data = 'Daten/StudentenstatistikWS_2000'

x_vars = c("ef32")
y_vars = c("ef135")
x_origins = c(2)       # index of datasets of x_vars
y_origins = c(1)
y_types = c("cat")     # cat or num for regression or classification
integration_order = c(1,2) # means transfer infos from dataset 1 to 2

# for plotting
#Schluessel fuer EF32 -> Metadaten
val = c(
  "sprach- und Kulturwissenschaften",
  "sport",
  "rechts-, wirtschafts- und sozialwissenschaften",
  "mathematik, naturwissenschaften",
  "humanmedizin/gesundheitswissenschaften",
  "veterinärmedizin",
  "agrar-, forst- und ernährungswissenschaften",
  "ingenieurwissenschaften",
  "kunst-, kunstwissenschaft",
  "außerhalb der studienbereichsgliederung"
)
val = sapply(val, function(x) {str_replace(str_to_title(x), "Und", "und")})
x_key_list = list(data.frame(key = 1:10, val))

#Schluessel fuer EF135 -> Metadaten
val = c(
  "mit auszeichnung bestanden (summa cum laude)",
  "bestanden, gesamtnote nicht bekannt",
  "sehr gut (magna cum laude)",
  "gut (cum laude)",
  "befriedigend (rite)",
  "ausreichend",
  "voll befriedigend",
  "bestanden, gesamtnote nicht bekannt",
  "endgültig nicht bestanden"
)
key = c(0,8,100,200,300,400,700,800,900)
y_key_list = list(data.frame(key, val))
y_key_list

###############################################################################
###############################################################################
###############################################################################
# -- from here automated --
###############################################################################
# read data
# TODO: drop the data.frames, only hold the list or only load
setwd(wd)

datlist = read_all_files(path_data)
# for more than 2 datasets loop from here over integration_order[:-1]
ind_d = integration_order[1]
ind_r = integration_order[2]

# declare donator and receiver -> later iteratively
dat_d = datlist[[ind_d]]
dat_r = datlist[[ind_r]]
###############################################################################
# - create dataset w/ only variables in the intersect and missing variables of interest
com_vars = intersect(names(dat_d),  names(dat_r))
targets = setdiff(c(y_vars,x_vars), com_vars)
df = dat_d[c(com_vars, targets)]

# for Classification convert the targets to factor
if (length(targets) > 1){df[,targets] = lapply(df[,targets], factor)
} else if (length(targets) == 1){df[,targets] = factor(df[,targets])}
###############################################################################
## Preprocessing
preprocess = . %>%
  # necessary preprocessing
  rm_var_miss(threshold = .5) %>%   # removes variables w/ missing pct over threshold
  rm_var_lowvar(threshold = 1) %>%  # removes variables w/ variance under threshold (can also be used to remove constant variables)
  imp_const(imp_val = -1) %>%       # impute missings by a constant
  # optional preprocessing
  reduce_target_cat(target = targets, n = 4, strategy =  "combine") # reduces small classes in the target


#df_no_preprocess = df               # save not processed data
#df = df %>% preprocess              # execute preprocessing -> seperated for train/test (test data should not be seen in preprocessing, e.g. scaling fitted on train data)
#summary(df)                         # output

# TODO
# - impute different strategies
# - encoding categorical data
# - feature scaling
# - resampling: smote, rose, ...
# - fix: reduce_target_cat, for fixed n, same number of remaining classes, maybe n = remaining classes
###############################################################################
# Train test split
set.seed(SEED)
p_tts = .66
index_train = caret::createDataPartition(df[, targets], p=p_tts , list=FALSE)
trd = df[ index_train,]
ted = df[-index_train,]

# preprocess the data
trd_pro = trd %>% preprocess
ted_pro = trd %>% preprocess
###############################################################################
# training
train = function(data, ...){acKI::train(data = data, target = targets, n_kernel = .75, ...)} # needed?
?acKI::train
###############################################################################
# test: erweitern oder default in package hinterlegen
# list of available models:
# https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
# needed: ...
input = list(
  list(method = "rpart", grid = expand.grid(.cp=c(.01,.1, 1)))
  #, list(method = "rpart")
  #, list(method = "multinom")
  #, list(meth = "nb", tuneGrid = expand.grid(.fL=c(0,1,1.25,1.5,1.75,2), .adjust=c(0.5,1,2), .usekernel=c(TRUE))) # slow?
  #, list(method = "nb", grid = expand.grid(.fL=c(0,1,2), .adjust=c(0.5,1,2), .usekernel=c(TRUE))) # too slow, w?
  , list(method = "C5.0", grid = expand.grid(.trials=c(20,30,40), .model=c("tree"), .winnow=c(TRUE,FALSE)))
  , list(method = "gbm")
  , list(method = "ada")
  # slow
  , list(method = "rf", grid = expand.grid(.mtry=floor(sqrt(length(df[,targets[1]]))/(seq(0.5,1,1.5)))))
  , list(method = "knn", grid = expand.grid(.k=seq(1,21,2)))
  , list(method = "svmRadialSigma", grid = expand.grid(.trials=c(20,30,40), .model=c("tree"), .winnow=c(TRUE,FALSE), .cost=c(0.5,1,2)))
)

output = input

output = autotrain(trd_pro, targets, ted_pro
              , models = output                          # models to use
              , save_path = getwd()                      # if set, models will be saved in path
              , max_time = 60 * 5 #60 * 2                   # last number in hours
              , n_kernel = parallel::detectCores() - 1)  # use all cores except 1



output[1:2]

results = output %>% summary_results

#results[setdiff(colnames(results), c("total_time", "grid", "path"))][1:11]


results %>% print_summary


results %>% write.csv2(file =paste0(path_data, "/models/_time2_model_results.csv"), row.names = FALSE)



# -> [ ] times in outputs
# -> [x] results for each class
# -> [x] to disk (first set wd)
# -> [ ] write log to disk
# -> [x] ausschnitt ausgeben
# -> [ ] aus gespeicherten Modellen methode, grid, tunedVaue, time, und conf_matrix auslesen -> summary_results


# -- xxx --
# TODO print_results(out, ted_pro, targets) -> model, grid, runtime(final/total), measures (acc, kappa, F1, ...), later preprocess
## show results
# use: data2string(out[[1]]$fit$bestTune)
# does the time restriction work?
# wird das Ergebnis rekursiv befuellt?
# add prio, order
### func: fill_train -> training for fixed time
# if append_results(inputs2, inputs)[[1]]["fit"] is null, train, fill fit, runtime,
### func: show_training_results, add acc, F1, ... prints result as dataframe




################################################
# old:
#result_list[[1]]
#length(result_list[[2]])

rem_ind = c()
for (i in 1:length(result_list)){
  if(length(result_list[[i]]) == 1){result_list[[i]]$grid = list(NULL)
  print(i)
  }
  if(length(result_list[[i]]) < 3){
    result_list[[i]]$preprocess = preprocess
    result_list[[i]]$fit = NA
    result_list[[i]]$runtime = NA
  }
  if (typeof(result_list[[i]]$fit) != "list"){
    rem_ind = c(rem_ind, i)}
}

rem_ind


test_list[[1]] = NULL
test_list

# train models
for (i in rem_ind){
  input = result_list[[i]]
  start = Sys.time()
  result_list[[i]]$fit = train(trd, method = input$meth, tuneGrid = input$grid)
  result_list[[i]]$runtime = as.numeric (Sys.time() - start, units = "mins")
}


result_list


test_list = list(1,2)
test_list[[3]] == 2

# todo: result_output
result_output = list()
result_output[[1]]$a = 3
result_output
for (i in 1:length(result_list)){
  result_output[[i]] = list(method = result_list[[i]]$meth,
                            runtime = result_list[[i]]$runtime)
}
data.frame(result_output)



ind = 1
input = result_list[[ind]]
if (typeof(input$fit) != "list") {
  #measure time
  #only calc a fixed time
  start_time = Sys.time()
  model = trd_pro %>% train(method = input$meth, tuneGrid = input$grid)
  runtime = as.numeric (Sys.time() - start_time, units = "mins")
  model_list[[ind]] = model
  result_list[[ind]]$runtime = runtime
  result_list[[ind]]$acc = acc(model, ted_pro, targets)

}else{sprintf("model %s already used", ind)}


result_list[[]]$model
result_list

data.frame(result_list)


str(preprocess)

str(model)
deparse(preprocess)
preprocess
# how to handle multiple outputs?
# e.g.https://stats.stackexchange.com/questions/49513/packages-or-libraries-for-multiple-output-learning
# TODO: integrate model optimization, use results of LMU
# TODO: save results and plot to disk

###############################################################################
# Combine dataframes
# distributed learning: https://tensorflow.rstudio.com/tutorials/advanced/distributed/distributed_training_with_keras/
# 1. Investigate structural differences of the data.frames
smry_d = summary(dat_d[com_vars])
smry_e = summary(dat_r[com_vars])


dat_all = rbind(dat_d[com_vars], dat_r[com_vars])
dat_all[1:nrow(dat_d),"origin"] = "don"
dat_all[nrow(dat_d) +(1:nrow(dat_r)),"origin"] = "rec"

# mean, sd, etc,...
getOption("max.print")
options(max.print=1000)
getOption("max.print")

dat_comp = dat_all %>% dplyr::group_by(origin) %>% dplyr::summarise_all(list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))) %>% t



dat_all %>% dplyr::group_by(origin) %>% dplyr::summarise_all(~sd(.x, na.rm = TRUE)) %>% t
dat_all %>% dplyr::group_by(origin) %>% dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>% t

#dat_comp[,"diff"] = as.numeric(dat_comp[,1]) - as.numeric(dat_comp[,2])
# -> geht nicht, warum?
dat_comp

# 2. use additional information to combine the data or assume independence
# ...



###############################################################################
### Visualization

## 1. Stacked bar plot
dat_plot = dat_d  # later new synthetical dataset
ind = 1 # first x and y variable, later plot more and use different plots

# create the bar heights
dat_plot = na.omit(dat_plot[c(x_vars, y_vars)]) %>% group_by_at(c(x_vars, y_vars)) %>% count %>% data.frame

# Add labels for x- and y-variables
x_key = x_key_list[[ind]]
for(key in x_key$key){dat_plot[dat_plot[,x_vars[ind]] == key, "x_var_label"] = x_key[x_key$key == key, "val"]}
y_key = y_key_list[[ind]]
for(key in y_key$key){dat_plot[dat_plot[, y_vars[ind]] == key, "y_var_label"] = y_key[y_key$key == key, "val"]}

# create the actual plot
ggplot(dat_plot, aes(fill = reorder(y_var_label, !! sym(y_vars[ind])), y = n, x = reorder(x_var_label,- !! sym(x_vars[ind])))) +
  geom_bar(position="stack", stat="identity") +
  labs(fill = attr(dat_d[, y_vars], "label"),
       x = attr(dat_d[, x_vars], "label")) +
#
#
# costomizations afterward
labs(title = "Notenverteilung nach Studienfach") +
coord_flip()


###############################################################################
###############################################################################
###############################################################################


###############################################################################
###############################################################################
###############################################################################

# Anhang
# TODO
# - fix plot from package

# tests
?read_all_files




