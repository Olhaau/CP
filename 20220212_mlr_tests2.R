
# Interface fuer Pipeline des Anwendungscampus KI
# Funktionen der Pipeline: Verknuepfen und Visualisieren
# Zugriff auf eigenes Paket
###############################################################################
# Clean all
rm(list = ls())
while(!is.null(dev.list())) dev.off()

###############################################################################
# Import packages
# see https://mlr3.mlr-org.com/

library(acKI)
###############################################################################
## Parameter
SEED = 1

wd = 'C:/Users/Asus/Documents/01_code/20220104_CampusPipe/products/00_ErwStudienabschluesse'
path_data = 'daten'

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

if (length(targets) > 1){df[,targets] = lapply(df[,targets], factor)
} else if (length(targets) == 1){df[,targets] = factor(df[,targets])}


#### test mlr
# mlr book: https://mlr3book.mlr-org.com/basics.html#tasks-creation
# if task does not work: install.packages("mlr3")
# https://stackoverflow.com/questions/68576432/why-does-my-mlr3-classification-task-have-no-features

install.packages("mlr3")
detach("package:mlr3", unload=TRUE)
library(mlr3)
library(mlr3verse) # -> for ppl pipeline/graph needed


#### create a task
task = as_task_classif(df, target = "ef135", id = "1")
# altern.: task = TaskClassif$new(id = "1", backend = df, target = "ef135")
print(task)

#### train/test split
train_set <- sample(task$nrow, 0.8 * task$nrow)
test_set <- setdiff(seq_len(task$nrow), train_set)


#### recommended preprocessing, train basic learner
preprocess = ppl("robustify", task = task, factors_to_numeric= TRUE)
learner = lrn("classif.rpart")

#https://mlr3book.mlr-org.com/pipelines.html
po("imputeconstant", constant = -1)

graph =

  preprocess %>>%
  #po("filter", mlr3filters::flt("variance"), filter.frac = .1) %>>%
  po("collapsefactors", target_level_count = 2) %>>%
  #po("classbalancing") %>>%  # rpart, only acc 0.4587156
  #po("pca") %>>% # rpart, only acc 0.4587156
  #po("smote") %>>%
  learner
plot(graph)

{
graph_learner = GraphLearner$new(graph)
# learning
set.seed(SEED)
graph_learner$train(task, row_ids = train_set)
# predict data
prediction <- graph_learner$predict(task, row_ids = test_set)
prediction$confusion


# train the model
set.seed(SEED)
learner$train(task, row_ids = train_set)

# predict data
prediction <- learner$predict(task, row_ids = test_set)

# calculate performance
prediction$confusion
prediction$score(msrs(c("classif.acc", "classif.ce")))
}
task

#### multiple models preprocessing branching
# https://mlr3gallery.mlr-org.com/posts/2021-03-11-practical-tuning-series-build-an-automated-machine-learning-system/

prediction

learner = as_learner(graph)
learner
as.data.table(learner$param_set)
?msr
library(mlr)
multilabel.f1

multilabel.f1(c("1", "2"), c("2", "2"))

prediction
#### optimization, evaluation
#### f1 or custom scores and their optimization
#### custom imputation, measures, preprocessing, models, ...

