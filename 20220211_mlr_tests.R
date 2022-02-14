

# see https://mlr3.mlr-org.com/


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


df

# nochmal ablegen
library(acKI)
library(dplyr)

preprocessing = . %>%
  # necessary preprocessing
  rm_var_miss(threshold = .5) %>%   # removes variables w/ missing pct over threshold
  rm_var_lowvar(threshold = 2) %>%  # removes variables w/ variance under threshold (can also be used to remove constant variables)
  imp_const(imp_val = -1) %>%       # impute missings by a constant
  # optional preprocessing
  reduce_target_cat(target = targets, n = 3, strategy =  "drop") # reduces small classes in the target


df2 = df %>% preprocessing

#install.packages("mlr3")
#install.packages("mlr3verse")
library(mlr3)
library(mlr3verse)

task = TaskClassif$new(id = "1", backend = df2, target = "ef135")
task

# train/test split
train_set <- sample(task$nrow, 0.8 * task$nrow)
test_set <- setdiff(seq_len(task$nrow), train_set)



learner <- lrn("classif.rpart", cp = .05)
# train the model
set.seed(SEED)
learner$train(task, row_ids = train_set)

# predict data
prediction <- learner$predict(task, row_ids = test_set)

# calculate performance
prediction$confusion

measures <- msrs("classif.acc")
prediction$score(measures)



####


task = TaskClassif$new(id = "2", backend = df, target = "ef135")

preprocess = ppl("robustify", task = task, factors_to_numeric= TRUE)


learner <- lrn("classif.rpart", cp = .05)
learner <- lrn("classif.C50")
learner <- lrn("classif.ranger")

pipeline = preprocess %>>% learner
pipeline %>% plot

pipeline_learner = GraphLearner$new(pipeline)
pipeline_learner$param_set


# train the model
set.seed(SEED)
pipeline_learner$train(task, row_ids = train_set)

# predict data
prediction <- pipeline_learner$predict(task, row_ids = test_set)

prediction


# calculate performance
prediction$confusion
# https://mlr.mlr-org.com/articles/tutorial/measures.html
#https://mlr3gallery.mlr-org.com/posts/2021-03-11-practical-tuning-series-build-an-automated-machine-learning-system/
measures <- msrs(c("time_train","classif.acc", "classif.ce"))


# own measures: https://mlr.mlr-org.com/reference/makeMeasure.html
# https://mlr3.mlr-org.com/reference/mlr_measures.html
library(measures)

MultilabelF1

measure = mlr::makeMeasure(id = "my.f1", minimize = TRUE,fun = MultilabelF1)
?makeLearner

f = function(task, model, pred, extra.args) {
  sum((pred$data$response - pred$data$truth)^2)
}
measure = mlr::makeMeasure(id = "my.sse", minimize = TRUE, fun = f)

prediction$score(measure)

f = function(task, model, pred, extra.args)
  sum((pred$data$response - pred$data$truth)^2)
measure = makeMeasure(id = "my.sse", minimize = TRUE, properties = c("regr", "response"), fun = f)
#https://arxiv.org/pdf/1609.06146.pdf

MultilabelF1
measure

msr("MultilabelF1")
mlr3::makeMeasure
pipeline_learner$print()

prediction

listMeasures(obj, properties = character(0L), create = FALSE)

install.packages("measures")


prediction$score(msr("classif.ce"))
?learners
prediction$score(MultilabelF1)
?msr
MultilabelF1

library(mlr)

listMeasures("classif", properties = "classif.multi")


mlr::performance(prediction, measures = auc)



learners = list(
  lrn("classif.kknn", id ="kknn"),
  lrn("classif.ranger", id = "ranger"),
  lrn("classif.C50", id = "C5.0")
)


graph = ppl("robustify", task = task, factors_to_numeric = TRUE) %>>%
  po("collapsefactors", target_level_count = 5) %>>%
  ppl("branch", lapply(learners, po))

graph %>% plot

graph_learner = GraphLearner$new(graph)

graph_learner$param_set

po("collapsefactors")
set.seed(SEED)
graph_learner$train(task, row_ids = train_set)
graph
po("collapsefactors")
prediction <- graph_learner$predict(task, row_ids = test_set)
?mlr3pipelines::collapsefactors
# calculate performance
prediction$confusion
# https://mlr.mlr-org.com/articles/tutorial/measures.html
measures <- msrs(c("classif.acc", "classif.ce"))
prediction$score(measures)

#https://mlr.mlr-org.com/articles/tutorial/create_measure.html


prediction

