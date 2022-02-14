
install.packages("mlr3verse")
install.packages("mlr3")
library("mlr3")
library("mlr")
data("mtcars", package = "datasets")
data = mtcars[, 1:3]
str(data)

task_mtcars = as_task_regr(data, target = "mpg", id = "cars")
print(task_mtcars)

as.data.table(mlr_tasks)

tsk("penguins")
