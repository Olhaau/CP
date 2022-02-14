# Clean all
rm(list = ls())
while(!is.null(dev.list())) dev.off()

product = "01_Microzensus_SplitCombine1"
dat_name = "mcpanel"

#https://ec.europa.eu/eurostat/cros/system/files/DOrazio-etal_The%20use%20of%20uncertainty%20to%20choose%20the%20matching%20variables%20in%20statistical%20matching%20with%20authors.pdf
#also:
# - record linkage: zusammenfuegen anhang eind. identifikatoren
# - linken anhand aehnlichkeiten
# - statistical matching
# - fange R notebook an, dass den Verlauf wiederspiegelt

wd = paste('C:/Users/Asus/Documents/01_code/20220104_CampusPipe/products', product, sep ="/")
setwd(wd)

list.files("daten")[[2]]
#library(foreign)
#library(tidyr)
#df = read.spss(paste("daten",list.files("daten")[[1]], sep = "/"))
# -> hat nicht so gut geklappt
# Stata? -> besser
rm(df)
if (paste0(dat_name, ".rda") %in% list.files("daten")){
  load(paste0("daten/",dat_name, ".rda"))
} else {
  df = haven::read_dta(paste("daten","fdz_cf_mz_panel_stata.dta", sep = "/"))
  save(df, file = paste0("daten/",dat_name, ".rda"))
  write.csv2(df, file = "daten/mcpanel.csv", row.names = FALSE)
}

## Splits



## Anhang: tests
apply(expand.grid(c("a","b","c"), c(1,2,3)),1, . %>% paste(collapse=""))

sum(!grepl("af|bf|cf",colnames(df)))
# split 1: 3 only one year (af?),
sample(nrow(df), 10)

setdiff(c("a", "b", "c", "d"), "a")
library(tidyr)
df[!grepl("af|bf|cf",colnames(df))] %>% str

"af" %in% colnames(df)[14]

colnames(df)[14]
str(df)

