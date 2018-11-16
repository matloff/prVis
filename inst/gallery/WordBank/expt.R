
wb <- read.csv('English.csv',header=T)
wb <- wb[,c(2,5,6,7,13,15)]
# age birth_order ethnicity    sex         mom_ed vocab

# lots of NA values, just use complete cases
wbcc <- wb[complete.cases(wb),]

wbcc1 <- factorsToDummies(wbcc)
prVis(wbcc1)
plot(uwot:::umap(wbcc1))

