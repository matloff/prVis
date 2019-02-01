# sitka89
# https://vincentarelbundock.github.io/Rdatasets/doc/geepack/sitka89.html
sitka <- read.csv("~/desktop/sitka89.csv", header = T)
sitka <- sitka[, -1]
str(sitka)
prVis(sitka, labels = T)