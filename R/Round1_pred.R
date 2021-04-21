# Predicting the outcome of tournaments - Round 1
path = 'data/Data_2002_19.rda'
load(path)

x = rnorm(100)
y = rnorm(100)

y <- Data_2002_19$rd_1[which(Data_2002_19[, 2]<="2016")]

# quantreg::rq(y~x) # Every time you use a package, you need to import it in DESCRIPTION
