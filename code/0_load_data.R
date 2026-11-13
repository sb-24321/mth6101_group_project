# Load all the data from the datasets ####################################################

features    <- read.csv('./data/ZakynthosTurtles_MegaDescriptor_features.csv')
annotations <- read.csv('./data/annotations.csv')
bbox        <- read.csv('./data/bbox.csv')

names(features)
dim(features)
head(features[, 1:4])
table(features$orientation)





