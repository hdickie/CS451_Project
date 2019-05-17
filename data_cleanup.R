library(plyr)

## Read table
train_data <- read.table("data/bio_train.dat", header = FALSE)

## Remove unnecessary columns
train_data <- train_data[-c(0:2)]

# Count per classification
count(train_data, "V3")

# Check for any null values
apply(train_data, 2, function(x) any (is.null(x)))
apply(train_data, 2, function(x) any (is.na(x)))

