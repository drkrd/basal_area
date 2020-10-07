library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

mets_df <- NULL
all_mets <- data.frame(matrix(nrow = 56, ncol=0))
for (i in 1:length(aois))
{
  aoi_f <- lasfilterfirst(aois[[i]])
  aoi_f_h <- lasfilter(aoi_f, Z > 2 & Z < 60)
  mets <- cloud_metrics(aoi_f_h, func = .stdmetrics)
  mets_df <- as.data.frame(matrix(unlist(mets), dimnames = list(names(mets), names(aois)[i])))
  all_mets <- as.data.frame(cbind(all_mets, mets_df))
}

all_mets <- as.data.frame(t(all_mets))


all_mets1 <- as.data.frame(cbind(field_data_ba_summary, all_mets))


full_model <- lm(sum_ba_hec~., data = all_mets1)

step_model <- stepAIC(full_model, direction = "both", trace = FALSE)