library(logisticPCA)

# This code requires pre-computed objects from initialcompute.R. It's not standalone.

load("Leader2.Rdata")

qual_cvlpca <- cv.lpca(pca_data_qualities2, ks = 1:20, ms = 5:15)
fun_cvlpca = cv.lpca(pca_data_functions2, ks = 1:20, ms = 5:15)

save(qual_cvlpca, fun_cvlpca, file = 'logisticPCA.RData')
