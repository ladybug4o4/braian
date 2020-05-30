# Title     : DissNames
# Objective : Precomputed matrix of dissimilarities of the names
# Created by: quito
# Created on: 27.05.2020

source('data_preprocessing.R')

calculate_matrix <- function(plec){
    df <- DF_1000_PER_PLEC[[plec]][NAMES_300[[plec]],]
    df <- as.matrix(df)
    df <- log(df+.1)
    dissNames <- TSclust::diss(as.matrix(df), 'DTWARP')
    dissNames <- as.matrix(dissNames)
    return(dissNames)
}

dissNamesGirls <- calculate_matrix('K')
saveRDS(dissNamesGirls, file='dissNames_girls2019.rds')
dissNamesBoys <- calculate_matrix('M')
saveRDS(dissNamesBoys, file='dissNames_boys2019.rds')

#### MDS ####
mds_matrix <- function(data){
    data <- as.dist(log(data+1) + 0.00001)
    fit <- MASS::isoMDS(data,k=2)
    return(fit)
}

dissNamesGirls <- readRDS('dissNames_girls2019.rds')
fit_k <- mds_matrix(dissNamesGirls)
saveRDS(fit_k, file='mds_girls2019.rds')


dissNamesBoys <- readRDS('dissNames_boys2019.rds')
fit_m <- mds_matrix(dissNamesBoys)
saveRDS(fit_m, file='mds_boys2019.rds')
