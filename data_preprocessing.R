## data source: https://dane.gov.pl/dataset/219

library(reshape2)  # dcast(),

# ZALOZENIE 1: liczba urodzen jest taka sama jak suma zliczeń imion. Jest to niedoszacowane, gdyż min(DATA$Liczba) == 2.
# A zatem można podejrzewać, że dane nie obejmuja imion nadanych tylko jednemu dziecku w danym roku.

# ZALOZENIE 2: w "mapie imion" uwzgledniam tylko te imiona, ktore na przestrzeni 20 lat zostaly nadane przynajmniej 300
# razy, czyli srednio min 15 osobom w roku.


DATA <- read.csv('Imiona_nadane_wPolsce_w_latach_2000-2019.csv', sep=',', fileEncoding = "UTF-8")
idx <- rownames(DATA[DATA$Imię %in% c('MILENA', 'NIKOLA', 'NICOLA') & DATA$Płeć == 'M',])
DATA <- DATA[-as.numeric(idx),]
idx <- rownames(DATA[DATA$Imię %in% c('ALEX', 'SZYMON', 'DANIEL', 'FABIAN', 'ARIEL') & DATA$Płeć == 'K',])
DATA <- DATA[-as.numeric(idx),]
counts <- aggregate(Liczba ~ Imię + Płeć, DATA, sum)
NAMES <- as.character(counts$Imię[counts$Liczba > 300])
data <- DATA[DATA$Imię %in% NAMES,]




#########################################################
## IMIE_ROK_COUNTS_PER_PLEC
#########################################################

#> str(IMIE_ROK_COUNTS_PER_PLEC)
# List of 2
#  $ K:'data.frame':      1800 obs. of  21 variables:
#   ..$ Imię: Factor w/ 3602 levels "AADYA","AAHANA",..: 1 2 3 4 9 10 11 16 17 19 ...
#   ..$ 2000: num [1:1800] 0 0 0 0 0 0 0 0 8 0 ...
#   ..$ 2001: num [1:1800] 0 0 0 0 0 0 0 0 6 0 ...
# ...
#   ..$ 2019: num [1:1800] 0 0 0 3 0 0 0 0 28 0 ...
#  $ M:'data.frame':      1838 obs. of  21 variables:
#   ..$ Imię: Factor w/ 3602 levels "AADYA","AAHANA",..: 5 6 7 8 12 13 14 15 18 20 ...
#   ..$ 2000: num [1:1838] 9 0 0 0 0 0 0 0 0 0 ...
#   ..$ 2001: num [1:1838] 7 0 0 0 0 0 0 0 0 0 ...
# ...
#   ..$ 2019: num [1:1838] 39 0 0 0 0 0 2 0 0 2 ...

k <- split(DATA, DATA$Płeć)
imie_rok_counts_per_plec <- lapply(k, function(df)  dcast(Imię ~ Rok, data=df, value.var = 'Liczba'))
imie_rok_counts_per_plec[['K']][is.na(imie_rok_counts_per_plec[['K']])] <- 0
imie_rok_counts_per_plec[['M']][is.na(imie_rok_counts_per_plec[['M']])] <- 0
rownames(imie_rok_counts_per_plec[['K']]) <- imie_rok_counts_per_plec[['K']][,1]
rownames(imie_rok_counts_per_plec[['M']]) <- imie_rok_counts_per_plec[['M']][,1]
imie_rok_counts_per_plec[['M']] <- imie_rok_counts_per_plec[['M']][, -1]
imie_rok_counts_per_plec[['K']] <- imie_rok_counts_per_plec[['K']][, -1]
IMIE_ROK_COUNTS_PER_PLEC <- imie_rok_counts_per_plec

#########################################################
##ROK_PLEC_COUNTS
#########################################################

# > head(ROK_PLEC_COUNTS, 3)
#           K      M
# 2000 187142 197256
# 2001 181975 191947
# 2002 175590 185415
# ...

ROK_PLEC_COUNTS <- sapply(IMIE_ROK_COUNTS_PER_PLEC, function(i) colSums(i, na.rm=TRUE))

#########################################################
##DF_1000_PER_PLEC
#########################################################

# > str(DF_1000_PER_PLEC)
# List of 2
#  $ K: num [1:1800, 1:20] 0 0 0 0 0 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:1800] "AADYA" "AAHANA" "AALIA" "AALIYAH" ...
#   .. ..$ : chr [1:20] "2000" "2001" "2002" "2003" ...
#  $ M: num [1:1838, 1:20] 0.0456 0 0 0 0 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:1838] "AARON" "AARUSH" "AARYAN" "AAYAN" ...
#   .. ..$ : chr [1:20] "2000" "2001" "2002" "2003" ...


#Liczba nadanych imion na 1000 urodzen
k <- IMIE_ROK_COUNTS_PER_PLEC[['K']]
m <- IMIE_ROK_COUNTS_PER_PLEC[['M']]
df_100_k = t(t(k)/colSums(k, na.rm=TRUE)*100)
df_100_m = t(t(m)/colSums(m, na.rm=TRUE)*100)

##### test #####
# k['ŻANETA', '2017'] = 33
# colSums_2019 = 188557 urodzen dziewczynek.
k['ŻANETA', '2017'] / colSums(k, na.rm=TRUE)['2017'] * 100 == df_100_k['ŻANETA', '2017']
##### koniec testu ######

DF_100_PER_PLEC <- list()
DF_100_PER_PLEC[['K']] <- as.data.frame(df_100_k)
DF_100_PER_PLEC[['M']] <- as.data.frame(df_100_m)

#########################################################
## NAMES_300
#########################################################

names_counts <- lapply(IMIE_ROK_COUNTS_PER_PLEC, rowSums)
NAMES_300 <- lapply(names_counts, function(x) names(x[x>300]))