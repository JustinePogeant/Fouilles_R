rm(list = ls())
library(kableExtra)
#- > on fixe la graine
set.seed(128943)
#-> simulation d'une distribution normale (vecteurs)
var_1 <- rnorm(100,0,4)
var_2 <- rnorm(100,3,6)
#-> simulation d'une distribution uniforme (vecteurs)
var_3 <- runif(100, min = 6, max = 10)

# méthode 1 (élégante)
seuil <- 0.5
var_4 <- ifelse(runif(100,min = 0, max = 1) <= seuil, 1, 0)

# méthode 2 (plus standard)
var_4     <- rep(0,100)
id        <- which(runif(100,min = 0, max = 1) > seuil)
var_4[id] <- 1 

df_1 <- data.frame('A' = var_1,
                   'B' = var_2,
                   'C' = var_3,
                   'D' = var_4)
# pour rappel
#ncol(df_1) # nombre colonnnes 
#nrow(df_1) # nombre de lignes
df_1$D <- factor(df_1$D , levels = c(0,1), labels = c('Oui','Non'))
class(df_1$D)
row.names(df_1) <- paste0('Patient_',1:100)

# extraction à partir des index
# attention si extractiuon d'une seule variable, le résultat est un vecteur
v_2 <- df_1[,3]
# extraction des variables A et D (le résultat est un dataframe)
df_3 <- df_1[,c(1,3)]
class(df_3)

# une seule variable
v_3 <- df_1$A
# plusieurs variables
df_5 <- data.frame(df_1$A, df_1$D)
df_6 <- df_1[c('A','D')]

# sélection de la ligne 53 le résultat est un dataframe !
df_7 <- df_1[53,] 

# sélection des la lignes 11 à 15, 38 , 40, de 70, 72, 74 , 76, 78, 80 le résultats est un dataframe !
df_8 <- df_1[c (11:15, 38,40, seq(70,80,2)), ]

# Sélection des patients 25, 28,74
df_9 <- df_1[c('Patient_25','Patient_28','Patient_74'), ]

# extraire les patients 11 à 15, 38 , 40, de 70, 72, 74 , 76, 78, 80 pour les variables A et D
df_10 <- df_1[c (11:15, 38,40, seq(70,80,2)), c(1,4)]

# Sélection des patients 25, 28,74 pour les variables A et C
df_11 <- df_1[c('Patient_25','Patient_28','Patient_74'),c('A','C') ]
df_12 <- df_1[ df_1$A > 0, ]
df_13 <- df_1[df_1$A > 0.3 & df_1$B < 2 ,]
df_14 <- df_1[df_1$A > 0.3 & df_1$B < 2 & df_1$D == 'Oui' ,]
df_14 <- df_1[df_1$A > 0.3 & df_1$B < 2 & df_1$D == 'Oui' , c('B','D')]
df_15 <- subset(df_1, A > 0.3 & B < 2 & D == 'Oui', select = c(B,D))
print("Dataframe df_15:")
print(df_15)

# moyenne par colonne margin = 2
s1 <- apply(df_1[, -4], MARGIN = 1, mean)

# moyenne par colonne margin = 2
s2 <- apply(df_1[, -4], MARGIN = 2, mean)

s2 <- sapply(df_1[, -4], mean)

# utilisation d'une fonction externe
cv <- function(x){return(sd(x, na.rm = T) / mean(x, na.rm = T) *100 )}
s3 <- sapply(df_1[-4], FUN = cv)

# utilisation d'une fonction interne
s5 <- sapply(df_1[-4], function(x){return(sd(x, na.rm = T) / mean(x, na.rm = T) *100 )})

print("Coefficient de variation des variables numériques:")
print(s3)

cv <- function(x){return(sd(x, na.rm = T) / mean(x, na.rm = T) *100 )}
s4 <- lapply(df_1[-4], FUN = cv)
s4
print("Coefficient de variation des variables numériques (avec lapply):")
print(s4)

ag_1 <- aggregate(df_1[-4], by = list(df_1$D), mean)

# concaténation en colonnes
# On crée deux dataFrame
df_AB <- data.frame('A' = var_1,'B' = var_2)
df_BC <- data.frame('C' = var_3,'D' = var_4)
df_ABCD <- cbind(df_AB, df_BC)

# concaténation en ligne
df_1_50   <- df_1[1:50,]
df_51_100 <- df_1[51:100,]
df_1_100 <- rbind(df_1_50, df_51_100)
# attention il est nécessaire les noms des variables soient identiques sinon cela ne fonctionne pas !

# modifier tous les noms 
names(df_1) <- c('VAR_1','VAR_2','VAR_3','VAR_4')

# modifier le nom de la 3 ième variable
names(df_1)[3] <- 'VAR_999'
print("Noms des variables modifiés:")
print(names(df_1))

# variable numériques
id_num <- which(sapply(df_1, is.numeric))
# puis les extraire 
df_num <- df_1[,id_num]

# variable catégorielle
id_cat <- which(sapply(df_1, is.factor))
# puis l'extraire (attention sous forme de vecteur puisqu'une seule variables !)
df_cat <- df_1[,id_cat] 

vecteur <- seq(2,10,by=3)
matrice <- matrix(1:8,ncol=2)
facteur <- factor(c("M","M","F","M","F","M","M","M"))
ordonne <- ordered(c("débutant","débutant","champion",
                     "champion","moyen","moyen","moyen","champion"),
                   levels=c("débutant","moyen","champion"))
mylist <- list(vecteur,matrice,facteur,ordonne)
mylist
