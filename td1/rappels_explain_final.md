> AP-4209 ESIEE-Paris: 2023 -2024\
> Fouille de données avec R pour la data science et l'intelligence artificielle \
> --TD 1: Rappels--\
> Author : Badr TAJINI -- ESIEE Paris

---

### Fonction #1
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### Explication
Ce bloc de code R est utilisé pour configurer les options globales pour les blocs de code R dans un document R Markdown. Ici, `knitr::opts_chunk$set(echo = TRUE)` est utilisé pour s'assurer que le code R dans les blocs suivants sera affiché dans le document final, en plus des résultats.

---
### Fonction #2
```{r}
rm(list = ls())
library(kableExtra)
# -> on fixe la graine
set.seed(128943)
# -> simulation d'une distribution normale (vecteurs)
var_1 <- rnorm(100,0,4)
var_2 <- rnorm(100,3,6)
# -> simulation d'une distribution uniforme (vecteurs)
var_3 <- runif(100, min = 6, max = 10)
```
### Explication
Ce code R effectue plusieurs opérations :
1. `rm(list = ls())` efface toutes les variables existantes dans l'environnement.
2. `library(kableExtra)` charge la bibliothèque kableExtra, utilisée pour créer des tableaux avancés.
3. `set.seed(128943)` fixe la graine pour la génération de nombres aléatoires, ce qui garantit la reproductibilité des résultats.
4. `rnorm(100,0,4)` et `rnorm(100,3,6)` génèrent deux vecteurs de 100 valeurs chacun, suivant une distribution normale avec des moyennes et écarts-types différents.
5. `runif(100, min = 6, max = 10)` génère un vecteur de 100 valeurs suivant une distribution uniforme entre 6 et 10.


---
### Fonction #3
```{r}
# méthode 1 (élégante)
seuil <- 0.5
var_4 <- ifelse(runif(100, min = 0, max = 1) <= seuil, 1, 0)

# méthode 2 (plus standard)
var_4     <- rep(0,100)
id        <- which(runif(100, min = 0, max = 1) > seuil)
var_4[id] <- 1 
```
### Explication
Ce code R illustre deux méthodes pour générer un vecteur binaire (contenant des 0 et des 1) de 100 éléments :
1. La première méthode utilise `ifelse` avec `runif` pour assigner 1 si la valeur générée aléatoirement est inférieure ou égale à 0.5, sinon 0.
2. La deuxième méthode crée un vecteur de 100 zéros (`rep(0,100)`) et remplace certains de ces zéros par des 1. Les indices où les valeurs sont remplacées sont déterminés par `which(runif(100, min = 0, max = 1) > seuil)`. Cela sélectionne les emplacements où les valeurs aléatoires sont supérieures à 0.5.

---
### Fonction #4
```{r}
df_1 <- data.frame('A' = var_1,
                   'B' = var_2,
                   'C' = var_3,
                   'D' = var_4)
# pour rappel
#ncol(df_1) # nombre colonnnes 
#nrow(df_1) # nombre de lignes
```
### Explication
Ce bloc de code crée un dataframe `df_1` à partir des vecteurs `var_1`, `var_2`, `var_3`, et `var_4` créés précédemment. Chaque vecteur devient une colonne dans `df_1`. Les commentaires `#ncol(df_1)` et `#nrow(df_1)` sont des rappels pour compter le nombre de colonnes et de lignes du dataframe, mais ils sont commentés et donc pas exécutés.

---
### Fonction #5
```{r, echo=F}
df_1 %>% kbl(digits=3) %>%  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", latex_options = 'stripped') %>% scroll_box(height = "250px")
```
### Explication
Ce code utilise la syntaxe de `dplyr` et `kableExtra` pour formater et afficher le dataframe `df_1`. 
- `kbl(digits=3)` crée une table avec trois chiffres après la virgule.
- `kable_styling` avec les options indiquées ajuste le style de la table : rayée (`striped`), pas en pleine largeur (`full_width = F`), centrée (`position = "center"`), et avec certaines options LaTeX (`latex_options = 'stripped'`).
- `scroll_box(height = "250px")` place la table dans une boîte défilante avec une hauteur fixe.

---
### Fonction #6
```{r}
df_1$D <- factor(df_1$D , levels = c(0,1), labels = c('Oui','Non'))
```
### Explication
Ce code transforme la colonne `D` du dataframe `df_1` en un facteur avec deux niveaux. Les valeurs 0 et 1 sont remplacées respectivement par les étiquettes 'Oui' et 'Non'. Cela est utile pour les analyses statistiques et les visualisations qui traitent des données catégorielles.

---
### Fonction #7
```{r}
class(df_1$D)
```
### Explication
Cette ligne de code vérifie la classe de la colonne `D` du dataframe `df_1`. Cela sert à confirmer que la colonne `D` est bien un facteur après la transformation dans le code précédent.

---
### Fonction #8
```{r}
row.names(df_1) <- paste0('Patient_', 1:100)
```
### Explication
Ici, le code attribue des noms de lignes personnalisés au dataframe `df_1`. Chaque ligne est nommée 'Patient_' suivi d'un numéro (de 1 à 100), ce qui pourrait être utile pour identifier des enregistrements spécifiques dans le dataframe, en particulier dans un contexte médical ou clinique.

---
### Fonction #9
```{r, echo = F}
df_1 %>% kbl(digits=3) %>%  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", latex_options = 'stripped') %>% scroll_box(height = "250px")
```
### Explication
Ce code semble être une répétition de la fonction #5. Il utilise les mêmes fonctions `kbl`, `kable_styling`, et `scroll_box` pour formater et afficher le dataframe `df_1`.

---
### Fonction #10
```{r}
# extraction à partir des index
# attention si extraction d'une seule variable, le résultat est un vecteur
v_2 <- df_1[,3]
# extraction des variables A et D (le résultat est un dataframe)
df_3 <- df_1[,c(1,4)]
class(df_3)
```
### Explication
Ce bloc de code montre comment extraire des données d'un dataframe :
- `v_2 <- df_1[,3]` extrait la troisième colonne de `df_1` (var_3) et la stocke dans `v_2`. Comme il s'agit d'une seule colonne, le résultat est un vecteur.
- `df_3 <- df_1[,c(1,4)]` extrait les colonnes 1 (var_1) et 4 (var_4, transformée en facteur) de `df_1` et crée un nouveau dataframe `df_3`.
- `class(df_3)` vérifie la classe de `df_3`, qui devrait être un dataframe.

---
### Fonction #11
```{r}
# une seule variable
v_3 <- df_1$A
# plusieurs variables
df_5 <- data.frame(df_1$A, df_1$D)
```
### Explication
Ce code montre deux autres méthodes d'extraction de données :
- `v_3 <- df_1$A` extrait la colonne 'A' de `df_1` dans un vecteur `v_3`.
- `df_5 <- data.frame(df_1$A, df_1$D)` crée un nouveau dataframe `df_5` en combinant les colonnes 'A' et 'D' de `df_1`.

---

### Fonction #12
```{r}
df_6 <- df_1[c('A','D')]
```
### Explication
Ce code crée un nouveau dataframe `df_6` en extrayant les colonnes 'A' et 'D' de `df_1`. C'est une autre méthode pour sélectionner des colonnes spécifiques d'un dataframe en utilisant leurs noms.

---
### Fonction #13
```{r}
# sélection de la ligne 53 le résultat est un dataframe !
df_7 <- df_1[53,] 

# sélection des la lignes 11 à 15, 38 , 40, de 70, 72, 74 , 76, 78, 80 le résultat est un dataframe !
df_8 <- df_1[c(11:15, 38, 40, seq(70, 80, 2)), ]
```
### Explication
- `df_7 <- df_1[53,]` sélectionne la 53ème ligne de `df_1` et la stocke dans `df_7`. Le résultat est un dataframe contenant une seule ligne.
- `df_8 <- df_1[c(11:15, 38, 40, seq(70, 80, 2)), ]` sélectionne un ensemble spécifique de lignes (les lignes 11 à 15, 38, 40, et toutes les deux lignes de 70 à 80) de `df_1` et les stocke dans `df_8`.

---
### Fonction #14
```{r}
# Sélection des patients 25, 28, 74
df_9 <- df_1[c('Patient_25','Patient_28','Patient_74'), ]
```
### Explication
Cette ligne de code sélectionne les lignes nommées 'Patient_25', 'Patient_28' et 'Patient_74' de `df_1` et les stocke dans `df_9`. Cette sélection est basée sur les noms de lignes personnalisés définis précédemment.

---

### Fonction #15
```{r}
# extraire les patients 11 à 15, 38 , 40, de 70, 72, 74 , 76, 78, 80 pour les variables A et D
df_10 <- df_1[c(11:15, 38, 40, seq(70, 80, 2)), c(1, 4)]
```
### Explication
Ce code crée `df_10` en sélectionnant des lignes et colonnes spécifiques de `df_1`. Les lignes choisies sont les 11 à 15, 38, 40 et de 70 à 80 par pas de 2, et les colonnes choisies sont la 1ère (A) et la 4ème (D). Cette opération résulte en un sous-ensemble de `df_1` contenant les données spécifiées.

---
### Fonction #16
```{r}
# Sélection des patients 25, 28, 74 pour les variables A et C
df_11 <- df_1[c('Patient_25', 'Patient_28', 'Patient_74'), c('A', 'C')]
```
### Explication
Ici, `df_11` est créé en extrayant des lignes et colonnes spécifiques de `df_1` en utilisant les noms de lignes et de colonnes. Les lignes sélectionnées sont celles nommées 'Patient_25', 'Patient_28', et 'Patient_74', et les colonnes sélectionnées sont 'A' et 'C'.

---
### Fonction #17
```{r}
df_12 <- df_1[df_1$A > 0, ]
```
### Explication
Ce code crée `df_12` en sélectionnant toutes les lignes de `df_1` où la valeur dans la colonne 'A' est supérieure à 0. C'est un exemple de filtrage conditionnel dans un dataframe.

---
### Fonction #18
```{r}
df_13 <- df_1[df_1$A > 0.3 & df_1$B < 2, ]
```
### Explication
Dans ce cas, `df_13` est créé en appliquant un filtrage plus complexe. Seules les lignes où la valeur dans la colonne 'A' est supérieure à 0.3 et celle dans la colonne 'B' est inférieure à 2 sont sélectionnées.

---

### Fonction #19
```{r}
df_14 <- df_1[df_1$A > 0.3 & df_1$B < 2 & df_1$D == 'Oui', ]
```
### Explication
Ce code crée `df_14` en sélectionnant les lignes de `df_1` qui répondent à plusieurs conditions : la valeur dans la colonne 'A' doit être supérieure à 0.3, celle dans la colonne 'B' doit être inférieure à 2, et celle dans la colonne 'D' doit être égale à 'Oui'. Cela illustre un filtrage plus avancé basé sur plusieurs critères.

---
### Fonction #20
```{r}
df_14 <- df_1[df_1$A > 0.3 & df_1$B < 2 & df_1$D == 'Oui', c('B', 'D')]
```
### Explication
Ce code est une variation de la fonction précédente. Il sélectionne non seulement les lignes qui répondent aux mêmes conditions, mais limite également les colonnes à 'B' et 'D'. Ainsi, `df_14` contient uniquement les colonnes 'B' et 'D' des lignes qui répondent aux critères.

---
### Fonction #21
```{r}
df_15 <- subset(df_1, A > 0.3 & B < 2 & D == 'Oui', select = c(B, D))
```
### Explication
Ce code utilise la fonction `subset` pour effectuer une opération similaire à celle de la fonction #20. Il crée `df_15` en sélectionnant les lignes de `df_1` où 'A' > 0.3, 'B' < 2, et 'D' == 'Oui', et ne conserve que les colonnes 'B' et 'D'. `subset` est une autre façon de filtrer et de sélectionner des données dans R.

---
### Fonction #22
```{r}
# moyenne par colonne margin = 2
s1 <- apply(df_1[, -4], MARGIN = 1, mean)

# moyenne par colonne margin = 2
s2 <- apply(df_1[, -4], MARGIN = 2, mean)
```
### Explication
Ce code utilise la fonction `apply` pour calculer des moyennes :
- `s1 <- apply(df_1[, -4], MARGIN = 1, mean)` calcule la moyenne de chaque ligne (`MARGIN = 1`) de `df_1`, en excluant la quatrième colonne (`df_1[, -4]`).
- `s2 <- apply(df_1[, -4], MARGIN = 2, mean)` calcule la moyenne de chaque colonne (`MARGIN = 2`) de `df_1`, en excluant également la quatrième colonne.

---

### Fonction #23
```{r}
s2 <- sapply(df_1[, -4], mean)
```
### Explication
Ce code calcule la moyenne de chaque colonne (sauf la 4ème) de `df_1` en utilisant `sapply`. `sapply` applique la fonction `mean` à chaque colonne sélectionnée et renvoie un vecteur de moyennes.

---
### Fonction #24
```{r}
# utilisation d'une fonction externe
cv <- function(x) { return(sd(x, na.rm = T) / mean(x, na.rm = T) * 100) }
s3 <- sapply(df_1[-4], FUN = cv)

# utilisation d'une fonction interne
s5 <- sapply(df_1[-4], function(x) { return(sd(x, na.rm = T) / mean(x, na.rm = T) * 100) })
```
### Explication
Ces lignes de code définissent une fonction `cv` pour calculer le coefficient de variation (écart-type divisé par la moyenne) et l'appliquent à toutes les colonnes de `df_1` (sauf la 4ème) :
- `s3` utilise une fonction nommée (`cv`).
- `s5` utilise une fonction anonyme qui fait la même opération.

---
### Fonction #25
```{r}
cv <- function(x) { return(sd(x, na.rm = T) / mean(x, na.rm = T) * 100) }
s4 <- lapply(df_1[-4], FUN = cv)
s4
```
### Explication
Similaire à la fonction précédente, mais utilise `lapply` au lieu de `sapply`. `lapply` renvoie une liste au lieu d'un vecteur.

---
### Fonction #26
```{r}
ag_1 <- aggregate(df_1[-4], by = list(df_1$D), mean)
```
### Explication
Ce code utilise `aggregate` pour calculer la moyenne des colonnes de `df_1` (sauf la 4ème) regroupées par la colonne 'D'.

---
### Fonction #27
```{r}
ag_2 <- aggregate(df_1[-4], by = list(df_1$D), function(x) { return(sd(x, na.rm = T) / mean(x, na.rm = T) * 100) })
```
### Explication
Similaire à `ag_1`, mais calcule le coefficient de variation pour chaque groupe défini par 'D'.

---
### Fonction #28
```{r}
# concaténation en colonnes
df_AB <- data.frame('A' = var_1, 'B' = var_2)
df_BC <- data.frame('C' = var_3, 'D' = var_4)
df_ABCD <- cbind(df_AB, df_BC)

# concaténation en ligne
df_1_50 <- df_1[1:50,]
df_51_100 <- df_1[51:100,]
df_1_100 <- rbind(df_1_50, df_51_100)
```
### Explication
Ce code montre comment concaténer des dataframes :
- `cbind` pour la concaténation en colonnes (`df_AB` et `df_BC`).
- `rbind` pour la concaténation en lignes (`df_1_50` et `df_51_100`).

---
### Fonction #29
```{r}
# modifier tous les noms 
names(df_1) <- c('VAR_1', 'VAR_2', 'VAR_3', 'VAR_4')

# modifier le nom de la 3ème variable
names(df_1)[3] <- 'VAR_999'
```
### Explication
Ce code renomme les colonnes de `df_1` :
- La première ligne remplace tous les noms de colonnes.
- La deuxième ligne modifie spécifiquement le nom de la troisième colonne.

---
### Fonction #30
```r
# variable numériques
id_num <- which(sapply(df_1, is.numeric))
# puis les extraire 
df_num <- df_1[,id_num]

# variable catégorielle
id_cat <- which(sapply(df_1, is.factor))
# puis l'extraire (attention sous forme de vecteur puisqu'une seule variables !)
df_cat <- df_1[,id_cat] 
```
### Explication
Ce bloc de code R effectue une opération de sélection et d'extraction de variables sur un dataframe `df_1`. 

- **Variables numériques**: 
  - `id_num <- which(sapply(df_1, is.numeric))`: Cette ligne trouve les indices des colonnes du dataframe `df_1` qui sont numériques. La fonction `sapply` applique la fonction `is.numeric` à chaque colonne de `df_1` pour vérifier si elles sont numériques.
  - `df_num <- df_1[,id_num]`: Cette ligne extrait les colonnes numériques en utilisant les indices trouvés précédemment et les stocke dans un nouveau dataframe `df_num`.

- **Variables catégorielles**:
  - `id_cat <- which(sapply(df_1, is.factor))`: Ici, on trouve les indices des colonnes du dataframe `df_1` qui sont des facteurs (variables catégorielles).
  - `df_cat <- df_1[,id_cat]`: Cette ligne extrait les colonnes catégorielles en utilisant les indices trouvés et les stocke dans un nouveau dataframe `df_cat`. La mention "(attention sous forme de vecteur puisqu'une seule variables !)" indique que si une seule variable catégorielle est présente, le résultat sera un vecteur plutôt qu'un dataframe.

---

### Fonction #31
```r
vecteur <- seq(2,10,by=3)
matrice <- matrix(1:8,ncol=2)
facteur <- factor(c("M","M","F","M","F","M","M","M"))
ordonne <- ordered(c("débutant","débutant","champion",
                     "champion","moyen","moyen","moyen","champion"),
                   levels=c("débutant","moyen","champion"))
mylist <- list(vecteur,matrice,facteur,ordonne)
mylist
```
### Explication
Ce bloc de code crée plusieurs structures de données en R et les stocke dans une liste.

- `vecteur <- seq(2,10,by=3)`: Crée un vecteur avec une séquence de nombres de 2 à 10, avec un pas de 3.
- `matrice <- matrix(1:8, ncol=2)`: Crée une matrice de 2 colonnes contenant les nombres de 1 à 8.
- `facteur <- factor(c("M","M","F","M","F","M","M","M"))`: Crée un facteur, une structure de données utilisée pour les variables catégorielles, avec des niveaux "M" (Masculin) et "F" (Féminin).
- `ordonne <- ordered(c("débutant","débutant","champion", "champion","moyen","moyen","moyen","champion"), levels=c("débutant","moyen","champion"))`: Crée une variable ordinale, similaire à un facteur, mais avec un ordre explicite entre les niveaux.
- `mylist <- list(vecteur, matrice, facteur, ordonne)`: Stocke toutes ces structures dans une liste nommée `mylist`.

---

### Fonction #32
```r
mat <- mylist[[2]]
mat
```
### Explication
Ce court morceau de code extrait le deuxième élément de la liste `mylist` (qui est la matrice créée dans le bloc de code précédent) et le stocke dans la variable `mat`. La syntaxe `[[2]]` est utilisée pour accéder à un élément spécifique d'une liste en R.

---

### Fonction #33
```r
names(mylist)[[1]] <- 'vecteur'
names(mylist)[[2]] <- 'matrice'
names(mylist)[[3]] <- 'facteur'
names(mylist)[[4]] <- 'ordre'
mylist
```
### Explication
Ce bloc de code attribue des noms aux éléments de la liste `mylist`.

- `names(mylist)[[1]] <- 'vecteur'`: Attribue le nom 'vecteur' au premier élément de la liste.
- `names(mylist)[[2]] <- 'matrice'`: Attribue le nom 'matrice' au deuxième élément de la liste.
- `names(mylist)[[3]] <- 'facteur'`: Attribue le nom 'facteur' au troisième élément de la liste.
- `names(mylist)[[4]] <- 'ordre'`: Attribue le nom 'ordre' au quatrième élément de la liste.
- `mylist`: Affiche la liste `mylist` avec les noms attribués à chaque élément.

---

### Fonction #34
```r
mylist$vecteur
```
### Explication
Ce code accède et affiche l'élément nommé 'vecteur' dans la liste `mylist`. En R, `$` est utilisé pour accéder aux éléments d'une liste (ou d'un dataframe) par leur nom.

---


### Fonction #35
```r
r <-  runif(10, min = 3, max = 12)
mylist[['rand']] <- r
mylist
```
### Explication
Ce bloc de code ajoute un nouvel élément à la liste `mylist`.

- `r <- runif(10, min = 3, max = 12)`: Génère un vecteur de 10 nombres aléatoires uniformément répartis entre 3 et 12.
- `mylist[['rand']] <- r`: Ajoute ce vecteur à `mylist` sous le nom 'rand'.
- `mylist`: Affiche la liste `mylist` mise à jour avec le nouvel élément.

---

### Fonction #36
```r
# pour l'exemple nous créons la même liste que précédemment mais en prenant de nousveau nom
mylist2 <- list('L_1' =  vecteur, 'L_2' = matrice, 'L_3' = facteur, 'L_4' = ordonne)
```
### Explication
Ce code crée une nouvelle liste, `mylist2`, en utilisant les mêmes éléments que dans `mylist` mais avec des noms différents.

- `mylist2 <- list('L_1' = vecteur, 'L_2' = matrice, 'L_3' = facteur, 'L_4' = ordonne)`: Crée une liste avec les éléments 'vecteur', 'matrice', 'facteur', et 'ordre', mais en les nommant respectivement 'L_1', 'L_2', 'L_3', et 'L_4'.

---


### Fonction #37
```r
# on crée une liste vide
mylist3 <- list()
#... que l'on remplit progressivement
mylist3[['A']] <-  seq(2,10,by=3)
mylist3[['B']] <-  matrix(1:8,ncol=2)
mylist3[['C']] <-  factor(c("M","M","F","M","F","M","M","M"))
mylist3[['D']] <-  ordered(c("débutant","débutant","champion",
                     "champion","moyen","moyen","moyen","champion"),
                   levels=c("débutant","moyen","champion"))
```
### Explication
Ce code crée et remplit progressivement une nouvelle liste, `mylist3`.

- `mylist3 <- list()`: Initialise une liste vide nommée `mylist3`.
- `mylist3[['A']] <- seq(2,10,by=3)`: Ajoute à `mylist3` un vecteur séquentiel (de 2 à 10 par pas de 3) sous le nom 'A'.
- `mylist3[['B']] <- matrix(1:8, ncol=2)`: Ajoute à `mylist3` une matrice 2x4 sous le nom 'B'.
- `mylist3[['C']] <- factor(c("M","M","F","M","F","M","M","M"))`: Ajoute à `mylist3` un facteur (variable catégorielle) sous le nom 'C'.
- `mylist3[['D']] <- ordered(c("débutant","débutant","champion", "champion","moyen","moyen","moyen","champion"), levels=c("débutant","moyen","champion"))`: Ajoute à `mylist3` une variable ordinale sous le nom 'D'.

---

### Fonction #38
```r
mylist4 <- as.list(df_1)
names(mylist4)
```
### Explication
Ce code convertit un dataframe en liste et affiche les noms de ses éléments.

- `mylist4 <- as.list(df_1)`: Convertit le dataframe `df_1` en liste, stockée dans `mylist4`.
- `names(mylist4)`: Affiche les noms des éléments de la liste `mylist4`. Ces noms correspondent aux noms des colonnes dans le dataframe original `df_1`.

---


### Fonction #39
```r
# extraction
mat <- mylist$matrice
# conversion
df <- data.frame(mat)
# affectation des noms lignes et colonnes
names(df) <- c('A','B') ; rownames(df) <- paste0('P_',1:nrow(df) )
# intégrer dans le dataframe dans la liste
mylist[['mydata']] <- df
# effacer l'élément matrice
mylist$matrice <- NULL

mylist
```
### Explication 
Ce bloc de code manipule des données à l'intérieur de la liste `mylist` et modifie sa structure.

- `mat <- mylist$matrice`: Extrait l'élément 'matrice' de `mylist` et le stocke dans `mat`.
- `df <- data.frame(mat)`: Convertit `mat` en un dataframe nommé `df`.
- `names(df) <- c('A','B') ; rownames(df) <- paste0('P_',1:nrow(df))`: Attribue les noms 'A' et 'B' aux colonnes de `df` et nomme les lignes 'P_1', 'P_2', etc.
- `mylist[['mydata']] <- df`: Ajoute `df` à `mylist` sous le nom 'mydata'.
- `mylist$matrice <- NULL`: Supprime l'élément 'matrice' de `mylist`.
- `mylist`: Affiche la liste `mylist` mise à jour.

---

### Fonction #40
```r
res1 <- mylist$mydata[c(2:4),2]
# ou bien
res2 <- mylist[['mydata']][c(2:4),2]
# ou bien
res3 <- mylist[['mydata']][c('P_2','P_3','P_4'),c('B')]
# les résultats sont identiques
```
### Explication
Ce code illustre différentes manières d'extraire des données spécifiques d'un dataframe stocké dans une liste.

- `res1 <- mylist$mydata[c(2:4),2]`: Extrait les éléments des lignes 2 à 4 et de la colonne 2 du dataframe 'mydata' dans `mylist`, et les stocke dans `res1`.
- `res2 <- mylist[['mydata']][c(2:4),2]`: Fait la même chose que `res1`, mais utilise une autre syntaxe pour accéder au dataframe.
- `res3 <- mylist[['mydata']][c('P_2','P_3','P_4'),c('B')]`: Extrait les mêmes éléments en utilisant les noms des lignes et de la colonne, au lieu de leurs indices.

---

### Fonction #41
```r
n <- length(mylist[['facteur']] == 'M') ; n
```
### Explication
Ce code compte le nombre d'occurrences de la valeur 'M' dans l'élément 'facteur' de la liste `mylist`.

- `n <- length(mylist[['facteur']] == 'M')`: Évalue l'expression `mylist[['facteur']] == 'M'`, qui renvoie un vecteur logique indiquant où les éléments de 'facteur' sont égaux à 'M'. La fonction `length` compte ensuite le nombre d'éléments dans ce vecteur logique.
- `n`: Affiche la valeur de `n`, qui représente le nombre de fois que 'M' apparaît dans 'facteur'.

---

### Fonction #42
```r
# l'écriture est un peu plus compliquée;  attention à bien positionner les crochets !
temp <- mylist[['mydata']][ mylist[['mydata']]$B > 6,] ; temp
```
### Explication
Ce code extrait des lignes spécifiques d'un dataframe stocké dans la liste `mylist` en se basant sur une condition.

- `temp <- mylist[['mydata']][ mylist[['mydata']]$B > 6,]`: Sélectionne les lignes du dataframe 'mydata' de `mylist` où les valeurs de la colonne 'B' sont supérieures à 6. Ces lignes sont ensuite stockées dans la variable `temp`.
- `temp`: Affiche le contenu de `temp`, qui est un sous-ensemble de 'mydata'.

---


### Fonction #43
```r
set.seed(1234)
var_6 <- ifelse(runif(100,min = 0, max = 1) <= 0.5 , 1, 0)
var_7 <- ifelse(runif(100,min = 10, max = 50) <= 35, 0, 1)

df_cat <- data.frame('Diag'    = factor(var_6, levels = c(0,1), labels = c('OUI','NON' )),
                     'Statut'  = factor(var_7, levels = c(1,0), labels = c('M','NM' ))
                    )

# dénombrent variable Diag
table(df_cat$Diag)
# proportions correspondantes
prop.table(table(df_cat$Diag))

# dénombrent variable Statut
table(df_cat$Statut)
# proportions correspondantes
prop.table(table(df_cat$Statut))
```
### Explication
Ce bloc de code génère des données aléatoires et effectue des calculs statistiques.

- `set.seed(1234)`: Fixe la graine pour la génération de nombres aléatoires, assurant la reproductibilité.
- `var_6` et `var_7`: Créent deux vecteurs de 100 éléments générés aléatoirement et les convertissent en 0 et 1 basés sur une condition.
- `df_cat`: Crée un dataframe avec deux variables catégorielles 'Diag' et 'Statut' à partir de `var_6` et `var_7`.
- `table(df_cat$Diag)` et `table(df_cat$Statut)`: Calculent le dénombrement des niveaux pour chaque variable catégorielle.
- `prop.table(table(df_cat$Diag))` et `prop.table(table(df_cat$Statut))`: Calculent les proportions correspondantes pour chaque niveau des variables catégorielles.

---

### Fonction #44
```r
table(df_cat$Diag, df_cat$Statut )
# calcul en fréquence
prop.table(table(df_cat$Diag, df_cat$Statut ))
```
### Explication
Ce code effectue une analyse croisée des variables 'Diag' et 'Statut' dans `df_cat`.

- `table(df_cat$Diag, df_cat$Statut)`: Crée un tableau croisé entre 'Diag' et 'Statut', montrant la fréquence de chaque combinaison de leurs niveaux.
- `prop.table(table(df_cat$Diag, df_cat$Statut))`: Calcule les fréquences relatives pour le tableau croisé, montrant la proportion de chaque combinaison dans l'ensemble des données.

---



### Fonction #45
```r
prop.table(table(df_cat$Diag, df_cat$Statut), margin = 1)
```
### Explication
Ce code calcule les proportions conditionnelles dans un tableau de contingence pour les variables 'Diag' et 'Statut' de `df_cat`.

- `prop.table(table(df_cat$Diag, df_cat$Statut), margin = 1)`: Applique `prop.table` sur le tableau croisé de 'Diag' et 'Statut' avec l'argument `margin = 1`. Cela calcule les proportions de chaque niveau de 'Statut' pour chaque niveau de 'Diag' séparément.

---

### Fonction #46
```r
tab <- prop.table(table(df_cat$Diag, df_cat$Statut ))
tab['OUI','M']
```
### Explication
Ce code extrait une proportion spécifique du tableau de contingence pour 'Diag' et 'Statut'.

- `tab <- prop.table(table(df_cat$Diag, df_cat$Statut ))`: Crée un tableau de proportions pour le tableau croisé de 'Diag' et 'Statut'.
- `tab['OUI','M']`: Extrait la proportion des observations où 'Diag' est 'OUI' et 'Statut' est 'M' dans le tableau `tab`.

---



