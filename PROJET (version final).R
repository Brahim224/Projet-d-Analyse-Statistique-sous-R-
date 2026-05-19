#ETAPE1:FORMULATION DE LA PROBLEMATIQUE
# Il s'agit d'une etude statistique sur l'employabilite des etudiants.
#Problematique :Quels sont les facteurs académiques, professionnels et personnels 
#qui influencent significativement l’employabilité des étudiants d’une école
#,mesurée par l’obtention d’un emploi dans les six mois suivant l’obtention du diplôme ?

#ETAPE2:COLLECTE DE DONNEES
# Nous exportons nos donnees depuis le fichier excel "Questionnaire  (réponses).xlsx"
library(readxl)
data <- read_excel("C:/Users/PC/Downloads/Questionnaire  (réponses).xlsx")
View(data)

# Nous renommons lescolonnes pour plus de clarté
colnames(data) <- c("Horodateur","Score","Age","Genre","Filiere","Niveau",
                    "Stage","NbStages","DureeStage","ResultatsInfluence","item1",
                    "item2","item3","item4",
                    "item5","item6","AutoFormation","ContratPrefere")
# Filtrage :Nous ciblons les etudiants des filieres GI et GIND en 1ère et 2ème année
data<- data[(data$Filiere %in% c("GIND", "GI")) & (data$Niveau %in% c("1ère année", "2ème année")), ]
#ETAPE3:PRETRAITEMENT
#3.1:Conversion et Codification
#3.1.1. Conversion des donnees
if(!is.character(data$Genre)){
  data$Genre=as.character(data$Genre)
}
data$Genre=as.factor(data$Genre)
if(!is.character(data$DureeStage)){
  data$DureeStage=as.character(data$DureeStage)
}
data$DureeStage=as.factor(data$DureeStage)
if(!is.character(data$Filiere)){
  data$Filiere=as.character(data$Filiere)
}
data$Filiere=as.factor(data$Filiere)
if(!is.character(data$Niveau)){
  data$Niveau=as.character(data$Niveau)
}
data$Niveau=as.factor(data$Niveau)
if(!is.character(data$Stage)){
  data$Stage=as.character(data$Stage)
}
data$Stage=as.factor(data$Stage)
if(!is.character(data$ResultatsInfluence)){
  data$ResultatsInfluence=as.character(data$ResultatsInfluence)
}
data$ResultatsInfluence=as.factor(data$ResultatsInfluence)
if(!is.character(data$item1)){
  data$item1=as.character(data$item1)
}
data$item1=as.factor(data$item1)
if(!is.character(data$item2)){
  data$item2=as.character(data$item2)
}
data$item2=as.factor(data$item2)
if(!is.character(data$item3)){
  data$item3=as.character(data$item3)
}
data$item3=as.factor(data$item3)
if(!is.character(data$item4)){
  data$item4=as.character(data$item4)
}
data$item4=as.factor(data$item4)
if(!is.character(data$item5)){
  data$item5=as.character(data$item5)
}
data$item5=as.factor(data$item5)
if(!is.character(data$item6)){
  data$item6=as.character(data$item6)
}
data$item6=as.factor(data$item6)
if(!is.character(data$AutoFormation)){
  data$AutoFormation=as.character(data$AutoFormation)
}
data$AutoFormation=as.factor(data$AutoFormation)
if(!is.character(data$Autofor)){
  data$ContratPrefere=as.character(data$ContratPrefere)
}
data$ContratPrefere=as.factor(data$ContratPrefere)
if(!is.numeric(data$Age)){
  data$Age=as.numeric(data$Age)
}
if(!is.numeric(data$Score)){
  data$Score=as.numeric(data$Score)
}
if(!is.numeric(data$NbStages)){
  data$NbStages=as.numeric(data$NbStages)
}

#3.1.2. Conversion des donnees et codification
recode <- c("Pas du tout d'accord" = 1,"Pas d'accord" = 2,"Neutre"=3,
            "D'accord" = 4,"Tout a fait d'accord" = 5)
data$item1 <- recode[data$item1]
data$item2 <- recode[data$item2]
data$item3 <- recode[data$item3]
data$item4 <- recode[data$item4]
data$item5 <- recode[data$item5]
data$item6 <- recode[data$item6]

#3.2: Nettoyage des donnees
#3.2.1. Traitement des valeurs abérantes 
boxplot(data$Age)
boxplot.stats(data$Age) 
for (i in 1:length(data$Age)){
  for (j in 1:length(boxplot.stats(data$Age)$out)){
    if (data$Age[i]==boxplot.stats(data$Age)$out[j] & !is.na(data$Age[i])){
      data$Age[i]=NA
    }
  }
}
boxplot(data$NbStages)
boxplot.stats(data$NbStages)
for (i in 1:length(data$NbStages)){
  for (j in 1:length(boxplot.stats(data$NbStages)$out)){
    if (data$NbStages[i]==boxplot.stats(data$NbStages)$out[j] & !is.na(data$NbStages[i])){
      
      data$NbStages[i]=NA
    }
  }
}

#3.2.2. Traitement des valeurs manquantes
p=0
for (i in 1:length(data$Age)){
  if (is.na(data$Age[i])){
    p=p+1
  }
}
prop=(p/length(data$Age))*100
print(prop)
if (prop>5){
  for (i in 1:length(data$Age)){
    if (is.na(data$Age[i])){
      data$Age[i]=mean(data$Age,na.rm=TRUE)
    }
  }
}
data$Age=as.integer(data$Age)
p=0
for (i in 1:length(data$NbStages)){
  if (is.na(data$NbStages[i])){
    p=p+1
  }
}
prop=(p/length(data$NbStages))*100
print(prop)
if (prop>5){
  for (i in 1:length(data$NbStages)){
    if (is.na(data$NbStages[i])){
      data$NbStages[i]=mean(data$NbStages,na.rm=TRUE)
    }
  }
}
data$NbStages=as.integer(data$NbStages)


#3.3 Test de normalite
vars_num <- data[, c("Age","NbStages","item1","item2","item3",
                     "item4","item5","item6")]
# On teste tous les items en une seule fois en les regroupant dans une dataframe
normalite <- data.frame(
  Variable = colnames(vars_num),
  Skewness = sapply(vars_num, skewness, na.rm = TRUE),
  Kurtosis = sapply(vars_num, kurtosis, na.rm = TRUE),
  Shapiro_p = sapply(vars_num, function(x) shapiro.test(x)$p.value))
# Affichage de la data frame
normalite
# Ajout des critères du cours
normalite$Decision_Shapiro <- ifelse(normalite$Shapiro_p < 0.05, "H1 acceptee", "H0 rejetee")
# Critère de tolérance [-3, 3] pour Skewness et Kurtosis
normalite$Acceptable_Indicateurs <- ifelse(
  abs(normalite$Skewness) <= 3 & abs(normalite$Kurtosis) <= 3, "Toléré", "Trop biaisé")
for(i in 1:nrow(normalite)) {
  if(normalite$Shapiro_p[i] < 0.05) {
    cat("-", normalite$Variable[i], ": Le test de Shapiro rejette la normalité (p < 5%).")
    if(normalite$Acceptable_Indicateurs[i] == "Toléré") {
      cat(" Cependant, avec un Skewness et Kurtosis entre [-3,3], la distribution est acceptable pour des tests paramétriques.\n")
    } else {
      cat(" La distribution est trop asymétrique, on opte pour des tests non parametriques.\n")
    }
  } else {
    cat("-", normalite$Variable[i], ": La distribution est parfaitement normale (p > 5%).\n")
  }
}

#3.4. Test de fiabilité et de validité
install.packages("psych")
library(psych)
#On regroupe les items
df_items <- data[, c("item1", "item2", "item3", "item4", "item5", "item6")]

# Definition de la fonction pour le calcul de la fiabilite
Reliability <- function(tableau_items) {
  # Calcul de l'Alpha de Cronbach via le package psych
  res <- psych::alpha(tableau_items, check.keys = TRUE)
  alpha_val <- res$total$raw_alpha
  cat("Resultat de la fiabilite\n")
  cat("Alpha de Cronbach :", round(alpha_val, 3), "\n")
  if (alpha_val >= 0.7) {
    cat("Conclusion : Le questionnaire est fiable.\n")
  } else {
    cat("Conclusion : Le questionnaire est peu fiable.\n")
  }
  return(res)
}
#Resultat de la fiabilite
resultat_fiabilite <-Reliability(df_items)

#3.5. Test de représentativité
table(data$Filiere)
(table(data$Filiere)/58)*100

# Test de Khi-2
test_rep <-chisq.test(table(data$Filiere), p=c(0.4915254237,0.5084745763))
if(test_rep$p.value > 0.05) {
  cat("Conclusion : L'échantillon est representatif de la population réelle.\n")
} else {
  cat("Conclusion : L'échantillon n'est pas parfaitement représentatif.\n")
}
#3.6. Test de saturation semantique
#  Creation de la dataframe 
data_employabilite <- data.frame(
  Themes = c( "Théorie vs Pratique", "Logiciels spécifiques", "Fondamentaux", 
              "Auto-formation", "Communication", "Travail en équipe", 
              "Adaptabilité", "Gestion du temps/stress", "Honnêteté/Confiance", 
              "Leadership", "Stages obligatoires", "Projets en équipe", 
              "Vie associative", "Projets personnels", "Manque de réseau", 
              "Manque d'expérience", "Marché saturé", "Étudiants étrangers", 
              "Mobilité", "Réseautage Alumni", "Partenariats entreprises", 
              "Ateliers CV/LinkedIn", "Module Insertion Prof.", "Alternance",
              "Système Mentorat", "Portfolio/GitHub obligatoire", 
              "Crédibilité forums", "Sous-estimation par profs"),
  Int_AD = c(1,0,0,1,1,1,1,0,0,0,1,1,1,0,1,1,1,1,0,1,0,1,1,0,0,0,0,0),
  Int_IK = c(1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,0,0,0,1,1,0,1,0,0,0,0),
  Int_YB = c(1,1,0,1,1,1,0,0,1,0,1,1,1,0,1,0,1,0,1,1,1,0,0,0,0,0,1,0),
  Int_NB = c(1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0),
  Int_AM = c(1,1,1,1,1,1,1,1,0,0,1,1,0,1,0,1,0,0,0,1,0,1,1,0,1,1,0,1))

#  Calcul des occurrences par thème (Redondance)
data_employabilite$Row_Sum <- rowSums(data_employabilite[, 2:6])

# Total des occurrences redondantes (Thèmes cités au moins 2 fois)
redondance_totale <- sum(data_employabilite$Row_Sum[data_employabilite$Row_Sum >= 2])

# Total global des thèmes abordés (Toutes les occurrences, y compris les uniques)
total_global <- sum(data_employabilite$Row_Sum)
# Calcul du ratio
saturation <- (redondance_totale / total_global) * 100
#Affichage des résultats
cat("Somme des thèmes redondants :", redondance_totale, "\n")
cat("Total global des occurrences :", total_global, "\n")
cat("Calcul de la saturation  :", round(saturation, 2), "%\n")

#Conclusion
if(saturation>= 80) {
  print("Conclusion : Niveau de saturation excellent. L'échantillon est validé.")
}else{
  print("Le niveau de saturation est faible.")
}

#ETAPE 4:TRAITEMENT
#4.1. Statistique descriptive 
#4.1.1. Traitement numérique
summary(data)

#4.1.2. Traitement Graphique
install.packages("ggplot2")
library(ggplot2)
items <- paste0("item", 1:6)

for (it in items) {
  p <- ggplot(data, aes(x = .data[[it]])) +
    geom_histogram(aes(y = ..density..), bins = 10, fill="skyblue", color="black") +
    geom_density(color="red", size=1) +
    labs(title = paste("Distribution de l'", it), x = it, y = "Densité") +
    theme_minimal()
  
  print(p)
}

# Analyse de la sommabilite des items avec l'Analyse a Composantes Principales (ACP)
#Installation des packages
install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)
# On fait l'ACP sur les 6 items
res_acp <- PCA(data[, c("item1", "item2", "item3", "item4", "item5", "item6")], graph = FALSE)
# On affiche le cercle
fviz_pca_var(res_acp, col.var = "blue") + 
  ggtitle("Toutes les flèches à droite = Somme possible")

# 4.2 Statistiques bivariee
#Test des hypothèses 
chisq.test(table(data$item1))
chisq.test(table(data$item2))
chisq.test(table(data$item3))
chisq.test(table(data$item4))
chisq.test(table(data$item5))
chisq.test(table(data$item6))
# Test parametrique et non parametrique
# Pour les items "Acceptables" (Paramétriques : 1, 2, 3, 5, 6)
# On compare les moyennes selon la filiere
items_param <- c("item1", "item2", "item3", "item5", "item6")

for (it in items_param) {
  # Utilisation de l'ANOVA car distribution suit un loi quasi normale
  res_anova <- aov(data[[it]] ~ data$Filiere, data =data)
  p_val <- summary(res_anova)[[1]][["Pr(>F)"]][1]
  
  cat("Item:", it, " p-value (ANOVA):", round(p_val, 4), "\n")
}

#  Pour l'item 4 
items_non_param <- c("item4")

# Utilisatiom de Kruskal-Wallis pour le test non parametrique
for (it in items_non_param) {
  res_kw <- kruskal.test(data[[it]] ~ data$Filiere)
  
  cat("Variable:", it, " p-value (Kruskal):", round(res_kw$p.value, 4), "\n")
}
