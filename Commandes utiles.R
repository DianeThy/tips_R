#### Manips R ####



#--------------------------------Gestion des fichiers

# Déposer Rmd sur GDrive pourtravailler en collaboration
trackdown::upload_file(file = "scripts/Rapport_final.Rmd", gfile = "Rapport_final.Rmd")
trackdown::download_file(file = "scripts/Rapport_final.Rmd", gfile = "Rapport_final")




#--------------------------------Départ d'analyse


# Créer df
x <- data.frame(col1 = c("BASSIN DE POMPEY", "CC AURE ET LOURON", "CC DU BRIANCONNAIS", "CC DU PAYS DE SALERS"),
                col2 = c(2, 1, 7,6))

# Import df zippé
download.file("https://www.insee.fr/fr/statistiques/fichier/5359146/dossier_complet.zip", "dossier_complet.zip")
unzip("dossier_complet.zip")
data <- read_delim("dossier_complet.csv", ";", trim_ws = TRUE)

# Scraping table with several pages
library(rvest)
data <- purrr::map(
        .x = (as.data.frame(rep(1:5, each = 1)) %>% rename(page = `rep(1:5, each = 1)`))$page,
        .y = data.frame(matrix(ncol = 1, nrow = 1)),
        .f = ~read_html(paste0("http://portal.core.edu.au/conf-ranks/?search=&by=all&source=all&sort=atitle&page=", .x)) %>% html_nodes('body')  %>% html_nodes('table') %>% html_table(dec = ","), 
        .default = NA)
data <- bind_rows(data)

 # Aplatissement JSON
     # sélection des variables qui nous intéressent
data <- purrr::map(
        .x = json_data,
        .y = data.frame(matrix(ncol = 1, nrow = 1)),
        possibly(.f = ~unnest(data.frame(    # on récupère chaque élément/variable qui nous intéresse, on les met dans un df
                       doi = .x$doi, 
                       .x$authorships),
                   cols = "institutions", names_repair = "universal") %>% select(doi, country_code), otherwise = NA_character_), 
        .default = NA)
     # suppression des NA et mise au format tabulaire
data <- data[data != "NA"] # replace NA by NULL
data <- rrapply(data, condition = Negate(is.null), how = "prune") #remove NULL
data <- data %>% bind_rows()



#--------------------------------Manipulations de bases


    ### MODIFICATION DE VARIABLES   

# Renomme colonne
data <- data %>% rename(nv_nom = ancien_nom)

# Changer format date value
data$Date <- format(as.Date(data$Date, format="%d/%m/%Y"),"%Y/%m/%d")

# Remplacer valeur df
data$column <- str_replace_all(data$column, c("ancienne valeur" = "nouvelle valeur"))
data[data == "null"] <- NA
data <- data %>% mutate_all(function(x) gsub("pattern1 | pattern2", "replacement", x))

# Supprimer les caractères spéciaux ex : ? ' !
data <- data %>% mutate(col = str_replace_all(col, "[^[:alnum:]]", " "))

# Supprimer valeurs contenant une virgule
data <- data %>% filter(!grepl(',', column))

# Garder valeurs qui contiennent une chaîne de caractères
data <- data %>% filter(grepl("mots particuliers", column) == TRUE)

# Remplacer NA par 0
data <- data %>% mutate(col = replace_na(col, 0))

# Remplacer les cellules vides par des NA
data <- data %>% mutate_all(na_if, "")

# Filtrer avec plusieurs conditions
data <- data %>% filter(type == "MET" | type == "CU" | type == "CC" | type == "CA") 

# Affecte aux NA les valeurs d'une autre colonne
data$column[is.na(data$column)] <- as.character(data$replacment[is.na(data$column)])  

# Ajouter un 0 pour passer de 1 digit à 2
data$nom <- sprintf("%02d", data$num)
data <- data %>% mutate(nom = str_pad(nom, 14, pad = "0"))

# Retrait du premier chiffre si c'est un zéro
data$nom <- gsub("^0", "", data$nom)

# Réunir plusieurs colonnes en 1
data <- data %>% mutate(new_col = coalesce(col1,col2,col3))

# Calculer nouvelle date = date + période en mois
library(mondate)
data <- data %>% mutate(date_fin = as.mondate(date_debut) + duree)

# Extraction de chiffres dans une chaîne de caractères
library(strex)
data <- data %>% mutate(min = str_nth_number(replies, n = 1)) # extrait le 1er chiffre du string

# Extraction d'une date
data <- data %>% mutate(annee = str_extract(`En quelle année ?`, "(1|2)\\d{3}")) #"\\d{5}" pour zipCode




    ### CREATION DE VARIABLES   

# Créer une variable booléenne
data$var1 <- case_when(data$pop_insee < 3500 ~ 0,
                       data$pop_insee >= 3500 ~ 1)
data$var2 <- case_when(is.na(data$nb_ptf) & is.na(data$nb_datagouv) ~ 0,
                       !is.na(data$nb_ptf) ~ 1,
                       !is.na(data$nb_datagouv) ~ 1)

# Affecter des valeurs aléatoires
random <- c("groupe 1", "groupe 2", "groupe 3")
data$new_col <- sample(random, size = nrow(data), replace = TRUE, prob = c(9/10,0.06,0.04))

# Valeurs des 2 colonnes en une
data$new_col <- paste(data$col1, data$col2, sep='')

# Extraire certains caractères d'une chaîne de caractères
data$sub_string <- substr(data$chaine_charac, 1, 5)





    ### REUNIR 2 DATAFRAMES

# Enlever majuscules et accents
    # majuscules
data <- data %>% mutate(nom = toupper(nom))
    # pas d'accents
data <- data.table::data.table(data)
data[, nom_upper := stringi::stri_trans_general (str = nom, id = "Latin-ASCII")]
    # pas d'accents, pas de traits d'union, majuscules
data <- data %>% mutate(col = stringi::stri_trans_general(str = gsub("-", " ", toupper(col)), id = "Latin-ASCII"))

# Match exact
data <- left_join(data, data2, by="col_name", copy=FALSE)

# Match inexact
library(fuzzyjoin)
data <- stringdist_left_join(data, data2, by="col_name", max_dist = 5, distance_col="distance")  %>% group_by(nom) %>% slice_min(distance)

# Merger 2 df ; lignes les unes après les autres
data_merged <- merge(df_1, df_2, all=TRUE)   # ou rbind()




    ### Comparaison de dataframes

# Différence entre 2 df
anti_join(df1, df2)

# Commun entre 2 df
semi_join(df1, df2)


#--------------------------------Statistiques


# Compte nombre de NA par colonne
data %>% count(is.na(col_name))

# Compte nb et % de NA ds df par colonne
NA_data <- as.data.frame(apply(is.na(data), 2, sum)) %>% 
                        rename(nb_NA = `apply(is.na(data), 2, sum)`) %>%
                        mutate(percent_NA = nb_NA/nrow(data)*100) %>% 
                        mutate(percent_NA = round(percent_NA, 2))


# Somme par groupe
data <- data %>% group_by(group) %>% summarise(value = sum(value)) %>% ungroup()


# Fréquence des valeurs
data <- as.data.frame(table(data$column))  #R base
data <- data %>% group_by(group) %>% count(column)   #dplyr
data <- data %>% group_by(group, column) %>% summarise(n = n()) #idem dplyr
n_distinct(data$column)   #nb d'obs différentes


#--------------------------------Plots


# Afficher plusieurs ggplots
library(gridExtra)
grid.arrange(g1,g2,g3, ncol=3, nrow = 1)
    # avec les boxs alignés
library(cowplot)
plot_grid(p3.1, p3.2, p3.3, p3.4, p3.5, align='vh')
# Afficher plusieurs ggplotlys
library(plotly)
subplot(plotly_positif, plotly_negatif, nrows=1)

# Trouver toutes les icones
    #https://fontawesome.com/v5.15/icons?d=gallery&p=2
    #https://ionic.io/ionicons
    #https://jpswalsh.github.io/academicons/

# GGplot
graph <- data %>% ungroup() %>% mutate(colonne = fct_reorder(colonne, n)) %>%
  ggplot(aes(x = colonne, y = n, fill = groupe, text = c(n, "individus"))) +
    # Type de graph
  geom_col(position = "stack", width = 0.7, color = "white") +
  coord_flip() +
  geom_line(size=1.7, alpha=0.9, linetype=1, color = "#0066CC") +
  geom_point(colour="#0066CC", fill="#0066CC", size = 2, pch = 21, stroke = 1.5) +
  geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = 2, fill = "#21468d") +
  geom_text_wordcloud(family = "Montserrat") +
    # Couleurs
  scale_fill_manual(values = c("#c898ae", "#da4729", "#f38337", "#74a466", "#fecf5d", "#5E79AC")) + #couleurs Bauhaus
    # Labels
  labs(x = "", y = "Nombre de commentaires", 
       title = "Propositions ayant été commentées par au moins 15 utilisateurs",
       fill = "", color = "") +
    # Axes
  xlim(1, 100) +
  scale_y_continuous(labels = scales::comma) + #grands chiffres lisibles
  scale_y_continuous(breaks= scales::pretty_breaks()) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + # pourcentages
    # Mise en page générale
  theme_classic() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 5, l = 0)), #augmenter marges
        plot.title = element_text(hjust = 1), #titre ne déborde pas à droite
        plot.background = element_rect(fill = "#FAF3EE", colour="#FAF3EE"), #background couleur Datactivist
        panel.background = element_rect(fill = "#FAF3EE", colour = "#FAF3EE"),
        legend.background = element_rect(fill = "#FAF3EE", colour = "#FAF3EE")) +
    # Légende
  guides(fill = guide_legend(nrow = 6, byrow = TRUE,  # nombre d'éléments par ligne
                             title = "titre légende")) + # titre légende
    # Éléments additionnels
  geom_text(aes(y = 1, label = title_projet, hjust="bottom"), #aligner geom_text à gauche avec coord_flip
            fontface = "italic", size = 2.6) +
  geom_vline(xintercept = -.5, linetype = 2) +
# Passe en plotly
ggplotly(graph, tooltip = c("text"))





#--------------------------------Fin d'analyse


# Save png

# Save widgets

# Save database
rio::export(data, "./Data/raw/base_de_donnees.xlsx")
write.csv(data,"./Data/raw/base_de_donnees.csv", row.names = FALSE, fileEncoding = "UTF-8")

