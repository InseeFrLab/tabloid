library(devtools)

# Initialise le package R
# create_package("tabloid")
# use_mit_license()

# Chargement des fonctions exportées du package
load_all()

# Documentation des fonctions
document()
# pkgdown::build_site()

library(dplyr)
library(tidyr)


# Chargement de la base des prénoms

prenoms <- readr::read_rds(system.file("data", "prenoms.rds", package = "tabloid"))

# --------------------------
# Tests sur tab_render (non fait avec test_that car objet attendu complexe)

t1 <- prenoms |>
  filter(annais == "1900") |>
  tab_build(var_rows = sexe,
           var_stat = nombre,
           stat = "sum")

t2 <- prenoms |>
  filter(annais == "1900") |>
  tab_build(var_rows = sexe,
            var_stat = nombre,
            stat = "mean")


tab <- prenoms |>
  filter(annais %in% c("1900", "2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum")
tab |> tab_render()

tab_mean <- prenoms |>
  filter(annais %in% c("1900", "2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "mean")
tab_mean |> tab_render()


tab |>
  left_join(tab_mean, by= c("lib_reg", "prenom"), suffix = c("_tot", "_mean")) |>
  tab_render()

tab_f <- prenoms |>
  filter(annais %in% c("1900", "2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire") &
           sexe == "F") |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum")
tab_g <- prenoms |>
  filter(annais %in% c("1900", "2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire") &
           sexe == "G") |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum")

tab_f |> left_join(tab_g, by = c("lib_reg", "prenom"), suffix = c("_filles", "_garcons"))
# ok - trouver un meilleur exemple d'utilisation puisque ça on peut le faire
# directement (et mieux) avec la fonction tab_build

tab_f |> left_join(tab_g,
                   by = c("lib_reg", "prenom"),
                   suffix = c("_filles", "_garcons")) |>
  tab_render()


tab20 <- prenoms |>
  filter(annais %in% c("2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = sexe,
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum")
tab20
tab20 <- tab20 |> tab_pct("row")
tab20
tab20 |> tab_round()
tab20 |> tab_mask(threshold_count = 5) |> tab_round()
tab20 <- tab20 |> tab_round(guarantee_100 = TRUE)


prenoms |>
  filter(annais %in% c("2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = sexe,
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_pct("row") |>
  tab_mask(threshold_count = 5) |>
  tab_round()

prenoms |>
  filter(annais == "2020" &
           prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_mask(mask_total_cols = TRUE) |>
  # tab_render( lab_cols = list(sexe = c("Garçons", "Filles")),      lab_rows = c("Région", "Prénom"))
  tab_xlsx(title = "Nombre de naissances avec des prénoms mixtes en 2020",
           note = "Source : Insee - base des prénoms",
           lab_cols = list(sexe = c("Garçons", "Filles")),
           lab_rows = c("Région", "Prénom"),
           path =  "Z:/test08.xlsx",
           sheet = "naissances_1")


prenoms |>
  filter(annais == "2020" &
       prenom %in% c("Charlie", "Camille") &
                lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  tab_build(var_rows = lib_reg,
            var_cols = c(prenom, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_mask(threshold_count = 5,
           threshold_sum = 100,
           mask_total_rows = TRUE,
           mask_total_cols = TRUE,
           verbose = TRUE) |>
  tab_round()

tab |>
  tab_round(0) |>
  tab_mask(threshold_count = 3,
           threshold_sum = 30,
           mask_total_cols = TRUE,
           mask_total_rows = TRUE) |>
  tab_render()


tab |>
  tab_render(lab_rows = c("Région", "Prénom", "Année de naissance"),
             lab_cols = list(sexe = c("Garçons", "Filles")),
             title = "Nombre de naissances avec des prénoms mixtes",
             note = "Source : Insee - base des prénoms")

# TODO tab_render : fusionner les libellés identiques qui se suivent ? par exemple le libellé de la région dans l'exemple ci-dessus
# TODO tab_build : trouver un moyen d'ajouter les 0 ? par exemple, aucun Charlie, ni fille ni garçon, en 1900 -> MF je serais plutôt d'avis de contre-arbitrer ça
# TODO tab_render : couleurs trop claires de certaines cellules total, il faudrait prendre le gris le plus foncé possible : pourrait peut-être passer par un max de l'indice de gris ?

# --------------------------
# Tests sur tab_xlsx (non fait avec test_that car objet attendu complexe)

data <- prenoms |>
  filter(annais == "2020",
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire"))


data |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_xlsx(title = "Nombre de naissances avec des prénoms mixtes en 2020",
           note = "Source : Insee - base des prénoms",
           lab_cols = list(sexe = c("Garçons", "Filles")),
           lab_rows = c("Région", "Prénom"),
           path =  "U:/test_export.xlsx",
           sheet = "naissances_1")

data |>
  tab_build(var_rows = c(lib_reg, lib_dpt, prenom),
            var_cols = sexe,
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_xlsx(title = "Nombre de naissances avec des prénoms mixtes - en 2020",
           note = "Source : Insee - base des prénoms",
           lab_cols = list(sexe = c("Garçon", "Fille")),
           path =  "U:/test_export.xlsx",
           sheet = "naissances_2")

data |>
  tab_build(var_rows = c(lib_reg),
            var_cols = c(prenom),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_xlsx(title = "Nombre de naissances avec des prénoms mixtes - en 2020",
           note = "Source : Insee - base des prénoms",
           lab_rows = c("Région"),
           path =  "U:/test_export.xlsx",
           sheet = "naissances_3")

data |>
  filter(lib_reg == "Occitanie") |>
  tab_build(var_rows = prenom,
            var_cols = sexe,
            var_stat = nombre,
            stat = "sum") |>
  tab_xlsx(title = 'Nombre de fois où le prénom est donné en Occitanie en 2020',
           path =  "U:/test_export.xlsx",
           sheet = "naissances_4")


# --------------------------
# A convertir en vignette


# TODO à mettre dans vignette
# si on veut changer l'ordre des colonnes, il faut:
# - trier la table ou définir la variable comme un facteur
prenoms |>
  filter(annais %in% c("1900", "2020"),
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
  arrange(annais, lib_reg) |>
  tab_build(var_rows = c(lib_reg, prenom),
            var_cols = c(annais,sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum")  |>
  tab_render(lab_cols = list(sexe = c("Garçon", "Fille")),
             lab_rows = c("Région", "Prénom"),
             title = "Nombre de naissances avec des prénoms mixtes",
             note = "Source : Insee - base des prénoms")

prenoms |>
  filter(annais %in% c("2020")) |>
  mutate(sexe = if_else(sexe == "F",
                        "Filles",
                        "Garçons")) |>
  mutate(annais = as.factor(annais),
         lib_reg = as.factor(lib_reg)) |>
  tab_build(var_rows = lib_reg,
            var_cols = sexe,
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_pct("row") |>
  tab_round(n = 0,
            guarantee_100 = TRUE) |>
  tab_render(title = "53 % des naissances de 2020 sont des garçons")


prenoms |>
  filter(annais == "2020",
         prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
           lib_reg %in% c("Occitanie", "Pays de la Loire"))  |>
  tab_build(var_rows = c(lib_reg, lib_dpt),
            var_cols = c(prenom, sexe),
            var_stat = nombre,
            lab_total = "Total",
            stat = "sum") |>
  tab_render(lab_rows = c("Région", "Département"),
             lab_cols = list(sexe = c("Garçon", "Fille")))





data <- data.frame(VAR1 = c("A","B","B","A"),
                   VAR2 = factor(c("X","X","Y","Y"), levels = c("X", "Y", "Z")),
                   N = c(3,5,8,9))
data
data |> tab_build(var_rows = VAR1, var_cols = VAR2, var_stat = N, stat = "sum")

head(prenoms)


