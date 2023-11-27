# création base pour les exemples
library(dplyr)


prenoms <- readr::read_delim("donnees_prenoms/dpt2021.csv")
corresp_reg_dep <- readr::read_delim("donnees_prenoms/corresp_reg_dep.csv") |>
  rename(reg = code_reg) |>
  mutate(code_dpt = if_else(code_dpt %in% c("2A", "2B"), "20", code_dpt),
         lib_dpt = if_else(code_dpt == "20", "Corse", lib_dpt))


head(prenoms)
head(corresp_reg_dep)

# ajout libellés départements et régions
# renommage variable prénom
# recodage variable sexe
# recodage variable prénom
prenoms <- prenoms |>
  left_join(corresp_reg_dep,
            by = c("dpt" = "code_dpt")) |>
  rename(prenom = preusuel) |>
  mutate(sexe = if_else(sexe == 1, "G", "F"),
         prenom = stringr::str_to_title(prenom)) |>
  relocate(prenom, sexe, annais, nombre, dpt, lib_dpt, reg, lib_reg)

# réduction taille données
liste_prenoms <- c("Charlie", "Alix", "Eden", "Sasha", "Camille", "Louison", "Noa")
prenoms <- prenoms |>
  filter(annais %in% c("1900", "2020") &
           prenom %in% liste_prenoms)

# enregistrement
usethis::use_data(prenoms, overwrite = TRUE)

