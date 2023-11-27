#' Exporter un tableau dans un classeur xlsx
#'
#' @description tab_xlsx() permet d'enregistrer le tableau mis en forme dans un fichier .xlsx.
#'
#'
#' @param tab tableau croisé issu de tab_build
#' @param path chemin dans lequel exporter le tableau
#' @param sheet nom de l'onglet dans le classeur - si l'onglet existe déjà, il sera mis à jour avec update = TRUE
#' @param title titre - par défaut : "" (aucun)
#' @param note note de bas de page - par défaut : "" (aucun)
#' @param lab_rows nouveaux libellés pour les variables lignes - par défaut : NULL
#' @param lab_cols nouveaux libellés pour les modalités des variables colonnes - par défaut : NULL
#' @param open_wb ouverture du classeur crée pour visualisation - par défaut : TRUE
#' @param update mise à jour de l'onglet s'il existe déjà dans le classeur - par défaut : TRUE
#'
#' @return classeur excel contenant les données exportées
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' prenoms |>
#'   filter(annais == "2020" &
#'            lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
#'   tab_build(var_rows = c(lib_reg, prenom),
#'             var_cols = sexe,
#'             var_stat = nombre,
#'             lab_total = "Total",
#'             stat = "sum") |>
#'   tab_xlsx(title = "Nombre de naissances avec des prénoms mixtes en 2020",
#'            note = "Source : Insee - base des prénoms",
#'            lab_cols = list(sexe = c("Garçons", "Filles")),
#'            lab_rows = c("Région", "Prénom"),
#'            path =  "U:/test_export.xlsx",
#'            sheet = "naissances_1")
#'}
tab_xlsx <- function(tab,
                     path,
                     sheet,
                     title = "",
                     note = "",
                     lab_rows = NULL,
                     lab_cols = NULL,
                     open_wb = TRUE,
                     update = TRUE){


  # vérifications des paramètres ====
  assertthat::assert_that(attr(tab, "type") == "tab", msg = "tab_xlsx s'utilise sur un objet issu de la fonction tab_build")
  if(!missing(lab_rows)) assertthat::assert_that(length(lab_rows) == length(attr(tab, "params")$var_rows),
                                                 msg = "Le paramètre lab_rows doit contenir autant de modalités que de variables en ligne dans le tableau")
  if(!missing(lab_cols)) assertthat::assert_that(inherits(lab_cols, "list"),
                                                 msg= "Le paramètre lab_cols doit s'écrire sous forme de liste -\n lab_cols = list(nom_var_col1 = c('...', '...'), nom_var_col2 = c('...', '...'))")
  if(!missing(lab_cols)) {
    assertthat::assert_that(all(names(lab_cols) %in% attr(tab, "params")$var_cols),
                            msg = glue::glue("Le parmètre lab_cols doit contenir des noms parmi :
                                             {glue::glue_collapse(attr(tab, 'params')$var_cols, sep = ', ', last = ' et ')}")
    )
  }
  if(!missing(lab_cols)) {
    assertthat::assert_that(all(purrr::map2_lgl(attr(tab, "params")$modalites[which(names(attr(tab, "params")$modalites) %in% names(lab_cols))],
                                                lab_cols,
                                                ~ length(.x) == (length(.y)+1))),
                            msg = "Le paramètre lab_cols n'a pas le bon nombre de modalités pour les variables colonnes")
  }


  assertthat::assert_that(is.character(title), msg = "Le paramètre title doit être un vecteur de format caractère")
  assertthat::assert_that(is.character(note), msg = "Le paramètre note doit être un vecteur de format caractère")
  assertthat::assert_that(is.character(path), msg = "Le paramètre path doit être un vecteur de format caractère")
  assertthat::assert_that(is.character(sheet), msg = "Le paramètre sheet doit être un vecteur de format caractère")
  assertthat::assert_that(length(title) == 1)
  assertthat::assert_that(length(note) == 1)
  assertthat::assert_that(length(path) == 1)
  assertthat::assert_that(length(sheet) == 1)

  assertthat::assert_that(stringr::str_sub(path, -5) == ".xlsx",
                          msg = "Renseigner un nom de fichier en .xlsx")

  assertthat::assert_that(rlang::is_bool(open_wb), msg = "Le paramètre open_wb doit être TRUE ou FALSE")
  assertthat::assert_that(rlang::is_bool(update), msg = "Le paramètre update doit être TRUE ou FALSE")

  if(file.exists(path) & !update){
    assertthat::assert_that(!(sheet %in% names(openxlsx::loadWorkbook(path))),
                            msg = "Le classeur contient déjà un onglet avec ce nom - pour écraser l'onglet, choisir update = TRUE, sinon, choisir un autre nom d'onglet")
  }
  # ===



  # 1. création ou récupération du classeur de résultats ====

  nom_fichier <- path |> stringr::str_remove("^.*(\\/)")
  chemin_dossier <- path |> stringr::str_remove(nom_fichier)

  ## si le dossier n'existe pas : création du dossier
  if(!dir.exists(chemin_dossier)) dir.create(chemin_dossier, recursive = TRUE)

  ## si le classeur existe déjà : on le récupère pour ajouter un onglet au classeur existant
  ## sinon : on crée un nouveau classeur
  if (file.exists(path)) {

    classeur <- openxlsx::loadWorkbook(path)

    if(sheet %in% names(classeur) & update){

      classeur |> openxlsx::removeWorksheet(sheet = sheet)
      message(glue::glue("L'onglet {sheet} existait déjà dans le classeur, son contenu est mis à jour"))
      message("Pour ne pas écraser l'onglet existant, choisir un autre nom")

    }
  } else {

    classeur <- openxlsx::createWorkbook()

  }

  # création de l'onglet
  classeur |> openxlsx::addWorksheet(sheet)

  # création des styles
  style_titre <- openxlsx::createStyle(fontSize = 14,
                                       fontColour = "black",
                                       textDecoration = c("bold"))
  style_entete <-  openxlsx::createStyle(fontSize = 12,
                                         fontColour = "black",
                                         textDecoration = c("bold"),
                                         border = c("top", "bottom", "left", "right"),
                                         borderStyle = "thin",
                                         wrapText = TRUE,
                                         halign = "center",
                                         valign = "center")
  style_cellule <- openxlsx::createStyle(fontSize = 12,
                                         fontColour = "black",
                                         border = c("top", "bottom", "left", "right"),
                                         borderStyle = "thin",
                                         halign = "center")
  style_source <- openxlsx::createStyle(fontSize = 9,
                                        fontColour = "black",
                                        textDecoration = c("italic"))





  # 2. écriture du tableau dans le classeur ====

  params <- attr(tab, "params")

  ## 2.a)  mise en forme du tableau ----
  # changement format numérique --> séparateur décimal = virgule, séparateur millier = espace
  xlsx_tab <- tab |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ format(.x, scientific = FALSE, big.mark = " ", decimal.mark = ",")))

  # modification des libellés
  xlsx_tab <- xlsx_tab |> modif_libelles(new_lab_col = lab_cols,
                                         new_lab_row = lab_rows,
                                         old_lab_col = params$var_cols,
                                         old_lab_row = params$var_rows,
                                         modalites = params$modalites,
                                         lab_total = params$lab_total)


  ## 2.b) ajout des différents éléments dans l'onglet ----

  ### 2.b).i) ajout du titre
  classeur |> openxlsx::setRowHeights(sheet = sheet, rows = 1, heights = 20)
  classeur |> openxlsx::writeData(sheet = sheet, x = title, startCol = 1, startRow = 1)
  classeur |> openxlsx::addStyle(sheet = sheet,
                                 cols = 1,
                                 rows = 1,
                                 style = style_titre)


  ### 2.b).ii) ajout des données

  #### 2.b).ii) - gestion des variables colonnes
  ### si une ou aucune variable colonne : le tableau commence à la troisième ligne de l'onglet
  ### si deux variables colonnes : la troisième comprend les modalités de la première variable colonne
  ###                               et le tableau commence alors à la quatrième ligne de l'onglet


  if(length(params$var_cols) != 2) {
    start_row <- 3
  }


  if(length(params$var_cols) == 2){

    start_row <- 4

    cols <- xlsx_tab |> get_var_cols_mod(num_col = 1,
                                         nb_var_lignes = length(params$var_rows))

    classeur |> openxlsx::writeData(sheet = sheet,
                                    x = t(as.data.frame(cols)),
                                    startRow = 3,
                                    startCol = length(params$var_rows) + 1,
                                    rowNames = FALSE,
                                    colNames = FALSE)

    classeur |> openxlsx::addStyle(sheet = sheet,
                                   style = style_entete,
                                   cols = (length(params$var_rows) + 1):length(cols),
                                   rows = 3)

    classeur |> merge_row_cells(sheet = sheet,
                                ligne = cols,
                                index_ligne = 3,
                                start_col = length(params$var_rows))

    names(xlsx_tab)[length(params$var_rows)+1:length(cols)] <- xlsx_tab |> get_var_cols_mod(num_col = 2,
                                                                                            nb_var_lignes = length(params$var_rows))

  }


  #### 2.b).ii) - ajout du tableau dans l'onglet

  classeur |> openxlsx::writeData(sheet = sheet,
                                  x = as.data.frame(xlsx_tab),
                                  startRow = start_row,
                                  startCol = 1,
                                  rowNames = FALSE,
                                  headerStyle = style_entete)

  classeur |> openxlsx::addStyle(sheet = sheet,
                                 style = style_cellule,
                                 cols = 1:ncol(tab),
                                 rows = (1:nrow(tab)) + start_row,
                                 gridExpand = TRUE)



  #### 2.b).ii) - ajout de la source

  classeur|> openxlsx::writeData(sheet = sheet,
                                 x = note,
                                 startRow = start_row + nrow(tab) + 2)
  classeur|> openxlsx::addStyle(sheet = sheet,
                                cols = 1,
                                rows = start_row + nrow(tab) + 2,
                                style = style_source)


  # 3. mise en forme de l'onglet ====

  ## 3.a) gestion de la hauteur des lignes ----
  classeur |> openxlsx::setRowHeights(sheet = sheet, rows = 3:start_row, heights = 30)


  ## 3.b) fusion des cellules identiques ----
  # pour toutes les modalités identiques des variables lignes, les cellules sont fusionnées
  # (dès qu'il y a plus d'une variable en ligne, sinon, toutes les modalités sont différentes)
  if (length(params$var_rows) > 1){

    1:(length(params$var_rows)-1) |>
      purrr::map(~ merge_col_cells(classeur = classeur,
                                   sheet = sheet,
                                   colonne = tab[[.x]],
                                   index_colonne = .x,
                                   start_row = start_row))

  }

  ## 3.c) gestion de la largeur des colonnes -----
  # pour chaque colonne : calcul du libellé le plus long en nb de caractères
  # (dans les cellules ou les intitulés)
  # puis ajustement de la taille des cellulles à ce max + 2
  data_widths <- xlsx_tab |> purrr::map_dbl(~ max(nchar(as.character(.x))) + 2)
  header_widths <- nchar(colnames(xlsx_tab)) + 2
  col_widths <- pmax(data_widths, header_widths)

  classeur |> openxlsx::setColWidths(sheet = sheet,
                                     cols = 1:ncol(tab),
                                     widths = col_widths)

  ## 3.d) gestion de la couleur sur les lignes / colonnes ensemble -----

  ### 3.d).i) lignes

  # identification des lignes qui correspondent à un total
  index_row_color <- get_index_row_total(tab)

  # pour chaque colonne du tableau :
  # --> pour chaque ligne correspondant à un total
  #     changement du style de cellule avec un fond nuance de gris (selon type de total) + gras
  1:ncol(xlsx_tab) |>
    purrr::map(function(colonne)  unique(index_row_color[which(index_row_color!=0)]) |>
                 purrr::map(function(n) classeur|> openxlsx::addStyle(sheet = sheet,
                                                                      cols = colonne,
                                                                      rows = which(index_row_color == n) + start_row,
                                                                      style = openxlsx::createStyle(fgFill = grDevices::grey(1 - n / 10),
                                                                                                    fontSize = 12,
                                                                                                    textDecoration = c("bold"),
                                                                                                    border = c("top", "bottom", "left", "right"),
                                                                                                    borderStyle = "thin",
                                                                                                    halign = "center"))))

  ### 3.d).ii) colonnes

  # identification des colonnes qui correspondent à un total
  index_col_color <- get_index_col_total(tab)

  # pour chaque ligne du tableau :
  # --> pour chaque colonne correspondant à un total
  #     changement du style de cellule avec un fond nuance de gris (selon type de total) + gras
  1:nrow(xlsx_tab) |>
    purrr::map(function(ligne)  unique(index_col_color[which(index_col_color!=0)]) |>
                 purrr::map(function(n) classeur|> openxlsx::addStyle(sheet = sheet,
                                                                      cols = which(index_col_color == n),
                                                                      rows = ligne + start_row - 1,
                                                                      style = openxlsx::createStyle(fgFill = grDevices::grey(1 - n / 10),
                                                                                                    fontSize = 12,
                                                                                                    textDecoration = c("bold"),
                                                                                                    border = c("top", "bottom", "left", "right"),
                                                                                                    borderStyle = "thin",
                                                                                                    halign = "center",
                                                                                                    valign = "center"))))


  # 4. enregistrement du classeur ====

  classeur |> openxlsx::saveWorkbook(path, overwrite = TRUE)


  # 5. ouverture du classeur pour visualisation ====

  if(open_wb){

    openxlsx::openXL(path)

  }

}


#' get_index_fusion
#'
#' @param vecteur vector de valeurs différentes ou non
#'
#' @return liste des index des cellules à fusionner
#' @noRd
get_index_fusion <- function(vecteur) {

  index_chgt <- which(vecteur != dplyr::lag(vecteur))

  purrr::map2(c(1, index_chgt),
              c(index_chgt-1, index_chgt[length(index_chgt)]),
              ~ c(.x, .y))

}


#' fusion_cellules_col
#'
#' @param classeur nom du classeur xlsx
#' @param sheet nom de l'onglet
#' @param colonne vecteur colonne
#' @param index_colonne integer : index de la colonne
#' @param start_row à partir de quelle ligne
#'
#' @noRd
merge_col_cells <- function(classeur,
                            sheet,
                            colonne,
                            index_colonne,
                            start_row = 0){

  index_fusion <- get_index_fusion(colonne)

  index_fusion |> purrr::map(~ classeur |>
                               openxlsx::mergeCells(sheet = sheet,
                                                    cols = index_colonne,
                                                    rows = .x + start_row))

  index_fusion |> purrr::map(~ classeur |>
                               openxlsx::addStyle(sheet = sheet,
                                                  cols = index_colonne,
                                                  rows = .x + start_row,
                                                  style = openxlsx::createStyle(valign = "top",
                                                                                halign = "center",
                                                                                border = c("top", "bottom", "left", "right"),
                                                                                borderStyle = "thin",
                                                                                fontColour = "black")))

}


#' fusion_cellules_row
#'
#' @param classeur nom du classeur xlsx
#' @param sheet nom de l'onglet
#' @param ligne vecteur ligne
#' @param start_col à partir de quelle colonne
#' @param index_ligne integer : index de la ligne
#'
#' @return classeur avec cellules fusionnées sur la ligne
#' @noRd
merge_row_cells <- function(classeur,
                            sheet,
                            ligne,
                            index_ligne,
                            start_col){

  index_fusion <- get_index_fusion(ligne)

  index_fusion |> purrr::map(~ classeur |>
                               openxlsx::mergeCells(sheet = sheet,
                                                    cols = .x + start_col,
                                                    rows = index_ligne))

  index_fusion |> purrr::map(~ classeur |>
                               openxlsx::addStyle(sheet = sheet,
                                                  cols = .x + start_col,
                                                  rows = index_ligne,
                                                  style = openxlsx::createStyle(valign = "center",
                                                                                halign = "center",
                                                                                border = c("top", "bottom", "left", "right"),
                                                                                borderStyle = "thin",
                                                                                fontColour = "black")))

}


#' renommer_modalites_colonnes
#'
#' @param df data.frame
#' @param old_mod character vector : nom des anciens modalités
#' @param new_mod character vector : nom des nouvelles modalités
#' @param nom_var character : nom de la variable dont on veut renommer les modalités
#'
#' @return data.frame
#' @noRd
rename_col_mod <- function(df,
                           old_mod,
                           new_mod,
                           nom_var){

  purrr::reduce2(old_mod[[nom_var]],
                 new_mod[[nom_var]],
                 function(data, mod_init, mod_rempl) data |>
                   dplyr::rename_with(.cols = tidyselect::matches(glue::glue("\\b{mod_init}|{mod_init}\\b")),
                                      .fn = function(nom) stringr::str_replace(nom, mod_init, mod_rempl)),
                 .init = df)
}
