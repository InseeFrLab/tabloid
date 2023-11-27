#' Afficher un tableau mis en forme au format kable
#'
#' @description tab_render() met en forme un tableau croisé au format html, pour le visualiser ou l'insérer dans
#' un rappport md.
#'
#'
#' @param tab tableau croisé issu de tab_build
#' @param title titre - par défaut : "" (aucun)
#' @param note note de bas de page - par défaut : "" (aucun)
#' @param lab_rows nouveaux libellés pour les variables lignes - par défaut : NULL
#' @param lab_cols nouveaux libellés pour les modalités des variables colonnes - par défaut : NULL
#'
#' @return tableau mis en forme pour insertion html (kable)
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' prenoms |>
#' filter(annais %in% c("1900", "2020") &
#'          prenom == "Camille") |>
#'   tab_build(var_rows = lib_reg,
#'             var_cols = c(annais, sexe),
#'             lab_total = "Ensemble",
#'             stat = "count") |>
#'   tab_render(title = "Nombre d'enfants prénommés Camille en 1900 et en 2020",
#'              note = "Source : Insee - base des prénoms",
#'              lab_rows = "Région",
#'              lab_cols = list(sexe = c("Garçons", "Filles")))
tab_render <- function(tab,
                       title = "",
                       note = "",
                       lab_rows = NULL,
                       lab_cols = NULL) {

  # vérifications des paramètres
  assertthat::assert_that(!is.null(tab), msg = "Erreur dans la réalisation du tableau, affichage impossible")
  assertthat::assert_that(attr(tab, "type") == "tab", msg = "tab_render s'utilise sur un objet issu de la fonction tab_build")
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
  assertthat::assert_that(length(title) == 1)
  assertthat::assert_that(length(note) == 1)
  # ===

  params <- attr(tab, "params")

  # 1. mise en forme du tableau ====
  # changement format numérique --> séparateur décimal = virgule, séparateur millier = espace
  kable_tab <- tab |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ format(.x, scientific = FALSE, big.mark = " ", decimal.mark = ",")))


  # modification des libellés
  kable_tab <- kable_tab |> modif_libelles(new_lab_col = lab_cols,
                                           new_lab_row = lab_rows,
                                           old_lab_col = params$var_cols,
                                           old_lab_row = params$var_rows,
                                           modalites = params$modalites,
                                           lab_total = params$lab_total)



  if(length(params$var_cols) == 2) {

    cols <- kable_tab |> get_var_cols_mod(num_col = 1,
                                          nb_var_lignes = length(params$var_rows))
    names(kable_tab)[length(params$var_rows)+1:length(cols)] <- kable_tab |> get_var_cols_mod(num_col = 2,
                                                                                              nb_var_lignes = length(params$var_rows))

  }

  # 2. export du tableau en kable ====

  ## format html ----
  kable_tab <- kable_tab |>
    knitr::kable(caption = title, align = "c")  |>
    kableExtra::kable_classic(full_width = FALSE,
                              html_font = '"Arial Narrow", arial, helvetica, sans-serif')  |>
    kableExtra::footnote(general = note,
                         general_title = "")


  ## gestion des intitulés de colonnes dans le cas de deux variables colonnes ----
  if(length(params$var_cols) == 2) {

    headers <- purrr::set_names(rle(cols)$lengths, unique(cols))

    kable_tab <- kable_tab |>
      kableExtra::add_header_above(header = c(" " = length(params$var_rows), headers))


  }

  ## gestion des couleurs des lignes et colonnes correspondant à des totaux ----

  index_row_color <- get_index_row_total(tab)
  index_col_color <- get_index_col_total(tab)

  if(sum(index_row_color) > 0){
    for(n in setdiff(unique(index_row_color), 0)){
      kable_tab <- kable_tab |>
        kableExtra::row_spec(which(index_row_color == n),
                             background = grDevices::grey(1 - n / 10),
                             bold = TRUE)

    }
  }

  if(sum(index_col_color) > 0){
    for(n in setdiff(unique(index_col_color), 0)){
      kable_tab <- kable_tab |>
        kableExtra::column_spec(which(index_col_color == n),
                                background = grDevices::grey(1 - n / 10),
                                bold = TRUE)

    }
  }


  return(kable_tab)
}
