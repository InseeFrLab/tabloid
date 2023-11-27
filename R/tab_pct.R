#' Calculer des pourcentages en lignes ou en colonnes
#'
#' @description à partir d'un tableau croisé contenant des effectifs ou des sommes,
#' tab_pct() calcule des pourcentages lignes ou colonnes.
#'
#'
#' @param tab tableau croisé issu de tab_build
#' @param pct type de pourcentage souhaité
#' \itemize{
#' \item{`"row"` : pourcentage en lignes}
#' \item{`"col"` : pourcentage en colonnes}
#' }
#'
#' @return tableau transformé en pourcentages lignes ou colonnes
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' prenoms |>
#'   filter(annais == "2020" &
#'          prenom %in% c("Charlie", "Eden", "Camille")) |>
#'   tab_build(var_rows = lib_reg,
#'             var_cols = c(prenom, sexe),
#'             var_stat = nombre,
#'             lab_total = "Total",
#'             stat = "sum") |>
#'   tab_pct(pct = "col")
tab_pct <- function(tab,
                    pct){

  # vérification des paramètres de la fonction ====
  # assertthat::assert_that(inherits(tab, "tab"), msg = "tab_pct s'utilise sur un objet issu de la fonction tab_build")
  assertthat::assert_that(attr(tab, "type") == "tab", msg = "tab_pct s'utilise sur un objet issu de la fonction tab_build")
  assertthat::assert_that(attr(tab, "state") == "build",
                          msg = "Revoir l'ordre : tab_pct s'utilise uniquement juste après tab_build")
  assertthat::assert_that(attr(tab, "params")$stat %in% c("count", "sum", "weighted_sum"),
                          msg = "Les pourcentages lignes ou colonnes ne peuvent être calculés que sur des effectifs ou des sommes")
  assertthat::assert_that(pct %in% c("row", "col"),
                          msg = "Le paramètre pct peut prendre les valeurs 'row' ou 'col' (pour ligne ou colonne)")

  # récupération des éléments permettant de calculer les pourcentages
  var_rows <- attr(tab, "params")$var_rows
  var_cols <- attr(tab, "params")$var_cols
  lab_total <- attr(tab, "params")$lab_total

  # pourcentage en colonnes :
  # ==> on divise chaque valeur des colonnes par la somme de la colonne
  #     puis on multiplie par 100 x (nb de variables lignes + 1)
  #     (pour avoir une somme à 100 en cas de groupes et sous groupes)
  if(pct == "col"){

    tab <- tab |>
      dplyr::mutate(dplyr::across(-dplyr::all_of(var_rows),
                                  ~ . / sum(.) * 100 * (length(var_rows) + 1)))

  }

  # pourcentage en lignes :
  # ==> on divise chaque valeur des lignes par la valeur de la colonne contenant l'ensemble
  #     puis on multiplie par 100

  if (pct == "row" && length(var_cols) > 0) {
    if (length(var_cols) == 1) {
      col_ensemble <- rlang::sym(lab_total)
    } else if (length(var_cols) == 2) {
      col_ensemble <- rlang::sym(paste0(lab_total, "_", lab_total))
    }

    tab <- tab |>
      dplyr::mutate(dplyr::across(-dplyr::all_of(var_rows), ~ . / !!col_ensemble * 100))
  }


  # mise à jour des différents attributs de l'objet tab
  attr(tab, "state") <- "pct"
  attr(tab, "params")$pct <- pct
  attr(tab, "without_mask") <- tab

  return(tab)

}
