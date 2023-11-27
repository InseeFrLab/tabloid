#' Arrondir les résultats d'un tableau
#'
#' @description tab_round() permet d'arrondir les données des cellules, et éventuellement de gérer la cohérence
#' des sommes à 100 % pour les pourcentages lignes et colonnes, en proposant différentes règles de décision.
#'
#'
#' @param tab tableau croisé issu de tab_build
#' @param n nombre de décimales à conserver - par défaut : 0 (arrondi à l'entier)
#'  Ce paramètre peut prendre des valeurs positives, ou négatives pour arrondir à la dizaine, la centaine, etc :
#' \itemize{
#' \item{`0, 1, 2, ...` : entier, 1 décimale, 2 décimales, etc...}
#' \item{`-1` : arrondi à la dizaine}
#' \item{`-2` : arrondi à la centaine}
#' \item{`"etc..."` : ...}
#' }
#' @param guarantee_100 pour les arrondis de pourcentages lignes ou colonnes,
#' faut-il gérer la cohérence des arrondis au niveau n ? - par défaut : FALSE
#' @param option option à retenir pour le calcul de la garantie à 100  - par défaut : "lowest_lie"
#'  Ce paramètre peut prendre les valeurs suivantes :
#' \itemize{
#' \item{`"lowest_lie"` : ...}
#' \item{`"closest"` : ...}
#' }
#' @param verbose affichage ou non des détails sur ce qui est fait pour guarantee_100 - par défaut : FALSE
#'
#' @return tableau avec cellules arrondies
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' prenoms |>
#'   filter(annais == "2020",
#'          lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
#'   tab_build(var_rows = c(lib_reg, sexe),
#'             var_cols = prenom,
#'             var_stat = nombre,
#'             lab_total = "Total",
#'             stat = "sum") |>
#'   tab_pct(pct = "row") |>
#'   tab_round(n = 0,
#'             guarantee_100 = TRUE,
#'             option = "closest",
#'             verbose = TRUE)
tab_round <- function(tab,
                      n = 0,
                      guarantee_100 = FALSE,
                      option = "lowest_lie",
                      verbose = FALSE){

  # vérifications des paramètres de la fonction
  assertthat::assert_that(attr(tab, "type") == "tab", msg = "tab_round s'utilise sur un objet issu de la fonction tab_build")

  assertthat::assert_that(attr(tab, "state") %in% c("build", "pct", "mask"),
                          msg = "tab_round s'utilise après tab_build, tab_pct ou tab_mask")
  assertthat::assert_that(round2(abs(n), digits = 0) == abs(n),
                          msg = "n doit être un entier positif ou négatif")
  assertthat::assert_that(!guarantee_100 || (guarantee_100 && attr(tab, "params")$pct != "-"),
                          msg = "L'option guarantee_100 ne s'applique qu'après tab_pct")

  # # Initialisation de l'objet en sortie (tab_), duquel on ne modifiera que l'attribut core et core_last sans toucher à core_init
  # tab_ <- tab

  params <- attr(tab, "params")
  state <- attr(tab, "state")
  pct <- params$pct
  colnames <- colnames(tab)
  core <- tab

  if(guarantee_100) {

      to_modify <- get_cells(attr(tab, "without_mask"), params)
      to_modify_with_mask <- get_cells(tab, params)


    if(pct == "col") {
      message("guarantee_100 : la mise en cohérence des marges en lignes reste à effectuer par l'utilisateur (ok en colonnes)")

      # on applique colonne à colonne la fonction (TODO peu élégant, à améliorer)
      for(k in 1:ncol(to_modify)){
        # k <- 1
        to_modify[, k] <- guarantee_sum100(to_modify[, k],
                                           n,
                                           option,
                                           verbose)
      }
    }

    else if (pct == "row")  {

      message("guarantee_100 : la mise en cohérence des marges en colonnes reste à effectuer par l'utilisateur (ok en lignes)")

      # on applique ligne à ligne la fonction (TODO peu élégant, à améliorer)
      for(k in 1:nrow(to_modify)){
        to_modify[k, ] <- guarantee_sum100(to_modify[k, ],
                                           n,
                                           option,
                                           verbose)
      }
    }

      # reconstitution du tableau
      core <- reconstitue_core(params,
                               tab,
                               to_modify,
                               to_modify_with_mask) |>
        dplyr::select(dplyr::all_of(colnames)) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(params$var_rows),
                                    ~ factor(.x, levels = params$modalites[[dplyr::cur_column()]]))) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(params$var_rows))) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(params$var_rows), as.character))

  }


  # dans tous les cas : calcul des arrondis sur l'ensemble des variables numériques du tableau
  round <- core |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~round2(.x, digits = n)))


  attributes(round) <- attributes(tab)
  attr(round, "state") <- "round"
  return(round)

}
