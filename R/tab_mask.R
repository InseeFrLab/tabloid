#' Masquer les cellules d'un tableau
#'
#' @description tab_mask() permet de masquer les cellules d'un tableau :
#' - les cellules non-nulles inférieures à un seuil, en nombre de lignes ou en somme de la variable var_stat
#' - les cellules correspondant aux marges
#'
#' @param tab tableau croisé issu de tab_build
#' @param threshold_count nombre de lignes de la table initiale en-deça duquel la valeur est masquée (exclusif) - par défaut : NULL
#' @param threshold_sum à renseigner pour stat = sum, valeur de la cellule en-deça de laquelle la valeur est masquée (exclusif) - par défaut : NULL
#' @param mask_total masquage ou non des totaux - par défaut : aucun
#' Ce paramètre peut prendre les valeurs suivantes :
#' \itemize{
#' \item{`"cols"` : masquage des totaux en colonnes}
#' \item{`"rows"` : masquage des totaux en lignes}
#' \item{`c("rows", "cols")` : masquage de l'ensemble des totaux}
#' }
#' @param verbose affichage ou non du nombre de cellues masquées par la fonction - par défaut : TRUE
#'
#' @return tableau avec cellules blanchies
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' prenoms |>
#'   filter(annais == "2020" &
#'        prenom %in% c("Charlie", "Camille") &
#'                 lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
#'   tab_build(var_rows = lib_reg,
#'             var_cols = c(prenom, sexe),
#'             var_stat = nombre,
#'             lab_total = "Total",
#'             stat = "sum") |>
#'   tab_mask(threshold_count = 5,
#'            threshold_sum = 100,
#'            mask_total = c("rows", "cols"),
#'            verbose = TRUE)
tab_mask <- function(tab,
                     threshold_count = NULL,
                     threshold_sum = NULL,
                     mask_total = "",
                     verbose = TRUE){

  # vérifications des paramètres de la fonction
  assertthat::assert_that(attr(tab, "type") == "tab", msg = "tab_mask s'utilise sur un objet issu de la fonction tab_build")
  assertthat::assert_that(attr(tab, "state") %in% c("build", "pct", "round"),
                          msg = "tab_mask s'utilise après tab_build, tab_pct ou tab_round")

  assertthat::assert_that(is.null(threshold_count) ||
                            (threshold_count > 0 &&
                               round(threshold_count, digits = 0) == threshold_count),
                          msg = "threshold_count doit être entier strictement positif")
  assertthat::assert_that(is.null(threshold_sum) ||
                            (is.numeric(threshold_sum)),
                          msg = "threshold_sum doit être numérique")
  assertthat::assert_that(!is.null(threshold_count) || !is.null(threshold_sum) || all(mask_total != ""),
                          msg = "Vous n'avez renseigné aucun élément à masquer")
  assertthat::assert_that(is.null(threshold_sum) ||
                            (!is.null(threshold_sum) && attr(tab, "params")$stat %in% c("sum", "weighted_sum")),
                          msg = "threshold_sum n'a de sens que pour stat = sum ou weighted_sum et fera le calcul sur la variable var_stat")


  # On initialise l'objet que l'on retournera à la fin
  # Cet objet ressemblera beaucoup à l'objet tab initial, mais on aura modifié l'attribut core (on ne touche pas à core_last ni core_init)
  # tab_ <- tab
  #   temp1 <- temp2 <- res_count <- NULL

  params <- attr(tab, "params")
  count <- attr(tab, "count")
  init <- attr(tab, "init")

  to_modif <- tab |>
    dplyr::select(-params$var_rows) |>
    as.matrix()

  temp1 <- count |>
    dplyr::select(dplyr::where(is.numeric)) |>
    as.matrix()

  if(verbose){
    message(paste0(sum(temp1 == 0), " cellules ne sont pas masquées mais concernent zéro ligne."))
  }

  if(!is.null(threshold_count)){

    # On masque si la cellule concerne moins de threshold lignes dans la table d'origine
    if(verbose){
      message(paste0(sum(temp1 > 0 & temp1 < threshold_count), " cellules sont masquées car concernent trop peu de lignes."))
    }
    to_modif[temp1 < threshold_count & temp1 != 0] <- NA
  }

  if(!is.null(threshold_sum)){

    temp2 <- init |>
      dplyr::select(dplyr::where(is.numeric))

    if(verbose){
      if(!is.null(threshold_count)) {
        pos_mask_1 <- temp1 < threshold_count & temp1 != 0
        pos_mask_2 <- temp2 > 0 & temp2 < threshold_sum
        mask2times <- pos_mask_1 & pos_mask_2
        message(paste0(sum(pos_mask_2) - sum(mask2times), " cellules de plus sont masquées car valeur de la somme trop petite."))
      } else {
        pos_mask_2 <- temp2 > 0 & temp2 < threshold_sum
        message(paste0(sum(pos_mask_2), " cellules sont masquées car valeur de la somme trop petite."))
      }

    }

    to_modif[temp2 < threshold_sum & temp2 != 0] <- NA

  }

  mask <- tab |>
    dplyr::select(params$var_rows) |>
    dplyr::bind_cols(as.data.frame(to_modif))

  # suppression total lignes
  # on conserve les lignes où il n'y a pas lab_total sur l'une des lignes pour reconstituer la série brute
  if("rows" %in% mask_total) {
    mask <- mask |>
      dplyr::filter(dplyr::if_all(params$var_rows, ~. != params$lab_total))
  }

  # suppression total colonnes
  # on supprime les colonnes correspondant aux marges intermédiaires
  if("cols" %in% mask_total) {
    mask <- mask |>
      dplyr::select(- tidyselect::contains(params$lab_total))
  }


  attr(mask, "state") <- "mask"
  return(mask)

}
