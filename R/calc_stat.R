
#' calcul de statistique sur une variable numerique
#'
#' @param x variable numerique
#' @param stat statistique a calculer
#' @param var_w variable de ponderation dans le cas du calcul d une moyenne ponderee
#' @param na.rm faut-il supprimer les valeurs manquantes ? (par défaut : TRUE)
#' @param probs quantile à calculer si la statistique demandée est "quantile" - 0.5 pour la médiane, 0.1 pour le premier décile...
#'
#' @return numeric
#' @noRd
#' @examples calc_stat(1:10, "weighted_mean", 1:10)
calc_stat <- function(x,
                      stat,
                      var_w = NULL,
                      na.rm = TRUE,
                      probs = NULL){

  # vérifications des paramètres
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(stat %in% c("mean", "weighted_mean", "min", "max", "sum", "weighted_sum", "quantile", "median"))
  if (stat %in% c("weighted_mean", "weighted_sum")) assertthat::assert_that(!missing(var_w))
  if (stat %in% c("weighted_mean", "weighted_sum")) assertthat::assert_that(is.numeric(var_w))
  if(stat == "quantile") assertthat::assert_that(!missing(probs) & !is.null(probs))
  if(stat == "quantile") assertthat::assert_that(length(probs) == 1)
  if(stat == "quantile") assertthat::assert_that(is.numeric(probs))
  if(stat == "quantile") assertthat::assert_that(probs>=0 & probs<=1)

  # calcul de la statistique demandée
  switch(stat,
         mean = mean(x, na.rm = na.rm),
         min = min(x, na.rm = na.rm),
         max = max(x, na.rm = na.rm),
         median = median(x, na.rm = na.rm),
         sum = sum(x, na.rm = na.rm),
         weighted_mean = stats::weighted.mean(x, var_w, na.rm = na.rm),
         weighted_sum = sum(x * var_w, na.rm = TRUE),
         quantile = stats::quantile(x, probs = probs, names = FALSE)
  )
}


