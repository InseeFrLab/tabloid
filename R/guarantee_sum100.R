#' guarantee_sum100
#'
#' @param v numeric vector
#' @param n integer (arrondi souhaité en nb de chiffres après la virgule)
#' @param option 'lowest_lie' pour minimiser l'écart en %age avec la valeur initiale ou 'closest' pour minimiser l'écart en valeur absolue
#' @param verbose logical
#'
#' @return numeric vector arrondi à n décimales
#' @noRd
guarantee_sum100 <- function(v,
                             n,
                             option = "lowest_lie",
                             verbose = TRUE){
  # v <- c(19.39, 43.61, 17.5, 19.5)
  # v <- c(0.1, 9.8, 70.5, 19.6)
  # n <- 0
  # option <- "closest"
  # verbose <- TRUE

  assertthat::assert_that(option %in% c(
    "lowest_lie", "closest"
  ),
  msg = "L'option doit être renseignée à 'lowest_lie' (valeur par défaut) ou 'closest'")

  res <- round2(v, n)
  ecart <- calcule_ecart(v, n, verbose)
  # On veut disqualifier les chiffres qui sont à zéro ou bien ceux que l'on modifierait de plus d'une unité (en fait 10^-n)
  i_disqual <- which((abs(round2(v, n) + ecart - v) > 10^(-n)) | (v == 0))

  if(ecart != 0 & length(i_disqual) < length(v)){

    if(option == "lowest_lie"){

      if(abs(ecart) == 10 ^ (-n)){
        ctr <- 100 * ((round2(v, n) + ecart) / v - 1)
        i_lowest_lie <- setdiff(order(abs(ctr)), i_disqual)[1]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i_lowest_lie], n + 2), " en position ", i_lowest_lie, " par ", round2(res[i_lowest_lie], n) + ecart))
        }
        res[i_lowest_lie] <- res[i_lowest_lie] + ecart
      } else if (abs(ecart) == 2 * 10 ^ (-n)){
        ctr <- 100 * ((round2(v, n) + ecart) / v - 1)
        i1_lowest_lie <- setdiff(order(abs(ctr)), i_disqual)[1]
        i1_lowest_lie <- setdiff(order(abs(ctr)), i_disqual)[2]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i1_lowest_lie], n + 2), " en position ", i1_lowest_lie, " par ", round2(res[i1_lowest_lie], n) + ecart / 2))
          message(paste0(">> On change le ", round2(res[i2_lowest_lie], n + 2), " en position ", i2_lowest_lie, " par ", round2(res[i2_lowest_lie], n) + ecart / 2))
        }
        res[i1_lowest_lie] <- res[i1_lowest_lie] + ecart / 2
        res[i2_lowest_lie] <- res[i2_lowest_lie] + ecart / 2
      } else {
        if(verbose){
          message("L'écart est à répartir sur plus de 2 chiffres : arbitrairement on met tout l'écart sur la valeur sur laquelle on mentirait le moins")
        }
        ctr <- 100 * ((round2(v, n) + ecart) / v - 1)
        i_lowest_lie <- setdiff(order(abs(ctr)), i_disqual)[1]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i_lowest_lie], n + 2), " en position ", i_lowest_lie, " par ", round2(res[i_lowest_lie], n) + ecart))
        }
        res[i_lowest_lie] <- res[i_lowest_lie] + ecart
      }
    }

    else if(option == "closest"){

      if(abs(ecart) == 10 ^ (-n)){
        i_closest <- setdiff(order(abs(v - (res + ecart))), i_disqual)[1]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i_closest], n + 2), " en position ", i_closest, " par ", round2(res[i_closest], n) + ecart))
        }
        res[i_closest] <- res[i_closest] + ecart
      } else if (abs(ecart) == 2 * 10 ^ (-n)){
        i1_closest <- setdiff(order(abs(v - (res + ecart))), i_disqual)[1]
        i2_closest <- setdiff(order(abs(v - (res + ecart))), i_disqual)[2]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i1_closest], n + 2), " en position ", i1_closest, " par ", round2(res[i1_closest], n) + ecart / 2))
          message(paste0(">> On change le ", round2(res[i2_closest], n + 2), " en position ", i2_closest, " par ", round2(res[i2_closest], n) + ecart / 2))
        }
        res[i1_closest] <- res[i1_closest] + ecart / 2
        res[i2_closest] <- res[i2_closest] + ecart / 2
      } else {
        if(verbose){
          message("L'écart est à répartir sur plus de 2 chiffres : arbitrairement on met tout l'écart sur la valeur sur laquelle on mentirait le moins")
        }
        i_closest <- setdiff(order(abs(v - (res + ecart))), i_disqual)[1]
        if(verbose){
          message(paste0(">> On change le ", round2(v[i_closest], n + 1), " en position ", i_closest, " par ", round2(res[i_closest], n) + ecart))
        }
        res[i_closest] <- res[i_closest] + ecart
      }
    }

  } else{
    if(verbose){
      if(ecart == 0){
        message(">> La somme fait déjà 100 avec le bon niveau d'arrondi.")
      }
      if(length(i_disqual) == length(v)){
        message("La règle de décision n'a trouvé aucun chiffre potentiellement modifiable.")
      }
    }
  }

  return(res)
}

#' calcule_ecart, fonction appelée dans guarantee_sum100
#'
#' @param v numeric vector à arrondir
#' @param n integer (arrondi souhaité en nb de chiffres après la virgule)
#' @param verbose logical
#'
#' @return logical
#' @noRd
calcule_ecart <- function(v, n, verbose = TRUE){
  # v <- 100 * rep(1/3, 3)
  # n <- 1
  ar <- sum(round2(v, n), na.rm = TRUE)
  res <- round2(100 - ar, n)
  if(verbose){
    # message(round2(v, n))
    if(res != 0){
      message(paste0(">> La somme fait ", ar))
    }
  }
  return(res)
}
