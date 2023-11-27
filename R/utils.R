#' recup liste ajout
#'
#' @param vecteur numeric vector
#' @param nbcol integer
#'
#' @return list
#' @noRd
#'
#' @examples get_add_list(vecteur = 1:4, nbcol = 2)
get_add_list <- function(vecteur, nbcol){
  l <- length(vecteur)

  if (nbcol == 1){
    res <- (-l+1):(-1) |>
      purrr::map(~vecteur[.x:-(l-1)])
  }
  if (nbcol == 2){
    res <- (l-nbcol):1 |>
      purrr::map(~get_groups_list(vecteur[-(.x:(l-nbcol))])) |> purrr::flatten()
  }

  return(res)
}




#' recup liste groupes
#'
#' @param vecteur
#'
#' @return list de taille length(vecteur)
#' @noRd
#'
#' @examples get_group_liste(1:4)
get_groups_list <- function(vecteur){
  (length(vecteur):1) |> purrr::map(~vecteur[1:.x])
}


#' modifier libelles des variables lignes ou colonnes (pour render et xlsx)
#'
#' @param df data.frame
#' @param new_lab_col nouveaux libellés des colonnes
#' @param new_lab_row nouveaux libellés des lignes
#' @param old_lab_col anciens libellés des colonnes
#' @param old_lab_row anciens libellés des lignes
#' @param modalites liste des modalités
#' @param lab_total libellé des marges intermédiaires ou totales
#'
#' @return data.frame
#' @noRd
modif_libelles <- function(df,
                           new_lab_col = NULL,
                           new_lab_row = NULL,
                           old_lab_col,
                           old_lab_row,
                           modalites,
                           lab_total){

  if(!missing(new_lab_row) & !is.null(new_lab_row)){

    names(df)[which(names(df) %in% old_lab_row)] <- new_lab_row

  }


  if(!missing(new_lab_col) & !is.null(new_lab_col)){

    new_lab_col <- old_lab_col |>
      purrr::map(~ if(is.null(new_lab_col[[.x]])) {
        new_lab_col[[.x]] <- modalites[[.x]]
      } else {
        new_lab_col[[.x]] <- c(new_lab_col[[.x]], lab_total )})

    names(new_lab_col) <- old_lab_col

    df <- purrr::reduce(old_lab_col,
                        .f = ~ .x |> rename_col_mod(modalites, new_lab_col, .y),
                        .init = df)
  }

  return(df)
}

#' recup modalites var colonnes
#'
#' @param df data.frame
#' @param num_col integer
#' @param nb_var_lignes integer
#'
#' @return modalites
#' @noRd
get_var_cols_mod <- function(df, num_col, nb_var_lignes){

  names(df)[-(1:nb_var_lignes)]|>
    stringr::str_split("_") |>
    purrr::map_chr(num_col)

}


#' get index col total
#'
#' @param tab
#'
#' @return integer
#' @noRd
get_index_col_total <- function(tab){

  stringr::str_count(colnames(tab), attr(tab, "params")$lab_total)

}

#' get index row total
#'
#' @param tab
#'
#' @return integer
#' @noRd
get_index_row_total <- function(tab){

  rowSums(tab[, attr(tab, "params")$var_rows] == attr(tab, "params")$lab_total)

}



#' print arg
#'
#' @param arg
#'
#' @return character
#' @noRd
print_arg <- function(arg){

  arg <- as.character(rlang::quo_get_expr(rlang::enquo(arg)))

  if(any(arg %in% c("c", "[[", ".data"))){
    arg <- arg[-which(arg %in% c("c", "[[", ".data"))]
  }

  return(arg)

}

#' extrait_cellules_brutes dans le cadre d'un %age ligne ou colonne
#'
#' @param df df
#' @param params params
#'
#' @return numeric matrix
#'
#' @noRd
get_cells <- function(df, params) {

  # pourcentages en colonnes
  # on conserve les lignes où il n'y a pas lab_total sur l'une des lignes pour reconstituer la série brute
  # + on conserve uniquement les colonnes numériques

  if(params$pct == "col") {
    cells <- df |>
      dplyr::filter(dplyr::if_all(params$var_rows, ~. != params$lab_total)) |>
      dplyr::select(dplyr::where(is.numeric)) |>
      as.matrix()
  }

  # pourcentages en lignes
  # on ne garde pas les colonnes correspondant aux marges intermédiaires
  # on ne garde que les colonnes qui correspondent aux chiffres à modifier

  else if (params$pct == "row") {
    cells <- df |>
      dplyr::select(-tidyselect::contains(params$lab_total)) |>
      dplyr::select(-params$var_rows) |>
      as.matrix()
  }
  else {
    stop("extrait_cellules_brutes n'est pas sensée s'appliquer si pct = -")
  }
  return(cells)
}


#' reconstitue_core
#'
#' @param param attribut param d'un objet tab
#' @param core attribut core d'un objet tab
#' @param without_mask numeric matrix
#' @param with_mask numeric matrix
#'
#' @return data.frame
#'
#' @noRd
reconstitue_core <- function(param,
                             core,
                             without_mask,
                             with_mask){

  content <- without_mask
  content[is.na(with_mask)] <- NA

  if(param$pct == "col") {

    res <-
      # à gauche, les colonnes tab$param$var_rows
      core |>
      dplyr::select(param$var_rows) |>
      # mais sans les lignes où il y a lab_total sur l'une des lignes
      dplyr::filter(dplyr::if_all(param$var_rows, ~. != param$lab_total)) |>
      dplyr::bind_cols(
        # on colle à droite le contenu souhaité
        content |>
          as.data.frame()
      ) |>
      # on remet les lignes non considérées
      dplyr::bind_rows(
        core |>
          # on conserve les lignes où il n'y a pas lab_total sur l'une des lignes pour reconstituer la série brute
          dplyr::filter(dplyr::if_any(param$var_rows, ~. == param$lab_total)))

  } else if (param$pct == "row") {

    # à gauche, les colonnes param$var_rows (toutes lignes)
    res <- core |>
      dplyr::select(param$var_rows) |>
      # on colle à droite le contenu souhaité
      dplyr::bind_cols(
        content |>
          as.data.frame()
      ) |>
      # et on recolle encore à droite les marges
      dplyr::bind_cols(
        core |>
          dplyr::select(tidyselect::contains(param$lab_total))
      )
  }

  return(res)
}

#' Alternative à la fonction round de R qui arrondit une fois sur deux de façon inférieure
#'
#' @param x numeric
#' @param digits integer
#'
#' @return numeric
#' @noRd
#'
#' @examples
#' round2(seq(0.5, 9.5, 1))
#' round(seq(0.5, 9.5, 1))
round2 <- function(x, digits = 0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
