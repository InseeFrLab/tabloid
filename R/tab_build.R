#' Créer un tableau croise
#'
#' @description tab_build() permet construire un tableau croisé : elle calcule une statistique pour plusieurs croisements de
#' variables, en intégrant automatiquement les différentes marges.
#'
#' @param data table (data.frame ou tibble) contenant les données
#' @param var_rows variables lignes (caractère ou facteur)
#' @param var_cols variables colonnes (caractère ou facteur)
#' @param var_stat variable d'intérêt sur laquelle calculer la statististique (numérique)
#' @param stat statistique à calculer
#'  Ce paramètre peut prendre les valeurs suivantes :
#' \itemize{
#' \item{`"mean"` : moyenne}
#' \item{`"min"` : minimum}
#' \item{`"max"` : maximum}
#' \item{`"median"` : médiane}
#' \item{`"sum"` : somme}
#' \item{`"count"` : effectifs - dans ce cas, laisser le parametre var_stat non renseigné}
#' \item{`"quantile"` : quantile}
#' \item{`"weighted_mean"` : moyenne pondérée}
#' \item{`"weighted_sum"` : somme pondérée}
#' }
#' Pour les statistiques pondérées, renseigner le paramètre var_w avec la variable de pondération à utiliser pour les calculs
#' @param var_w variable de pondération éventuelle
#' @param lab_total libellé pour les colonnes de total
#' @param probs quantile à calculer si la statistique demandée est "quantile" - 0.5 pour la médiane, 0.1 pour le premier décile...
#'
#' @return tableau croisé
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' prenoms |>
#'   filter(annais == "2020" &
# '             prenom %in% c("Charlie", "Camille") &
#'              lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
#'   tab_build(var_rows = lib_reg,
#'             var_cols = c(prenom, sexe),
#'             var_stat = nombre,
#'             lab_total = "Total",
#'             stat = "sum")
tab_build <- function(data,
                      var_rows,
                      var_cols = NULL,
                      var_stat = NULL,
                      stat = "count",
                      var_w = NULL,
                      probs = NULL,
                      lab_total = "Ensemble") {

  # Vérification des paramètres ====
assertthat::assert_that(!missing(var_rows),
                        msg = "Il faut renseigner au moins une variable en ligne dans le paramètre var_rows")

  var_rows <- print_arg({{ var_rows }})
  var_cols <- print_arg({{ var_cols }})

  assertthat::assert_that(length(var_cols) < 3,
    msg = "Merci de ne renseigner qu'une ou deux variables en colonne"
  )

  assertthat::assert_that(length(intersect(var_rows, var_cols)) == 0,
    msg = glue::glue("Les variables
                     {glue::glue_collapse({intersect(var_rows, var_cols)},
                     sep=', ', last = ' et ')}
                     sont présentes à la fois en lignes et en colonnes")
  )
  assertthat::assert_that(all(var_rows %in% names(data)),
    msg = glue::glue("Les variables
                     {glue::glue_collapse({var_rows[which(!var_rows %in% names(data))]},
                     sep=', ', last = ' et ')}
                     ne sont pas dans la table de données")
  )
  assertthat::assert_that(all(var_cols %in% names(data)),
    msg = glue::glue("Les variables
                     {glue::glue_collapse({var_cols[which(!var_cols %in% names(data))]},
                     sep=', ', last = ' et ')} ne sont pas dans la table de données")
  )

  assertthat::assert_that(
    all(data |>
      dplyr::select(dplyr::all_of(c(var_rows, var_cols))) |>
      purrr::map_lgl(~ !is.numeric(.x))),
    msg = glue::glue("Les variables
                     {glue::glue_collapse({names(which(data |>
                     dplyr::select(dplyr::all_of(c(var_rows, var_cols))) |>
                     purrr::map_lgl(~is.numeric(.x))))},
                     sep=', ', last = ' et ')}
                     sont numériques et ne peuvent pas être utilisées en ligne ou en colonne")
  )

  if(length(var_cols) == 2) {
    assertthat::assert_that(length(var_rows) >= 1,
                            msg = "Aucune variable en ligne renseignée mais deux variables en colonnes
                            utiliser var_rows plutôt que var_cols")
    }

  assertthat::assert_that(!(length(var_rows) == 0 & length(var_cols) == 0),
                          msg = "Il est nécessaire de renseigner au moins une variable en ligne ou une variable en colonne
                          Les deux paramètres var_cols et var_rows ne peuvent être non renseignés tous les deux")

  # vérification sur la variable de calcul
  if (!missing(var_stat)) {
    assertthat::assert_that(rlang::as_name(rlang::enquo(var_stat)) %in% names(data),
      msg = glue::glue("La variable
                       {rlang::as_name(rlang::enquo(var_stat))}
                       n'est pas dans la table de données")
    )
  }
  if (!missing(var_stat)) {
    assertthat::assert_that(is.numeric(data |> dplyr::pull({{ var_stat }})),
      msg = "Le paramètre var_stat doit être une variable numérique"
    )
  }


  assertthat::assert_that(is.character(lab_total))

  # vérifications sur les statistiques demandées
  assertthat::assert_that(stat %in% c(
    "mean", "weighted_mean", "min", "max", "sum", "weighted_sum",
    "quantile", "count", "median"
  ),
  msg = "La statistique demandée n'est pas disponible\n merci de renseigner une valeur parmi les suivantes : mean, weighted_mean, min, max, median, sum, weighted_sum, quantile, count")

  if (stat == "weighted_mean") assertthat::assert_that(!missing(var_w),
                                                       msg = "Renseigner var_w")
  if (stat == "weighted_sum") assertthat::assert_that(!missing(var_w),
                                                      msg = "Renseigner var_w")

  if (stat %in% c("weighted_mean", "weighted_sum")) assertthat::assert_that(
    is.numeric(data |> dplyr::pull({{ var_w }})),
    msg = "La variable de pondération doit être numérique"
    )

  if (stat == "quantile") assertthat::assert_that(!missing(probs), msg = "Indiquer le quantile souhaité dans le paramètre probs \npar exemple, pour le 9ème décile : probs = 0.9")
  if (stat == "quantile") assertthat::assert_that(is.numeric(probs), msg = "Le paramètre probs doit être un numérique compris entre 0 et 1 \npar exemple, pour le 9ème décile : probs = 0.9")
  if (stat == "quantile") assertthat::assert_that(probs >= 0 & probs <= 1, msg = "Le paramètre probs doit être un numérique compris entre 0 et 1 \npar exemple, pour le 9ème décile : probs = 0.9")

  if (stat == "count" && !missing(var_stat)) {
    warning("Attention : var_stat renseignée mais inopérante pour stat = count")
  }
  if (!stat %in% c(
    "weighted_mean", "weighted_sum") && !is.null(var_w)) {
    warning("Attention : var_w renseignée mais inopérante pour la stat renseignée")
  }


  # 1. Préparation ====

  if (dplyr::is.grouped_df(data)){

    message("Attention ! Les données sont groupées - pour le calcul, elles seront dégroupées
            Pour construire les différents groupes, utiliser les paramètres var_rows et var_cols")

    data <- data |>
      dplyr::ungroup()

    }


  ## 1.a) récupération de l'ensemble des groupes et sous-groupes à partir des variables lignes/colonnes ----
  var_groupes <- c(var_rows, var_cols)
  nb_var_groupes <- length(var_groupes)
  liste_groupes <- get_groups_list(var_groupes)

  ## 1.b) récupération des modalités de chaque variable ----
  ## si une variable est de type facteur : récupération des différents niveaux renseignés
  ## sinon : récupération des différentes modalités dans l'ordre dans lequel elles apparaissent dans la table
  ## ajout du niveau "total" pour chaque variable (poour les tris)
  modalites <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_groupes) & !dplyr::where(is.factor),
                  ~forcats::fct_inorder(.x))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_groupes) & dplyr::where(is.factor),
                                ~forcats::fct_drop(.x))) |>
    dplyr::select(dplyr::all_of(var_groupes)) |>
    purrr::map(~c(levels(.x), lab_total))


  ## 1.c) mise au format caractère de l'ensemble des variables de groupes et sous-groupes ----
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_groupes), as.character))


  ## 1.d) gestion des valeurs manquantes ----
  # s'il y a des valeurs manquantes dans les variables de groupes et sous-groupes
  # --> remplacement par "manquant" pour mieux les visualiser en sortie
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_groupes), ~ tidyr::replace_na(., "manquant")))

  # pour le calcul des effectifs (pondérés ou non)
  # ==> on se ramène à une somme
  stat_init <- stat
  if (stat == "count") {
    if (!missing(var_w)) {
      data <- data |> dplyr::mutate(eff = {{ var_w }})
    } else {
      data <- data |> dplyr::mutate(eff = 1)
    }
    var_stat <- rlang::quo(eff)
    stat <- "sum"
  }


  # 2. Calcul des résultats pour l'ensemble des groupes et sous-groupes ====
  # pour chaque groupe et sous-groupes :
  #   calcul de la statistique demandée, avec les options précisées
  #   + ajout du total sur l'ensemble des données
  tableau <- liste_groupes |>
    purrr::map(~ data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.x))) |>
      dplyr::summarise(
        indicateur = calc_stat({{ var_stat }},
          stat = stat,
          var_w = {{ var_w }},
          probs = probs
        ),
        .groups = "drop"
      )) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(data |>
      dplyr::summarise(indicateur = calc_stat({{ var_stat }},
                                              stat = stat,
                                              var_w = {{ var_w }},
                                              probs = probs
                                              ))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(var_groupes[-nb_var_groupes])))


  # 3. Mise en forme du tableau ====

  ## 3.a) ajout du libellé pour les totaux ----
  tableau <- tableau |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(., lab_total)))


  ## 3.b) passage des variables en colonnes si demandé ----

   if (length(var_cols) > 0) {

     ### 3.b).i) calcul des résultats pour les croisements manquants ----

     # identification des croisements manquants
     croisements_manquants <- get_add_list(var_groupes, length(var_cols))

     # calcul de la statistique demandée pour les croisements manquants
     ajout <- croisements_manquants |>
       purrr::map(~ data |>
                    dplyr::group_by(dplyr::across(all_of(.x))) |>
                    dplyr::summarise(
                      indicateur = calc_stat({{ var_stat }},
                                             stat = stat,
                                             var_w = {{ var_w }},
                                             probs = probs
                      ),
                      .groups = "drop"
                    )) |>
       dplyr::bind_rows()

     ## 3.b).ii) ajout des résultats pour les croisements manquants ----

    tableau <- tableau |>
      dplyr::bind_rows(ajout) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(., lab_total))) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(var_cols),
        values_from = indicateur,
        values_fill = 0
      )

  } else {

    # si aucun variable n'est demandée en colonne
    # la dernière colonne correspond à la statistique demandée
    tableau <- tableau |>
      dplyr::rename_with(
        .cols = tidyselect::all_of("indicateur"),
        .fn = ~ stringr::str_replace(.x, "indicateur", stat_init)
      )

  }

  tableau <- tableau |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_rows),
                                ~ factor(.x, levels = modalites[[dplyr::cur_column()]]))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(var_rows))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_rows), as.character))

  if (length(var_cols) == 1) {

    tableau <- tableau |>
      dplyr::select(dplyr::all_of(var_rows), modalites[[var_cols]])
  }

  if (length(var_cols) == 2) {

    liste_colonnes <- tidyr::expand_grid(modalites[[var_cols[1]]], modalites[[var_cols[2]]]) |>
      purrr::set_names("x", "y") |>
      dplyr::mutate(mod = glue::glue("{x}_{y}")) |>
      dplyr::pull(mod)

    tableau <- tableau |> dplyr::select(dplyr::all_of(var_rows), dplyr::any_of(liste_colonnes))

  }


  # 4. tableau_count
  # On calcule dès tab_build un attribut de comptage des lignes concernées
  # un peu coûteux à calculer mais gain précieux pour tab_mask car permet de ne pas traîner la table x

  # 4.1. Préparation ====

  tableau_count <- liste_groupes |>
    purrr::map(~ data |>
                 dplyr::group_by(dplyr::across(dplyr::all_of(.x))) |>
                 dplyr::summarise(
                   indicateur = dplyr::n(),
                   .groups = "drop"
                 )) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(data |>
                       dplyr::summarise(indicateur = dplyr::n())) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(var_groupes[-nb_var_groupes])))


  # 4.3. Mise en forme du tableau ====

  ## 4.3.a) ajout du libellé pour les totaux ----
  tableau_count <- tableau_count |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(., lab_total)))

  ## 4.3.b) passage des variables en colonnes si demandé ----

  if (length(var_cols) > 0) {

    ### 4.3.b).i) calcul des résultats pour les croisements manquants ----

    # calcul de la statistique demandée pour les croisements manquants
    ajout <- croisements_manquants |>
      purrr::map(~ data |>
                   dplyr::group_by(dplyr::across(all_of(.x))) |>
                   dplyr::summarise(
                     indicateur = dplyr::n(),
                     .groups = "drop"
                   )) |>
      dplyr::bind_rows()

    ## 4.3.b).ii) ajout des résultats pour les croisements manquants ----

    tableau_count <- tableau_count |>
      dplyr::bind_rows(ajout) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(., lab_total))) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(var_cols),
        values_from = indicateur,
        values_fill = 0
      )

  } else {

    # si aucun variable n'est demandée en colonne
    # la dernière colonne correspond à la statistique demandée
    tableau_count <- tableau_count |>
      dplyr::rename_with(
        .cols = tidyselect::all_of("indicateur"),
        .fn = ~ stringr::str_replace(.x, "indicateur", "sum")
      )

  }

  tableau_count <- tableau_count |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_rows),
                                ~ factor(.x, levels = modalites[[dplyr::cur_column()]]))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(var_rows))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var_rows), as.character))

  if (length(var_cols) == 1) {

    tableau_count <- tableau_count |>
      dplyr::select(dplyr::all_of(var_rows), modalites[[var_cols]])
  }

  if (length(var_cols) == 2) {

    liste_colonnes <- tidyr::expand_grid(modalites[[var_cols[1]]], modalites[[var_cols[2]]]) |>
      purrr::set_names("x", "y") |>
      dplyr::mutate(mod = glue::glue("{x}_{y}")) |>
      dplyr::pull(mod)

    tableau_count <- tableau_count |> dplyr::select(dplyr::all_of(var_rows), dplyr::any_of(liste_colonnes))

  }

  # --------------------------
  # 5. Rendu final : objet tab ====

  attr(tableau, "init") <- tableau
  attr(tableau, "without_mask") <- tableau
  attr(tableau, "count") <- tableau_count
  attr(tableau, "state") <- "build"
  attr(tableau, "type") <- "tab"
  attr(tableau, "params") <- list(
    var_cols = var_cols,
    var_rows = var_rows,
    modalites = modalites,
    stat = stat_init,
    var_stat = rlang::as_name(rlang::enquo(var_stat)),
    pct = "-",
    var_w = if(!missing(var_w)){rlang::as_name(rlang::enquo(var_w))} else {"NULL"},
    lab_total = lab_total,
    probs = probs
  )

  # class(tableau) <- c(class(tableau), "tab")


  # tab <- structure(
  #   list(
  #     core_init = tableau, # tableau du premier calcul issu de tab_build
  #     core = tableau, # tableau à afficher
  #     core_last = tableau, # tableau du dernier calcul courant
  #     count = tableau_count,
  #     state = "build",
  #     param = list(
  #       var_cols = var_cols,
  #       var_rows = var_rows,
  #       modalites = modalites,
  #       stat = stat_init,
  #       var_stat = rlang::as_name(rlang::enquo(var_stat)),
  #       pct = "-",
  #       var_w = if(!missing(var_w)){rlang::as_name(rlang::enquo(var_w))} else {"NULL"},
  #       lab_total = lab_total,
  #       probs = probs
  #     )
  #   ),
  #   class = "tab"
  # )

  return(tableau)
}
