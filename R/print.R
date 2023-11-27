#' #' print d'un objet de type tab
#' #'
#' #' @param tab objet issu de tab_build
#' #'
#' #' @return affiche le tableau dans la console
#' #' @export
#' print.tab <- function(tab, ...){
#'
#'   # # vérification des paramètres
#'   # assertthat::assert_that(inherits(tab, "tab"))
#'
#'   print(unclass(tab), ...)
#'
#' }


#'
#'
#' #' tail d'un objet tab
#' #'
#' #' @param tab objet issu de tab_build
#' #'
#' #' @return dernières lignes du tableau
#' #' @export
#' tail.tab <- function(tab, ...){
#'
#'   # tab$core |> utils::tail()
#'   utils::tail(unclass(tab), ...)
#'
#' }
#'
#' #' head d'un objet tab
#' #'
#' #' @param tab objet issu de tab_build
#' #'
#' #' @return premières lignes du tableau
#' #' @export
#' head.tab <- function(tab, ...){
#'
#'  # tab$core |> utils::head()
#'   utils::head(unclass(tab), ...)
#'
#' }
#'
#'
#'
#'
