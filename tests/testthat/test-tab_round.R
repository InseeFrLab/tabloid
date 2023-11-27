# testthat::test_that("tab_round_simple", {
#
#   core <- structure(list(prenom = c("Basile",
#                                     "Paulin", "Robin", "Zoé", "Ensemble"),
#                          `2000` = c(0.121341512333027, 0.0892714375119327, 0.347896793337189, 0.423891354442917, 0.338526339942503
#                          ),
#                          `2020` = c(0.239218591077403, 0.0980046084804879, 0.457699767186479, 0.590770891420203, 0.428558737774922),
#                          Ensemble = c(0.202461222436684, 0.0952258722632203, 0.40312314113121, 0.50971511631695, 0.388348189723406
#                          )),
#                     row.names = c(NA, -5L),
#                     class = c("tbl_df", "tbl", "data.frame"
#                     ))
#
#   tab <- structure(list(core_init = core,
#                         core = core,
#                         core_last = core,
#                         count = structure(list(prenom = c("Basile", "Paulin", "Robin",
#                                                           "Zoé", "Ensemble"),
#                                                `2000` = c(29L, 7L, 84L, 85L, 205L),
#                                                `2020` = c(64L, 15L, 85L, 90L, 254L),
#                                                Ensemble = c(93L, 22L, 169L, 175L, 459L)),
#                                           row.names = c(NA, -5L),
#                                           class = c("tbl_df", "tbl", "data.frame"
#                                           )),
#                         state = "build",
#                         param = list(var_cols = "annais", var_rows = "prenom",
#                                      modalites = list(prenom = c("Basile", "Paulin", "Robin", "Zoé", "Ensemble"),
#                                                       annais = c("2000", "2020", "Ensemble")),
#                                      stat = "mean",
#                                      var_stat = "part",
#                                      pct = "-",
#                                      var_w = "NULL",
#                                      lab_total = "Ensemble",
#                                      probs = NULL)),
#                    class = "tab")
#
#   testthat::expect_equal(
#     tab |>
#       tab_round(n = 3) |>
#       print(),
#     structure(list(prenom = c("Basile", "Paulin", "Robin", "Zoé",
#                               "Ensemble"),
#                    `2000` = c(0.121, 0.089, 0.348, 0.424, 0.339),
#                    `2020` = c(0.239, 0.098, 0.458, 0.591, 0.429),
#                    Ensemble = c(0.202, 0.095, 0.403, 0.51, 0.388)),
#               row.names = c(NA, -5L),
#               class = c("tbl_df", "tbl",
#                         "data.frame"))
#   )
#
#   testthat::expect_equal(
#     tab |>
#       tab_round(n = -1) |>
#       print() -> toto,
#     structure(list(prenom = c("Basile", "Paulin", "Robin", "Zoé",
#                               "Ensemble"),
#                    `2000` = c(0, 0, 0, 0, 0),
#                    `2020` = c(0, 0, 0, 0, 0),
#                    Ensemble = c(0, 0, 0, 0, 0)),
#               row.names = c(NA, -5L),
#               class = c("tbl_df",
#                         "tbl", "data.frame"))
#   )
#
# })
#
#
#
# testthat::test_that("tab_round_guarantee_100", {
#
#   library(dplyr)
#   prenoms <- readr::read_rds(system.file("data", "prenoms.rds", package = "tabloid"))
#   mixtes <- prenoms |>
#     filter(annais == "2020") |>
#     group_by(prenom, sexe) |>
#     summarise(nombre = sum(nombre)) |>
#     group_by(prenom) |>
#     mutate(n = n(),
#            min = min(nombre),
#            max = max(nombre)) |>
#     ungroup() |>
#     filter(n > 1 & min > 100 & max > 100) |>
#     select(prenom, sexe, nombre) |>
#     distinct()
#
#   testthat::expect_equal(
#     mixtes |>
#       tab_build(var_rows = prenom,
#                 var_cols = sexe,
#                 var_stat = nombre,
#                 stat = "sum") |>
#       tab_pct("col") |>
#       tab_round(n = 0,
#                 guarantee_100 = TRUE,
#                 option = "lowest_lie",
#                 verbose = TRUE) |>
#       print(),
#     structure(list(prenom = c("Alix", "Andréa", "Camille", "Charlie",
#                               "Eden", "Lou", "Louison", "Noa", "Sasha", "Ensemble"),
#                    F = c(17, 6, 18, 20, 4, 26, 4, 2, 3, 100),
#                    G = c(2, 4, 17, 13, 38, 2, 3, 14, 7, 100),
#                    Ensemble = c(11, 5, 18, 16, 18, 16, 4, 7, 5, 100
#                    )),
#               row.names = c(NA, -10L),
#               class = c("tbl_df", "tbl", "data.frame"
#               ))
#   )
#
#   testthat::expect_equal(
#     mixtes |>
#       tab_build(var_rows = prenom,
#                 var_cols = sexe,
#                 var_stat = nombre,
#                 stat = "sum") |>
#       tab_pct("col") |>
#       tab_round(n = 0,
#                 guarantee_100 = TRUE,
#                 option = "closest",
#                 verbose = TRUE) |>
#       print(),
#     structure(list(prenom = c("Alix", "Andréa", "Camille", "Charlie",
#                               "Eden", "Lou", "Louison", "Noa", "Sasha", "Ensemble"),
#                    F = c(17, 6, 18, 20, 4, 26, 4, 2, 3, 100),
#                    G = c(2, 4, 17, 13, 37, 2, 4, 14, 7, 100),
#                    Ensemble = c(11, 5, 18, 17, 18, 16, 3, 7, 5, 100
#                    )),
#               row.names = c(NA, -10L),
#               class = c("tbl_df", "tbl", "data.frame"
#               ))
#   )
#
# })
