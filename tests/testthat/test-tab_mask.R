# testthat::test_that("tab_mask_cells", {
#
#   core <- structure(list(prenom = c("Alix", "Camille", "Charlie", "Eden", "Sasha", "Ensemble"),
#                          G = c(12, 139, 111, 276, 64, 602),
#                          F = c(212, 240, 245, 39, 25, 761),
#                          Ensemble = c(224, 379, 356, 315, 89, 1363)),
#                     row.names = c(NA, -6L),
#                     class = c("tbl_df",
#                               "tbl", "data.frame"))
#
#   tab <- structure(list(core_init = core,
#                         core = core,
#                         core_last = core,
#                         count = structure(list(prenom = c("Alix", "Camille", "Charlie", "Eden", "Sasha", "Ensemble"),
#                                                G = c(2L, 14L, 9L, 15L, 8L, 48L),
#                                                F = c(14L, 16L, 14L, 7L, 5L, 56L),
#                                                Ensemble = c(16L, 30L, 23L, 22L, 13L, 104L)),
#                                           row.names = c(NA, -6L),
#                                           class = c("tbl_df",
#                                                     "tbl", "data.frame")),
#                         state = "build",
#                         param = list(var_cols = "sexe",
#                                      var_rows = "prenom",
#                                      modalites = list(prenom = c("Alix", "Camille", "Charlie", "Eden", "Sasha", "Ensemble"),
#                                                       sexe = c("G", "F", "Ensemble")),
#                                      stat = "sum",
#                                      var_stat = "nombre",
#                                      pct = "-",
#                                      var_w = "NULL",
#                                      lab_total = "Ensemble",
#                                      probs = NULL)),
#                    class = "tab")
#
#   testthat::expect_equal(
#     tab |>
#       tab_mask(threshold_count = 5) |>
#       print(),
#     structure(list(prenom = c("Alix", "Camille", "Charlie", "Eden",
#                               "Sasha", "Ensemble"),
#                    G = c(NA, 139, 111, 276, 64, 602),
#                    F = c(212, 240, 245, 39, 25, 761),
#                    Ensemble = c(224, 379, 356, 315, 89,
#                                 1363)),
#               row.names = c(NA, -6L),
#               class = c("tbl_df", "tbl", "data.frame"))
#   )
#
#   testthat::expect_equal(
#     tab |>
#       tab_mask(threshold_sum = 50) |>
#       print(),
#     structure(list(prenom = c("Alix", "Camille", "Charlie", "Eden",
#                               "Sasha", "Ensemble"),
#                    G = c(NA, 139, 111, 276, 64, 602),
#                    F = c(212, 240, 245, NA, NA, 761),
#                    Ensemble = c(224, 379, 356, 315, 89,
#                                 1363)),
#               row.names = c(NA, -6L),
#               class = c("tbl_df", "tbl", "data.frame"))
#   )
# })
#
# testthat::test_that("tab_mask_cols", {
#
#   top3 <- structure(list(annais = c("1900", "1900", "1900", "1900", "1900", "1900",
#                                     "2020", "2020", "2020", "2020", "2020", "2020"),
#                          sexe = c("F", "G", "F", "G", "F", "G", "G", "G", "G", "F", "F", "F"),
#                          prenom = c("Marie", "Jean", "Jeanne", "Louis", "Marguerite", "Pierre",
#                                     "Léo", "Gabriel", "Raphaël", "Jade", "Louise", "Emma"),
#                          nombre = c(48713, 14097, 13981, 9052, 8058, 7454, 4494, 4413, 3975, 3816, 3804, 3483),
#                          n = c(223390, 167115, 223390, 167115, 223390, 167115, 259962, 259962, 259962, 230768, 230768, 230768),
#                          part = c(21.806258113613, 8.43550848218293, 6.25856126057568, 5.41662926727104, 3.60714445588433,
#                                   4.46040151991144, 1.72871419669029, 1.69755579661643, 1.52906963325409,
#                                   1.65360881924704, 1.64840879151355, 1.50930804964293),
#                          rang = c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)),
#                     class = c("tbl_df", "tbl", "data.frame"),
#                     row.names = c(NA, -12L))
#
#   testthat::expect_equal(
#     top3 |>
#       mutate(rang = as.character(rang)) |>
#       tab_build(var_rows = rang,
#                 var_cols = c(annais, sexe),
#                 var_stat = part,
#                 stat = "sum") |>
#       tab_mask(mask_total_cols = TRUE,
#                verbose = FALSE) |>
#       print(),
#     structure(list(rang = c("1", "2", "3", "Ensemble"),
#                    `1900_F` = c(21.806258113613, 6.25856126057568, 3.60714445588433, 31.671963830073),
#                    `1900_G` = c(8.43550848218293, 5.41662926727104, 4.46040151991144, 18.3125392693654),
#                    `2020_F` = c(1.65360881924704, 1.64840879151355, 1.50930804964293, 4.81132566040352),
#                    `2020_G` = c(1.72871419669029, 1.69755579661643, 1.52906963325409, 4.95533962656081)),
#               row.names = c(NA, -4L),
#               class = c("tbl_df", "tbl", "data.frame"))
#   )
# })
#
