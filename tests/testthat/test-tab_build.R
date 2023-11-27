# testthat::test_that("tab_build_sum", {
#
#   library(dplyr)
#   prenoms <- readr::read_rds(system.file("data", "prenoms.rds", package = "tabloid"))
#
#   testthat::expect_equal(
#     prenoms |>
#       filter(annais == 2020 &
#                prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
#                lib_reg == "Occitanie") |>
#       tab_build(var_rows = prenom,
#                 var_cols = sexe,
#                 var_stat = nombre,
#                 stat = "sum") |>
#       print(),
#     structure(list(prenom = c("Camille", "Charlie", "Eden", "Sasha",
#                               "Alix", "Ensemble"),
#                    G = c(65, 49, 150, 38, 0, 302),
#                    F = c(113, 139, 22, 12, 107, 393),
#                    Ensemble = c(178, 188, 172, 50, 107, 695)),
#               row.names = c(NA, -6L),
#               class = c("tbl_df", "tbl", "data.frame")))
#
#   testthat::expect_equal(
#     prenoms |>
#       filter(annais == "2020",
#              prenom %in% c("Charlie", "Alix", "Eden", "Sasha", "Camille") &
#                lib_reg %in% c("Occitanie", "Pays de la Loire")) |>
#       tab_build(var_rows = c(lib_reg, lib_dpt),
#                 var_cols = c(prenom, sexe),
#                 var_stat = nombre,
#                 lab_total = "Total",
#                 stat = "sum") |>
#       print(),
#     structure(list(lib_reg = c("Pays de la Loire", "Pays de la Loire",
#                                "Pays de la Loire", "Pays de la Loire", "Pays de la Loire", "Pays de la Loire",
#                                "Occitanie", "Occitanie", "Occitanie", "Occitanie", "Occitanie",
#                                "Occitanie", "Occitanie", "Occitanie", "Occitanie", "Occitanie",
#                                "Occitanie", "Occitanie", "Total"),
#                    lib_dpt = c("Loire-Atlantique",
#                                "Maine-et-Loire", "Mayenne", "Sarthe", "Vendée", "Total", "Aude",
#                                "Aveyron", "Gard", "Haute-Garonne", "Hérault", "Hautes-Pyrénées",
#                                "Pyrénées-Orientales", "Tarn", "Tarn-et-Garonne", "Lozère", "Gers",
#                                "Total", "Total"),
#                    Alix_G = c(5, 7, 0, 0, 0, 12, 0, 0, 0, 0,
#                               0, 0, 0, 0, 0, 0, 0, 0, 12),
#                    Alix_F = c(49, 25, 11, 7, 13, 105,
#                               3, 10, 5, 48, 17, 3, 6, 11, 0, 0, 4, 107, 212),
#                    Alix_Total = c(54,
#                                   32, 11, 7, 13, 117, 3, 10, 5, 48, 17, 3, 6, 11, 0, 0, 4, 107,
#                                   224),
#                    Camille_G = c(32, 25, 3, 4, 10, 74, 4, 5, 10, 21, 12, 3,
#                                  3, 4, 3, 0, 0, 65, 139),
#                    Camille_F = c(46, 42, 10, 15, 14, 127,
#                                  9, 6, 11, 31, 27, 4, 7, 7, 3, 3, 5, 113, 240),
#                    Camille_Total = c(78,
#                                      67, 13, 19, 24, 201, 13, 11, 21, 52, 39, 7, 10, 11, 6, 3, 5,
#                                      178, 379),
#                    Charlie_G = c(30, 18, 3, 3, 8, 62, 0, 0, 4, 27, 14,
#                                  0, 4, 0, 0, 0, 0, 49, 111),
#                    Charlie_F = c(51, 27, 8, 7, 13, 106,
#                                  9, 14, 14, 46, 30, 3, 5, 15, 0, 0, 3, 139, 245),
#                    Charlie_Total = c(81, 45, 11, 10, 21, 168, 9, 14, 18, 73, 44, 3, 9, 15, 0, 0, 3, 188, 356),
#                    Eden_G = c(47, 27, 12, 18, 22, 126, 10, 3, 18, 41, 41,
#                               3, 13, 9, 9, 3, 0, 150, 276),
#                    Eden_F = c(13, 0, 0, 0, 4, 17,
#                               0, 0, 3, 7, 6, 0, 3, 3, 0, 0, 0, 22, 39),
#                    Eden_Total = c(60, 27, 12, 18, 26, 143, 10, 3, 21, 48, 47, 3, 16, 12, 9, 3, 0, 172, 315),
#                    Sasha_G = c(14, 8, 4, 0, 0, 26, 3, 0, 5, 14, 12, 0, 4,
#                                0, 0, 0, 0, 38, 64),
#                    Sasha_F = c(9, 0, 0, 4, 0, 13, 0, 0, 3,
#                                4, 5, 0, 0, 0, 0, 0, 0, 12, 25),
#                    Sasha_Total = c(23, 8, 4, 4,
#                                    0, 39, 3, 0, 8, 18, 17, 0, 4, 0, 0, 0, 0, 50, 89),
#                    Total_Total = c(296,
#                                    179, 51, 58, 84, 668, 38, 38, 73, 239, 164, 16, 45, 49, 15, 6,
#                                    12, 695, 1363)),
#               row.names = c(NA, -19L),
#               class = c("tbl_df", "tbl", "data.frame"))
#   )
#
# })
#
# testthat::test_that("tab_build_mean", {
#
#   library(dplyr)
#   prenoms <- readr::read_rds(system.file("data", "prenoms.rds", package = "tabloid"))
#   parts <- prenoms |>
#     filter(annais == "2020") |>
#     group_by(sexe, lib_reg) |>
#     mutate(part = 1000 * nombre / sum(nombre)) |>
#     filter(prenom %in% c("Zoé", "Robin", "Basile", "Paulin")) |>
#     ungroup()
#
#   testthat::expect_equal(
#
#     parts |>
#       tab_build(var_rows = lib_reg,
#                 var_cols = prenom,
#                 var_stat = part,
#                 stat = "mean") |>
#       print(),
#     structure(list(lib_reg = c("Hauts-de-France", "Auvergne-Rhône-Alpes",
#                                "Provence-Alpes-Côte d'Azur", "Grand Est", "Normandie", "Nouvelle-Aquitaine",
#                                "Bourgogne-Franche-Comté", "Centre-Val de Loire", "Bretagne",
#                                "Occitanie", "Pays de la Loire", "Ile-de-France", "Corse", "La Réunion",
#                                "Guadeloupe", "Martinique", "Guyane", "Ensemble"),
#                    Basile = c(0.770743216673221,
#                               0.445165188082292, 0.423250564334086, 0.422016825326544, 0.686341798215511,
#                               0.353107344632768, 0.709759188846641, 0.743846361915066, 0.951968844655993,
#                               0.696887801064029, 0.955877259818522, 0.372470586110685, 0, 0,
#                               0, 0, 0, 0.576747377012785),
#                    Paulin = c(0.117970900511207, 0.111291297020573,
#                               0.141083521444695, 0.166039406685853, 0, 0.244458930899609, 0,
#                               0.4057343792264, 0, 0, 0.438687631317679, 0.0553062385437077,
#                               0, 0, 0, 0, 0, 0.236051854710748),
#                    Robin = c(1.89539913488006,
#                              0.699545295557887, 0.940556809631302, 1.00238604777015, 1.88743994509266,
#                              0.684485006518905, 1.39416983523447, 1.13605626183392, 1.73085244482908,
#                              0.698587527408087, 1.64854194084644, 0.357515327728968, 3.70828182941904,
#                              3.62756952841596, 0, 0, 0, 1.10901406813913),
#                    `Zoé` = c(1.90259085528911,
#                              0.685757299328277, 1.25473864061082, 0.959202740579259, 1.41185571994971,
#                              0.884145707212549, 1.48649341329643, 1.66562565063502, 2.1764648341974,
#                              0.871870397643594, 1.912164788185, 0.635585866999861, 0, 7.51670378619154,
#                              16.4609053497942, 11.8483412322275, 8.04597701149425, 1.60212232863375
#                    ),
#                    Ensemble = c(1.4351023084202, 0.591054430544819, 0.825416027812915,
#                                 0.769714319024414, 1.37441753700528, 0.649011724081281, 1.23483029281406,
#                                 1.1447582011231, 1.68047051364308, 0.773706202335169, 1.32772126878905,
#                                 0.426508645507714, 3.70828182941904, 5.57213665730375, 16.4609053497942,
#                                 11.8483412322275, 8.04597701149425, 1.0980701390486)),
#               row.names = c(NA,
#                             -18L),
#               class = c("tbl_df", "tbl", "data.frame"))
#   )
#
# })
#
