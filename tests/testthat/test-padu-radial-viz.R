# tests/testthat/test-padu-radial-viz.R

library(testthat)
library(sf)
library(htmltools)

make_radial_test_sf <- function() {
  poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  sf::st_sf(
    id_pu = c(101L, 204L, 312L),
    RTRW = c("Hutan Lindung", "Industri", "Pertanian"),
    RZWP3K = c("Konservasi", "Pelabuhan", "Konservasi"),
    fungsi = c("lindung", "budidaya", "campuran"),
    idx_serasi = c(0.90, 0.85, 0.25),
    idx_padu_ke = c(0.84, 0.35, 0.52),
    idx_padu_hs = c(0.78, NA_real_, 0.55),
    idx_padu_kh = c(0.91, 0.22, 0.40),
    idx_padu_ki = c(0.68, 0.58, 0.18),
    idx_padu_rtp = c(0.32, 0.82, 0.65),
    idx_padu_se = c(0.45, 0.74, 0.30),
    idx_padu_all = c(0.66, 0.54, 0.43),
    idx_padu_lindung = c(0.80, 0.38, 0.41),
    idx_padu_budidaya = c(0.48, 0.71, 0.38),
    idx_padu_final = c(0.80, 0.71, 0.43),
    idx_padan = c(0.85, 0.78, 0.34),
    geometry = rep(poly, 3)
  )
}

test_that("plot_padu_radial_widget validates inputs cleanly", {
  expect_true(exists("plot_padu_radial_widget", mode = "function"))

  df_valid <- sf::st_drop_geometry(make_radial_test_sf())

  expect_error(plot_padu_radial_widget(NULL), "data.frame|sf")
  expect_error(plot_padu_radial_widget("bad"), "data.frame|sf")
  expect_error(plot_padu_radial_widget(df_valid[0, ]), "at least one row")
  expect_error(plot_padu_radial_widget(df_valid, id_col = "missing_id"), "missing_id")
  expect_error(plot_padu_radial_widget(df_valid, alpha = 1.5), "alpha")
  expect_error(
    plot_padu_radial_widget(data.frame(id_pu = 1:2, foo = c(1, 2))),
    "idx_padu_"
  )
})

extract_radial_payload <- function(html_str) {
  m <- regmatches(html_str, regexec("data-laspur-payload='([^']*)'", html_str))[[1]]
  if (length(m) < 2) {
    m <- regmatches(html_str, regexec('data-laspur-payload="([^"]*)"', html_str))[[1]]
  }
  expect_true(length(m) >= 2)
  json_txt <- gsub("&quot;", "\"", m[2], fixed = TRUE)
  json_txt <- gsub("&#39;", "'", json_txt, fixed = TRUE)
  json_txt <- gsub("&amp;", "&", json_txt, fixed = TRUE)
  jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
}

test_that("plot_padu_radial_widget renders self-contained HTML/SVG+JS without external CDNs and preserves NA as null", {
  sf_obj <- make_radial_test_sf()
  weights <- data.frame(
    Kode = c("ke", "hs", "kh", "ki", "rtp", "se"),
    Bobot = c(0.20, 0.20, 0.20, 0.15, 0.15, 0.10)
  )

  w <- plot_padu_radial_widget(sf_obj, padu_idx_weight = weights, alpha = 0.5)
  html_str <- as.character(htmltools::renderTags(w)$html)

  # Self-contained: no external CDN script tags
  expect_false(grepl("<script[^>]+src=['\"]https?://", html_str, ignore.case = TRUE))

  # Controls, labels, gradient, id_pu center badge text, and NA hatch present
  expect_match(html_str, "<select", fixed = TRUE)
  expect_match(html_str, "PADU", fixed = TRUE)
  expect_match(html_str, "PADAN", fixed = TRUE)
  expect_match(html_str, "Lindung", fixed = TRUE)
  expect_match(html_str, "Budidaya", fixed = TRUE)
  expect_match(html_str, "id_pu ' + pu.id_pu", fixed = TRUE)
  expect_match(html_str, "#facc15", fixed = TRUE)
  expect_match(html_str, "#1d4ed8", fixed = TRUE)
  expect_match(html_str, "naStripePattern", fixed = TRUE)

  payload <- extract_radial_payload(html_str)

  expect_true(isTRUE(payload$has_padan))
  expect_equal(payload$alpha, 0.5)
  ind_keys <- vapply(payload$indicators, `[[`, character(1), "key")
  expect_equal(ind_keys, c("ke", "hs", "kh", "ki", "rtp", "se"))
  expect_false("kl" %in% ind_keys)

  expect_equal(length(payload$records), 3L)
  pu204 <- payload$records[[2]]
  expect_equal(pu204$id_pu, "204")
  expect_null(pu204$scores$hs)
  expect_equal(pu204$scores$rtp, 0.82)
})

test_that("plot_padu_radial_widget handles PADU-only data, boundary alpha (0/1), partial weights, and unique DOM IDs", {
  df_padu_only <- data.frame(
    id_pu = 1:2,
    fungsi = c("lindung", "campuran"),
    idx_padu_ke = c(0.7, 0.3),
    idx_padu_hs = c(0.6, NA_real_),
    idx_padu_final = c(0.65, 0.3)
  )

  w1 <- plot_padu_radial_widget(df_padu_only)
  w2 <- plot_padu_radial_widget(df_padu_only)
  h1 <- as.character(htmltools::renderTags(w1)$html)
  h2 <- as.character(htmltools::renderTags(w2)$html)

  id1 <- regmatches(h1, regexpr('id="laspur-radial-[^"]+"', h1))
  id2 <- regmatches(h2, regexpr('id="laspur-radial-[^"]+"', h2))
  expect_true(nzchar(id1) && nzchar(id2) && id1 != id2)

  payload <- extract_radial_payload(h1)
  expect_false(isTRUE(payload$has_padan))

  # Boundary alpha = 0 and alpha = 1 + partial weight table (only ke & hs provided when kl also present)
  sf_obj <- make_radial_test_sf()
  partial_w <- data.frame(Kode = c("ke", "hs", "kh", "ki"), Bobot = c(0.4, 0.3, 0.2, 0.1))
  w_a0 <- plot_padu_radial_widget(sf_obj, padu_idx_weight = partial_w, alpha = 0)
  w_a1 <- plot_padu_radial_widget(sf_obj, padu_idx_weight = partial_w, alpha = 1)
  p_a0 <- extract_radial_payload(as.character(htmltools::renderTags(w_a0)$html))
  p_a1 <- extract_radial_payload(as.character(htmltools::renderTags(w_a1)$html))
  expect_equal(p_a0$alpha, 0)
  expect_equal(p_a1$alpha, 1)
  expect_equal(p_a0$records[[1]]$weights$ke, 0.4)
  expect_null(p_a0$records[[1]]$weights$rtp)
})
