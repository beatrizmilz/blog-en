qmd_files_talks <- fs::dir_ls(path = "talks",
                              glob = "*.qmd",
                              recurse = TRUE)
qmd_files_teaching <- fs::dir_ls(path = "teaching",
                                 glob = "*.qmd",
                                 recurse = TRUE)
qmd_files <- c(qmd_files_talks, qmd_files_teaching)
# lendo yaml dos arquivos e buscando a data
lista_yaml_qmd <- qmd_files |>
  purrr::map(read_yaml_matter)


library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(rlang)

# Transformar a lista em tibble
tabela_talks <- lista_yaml_qmd |>
  imap_dfr( ~ {
    path <- .x$path %||% .y  # garante que o path venha do .x, senão usa o nome da lista
    idioma <- stringr::str_extract(path, "/(pt|en|es)/") |>
      stringr::str_replace_all("/", "")  # remove as barras

    tibble(
      title = .x$title %||% NA_character_,
      subtitle = .x$subtitle %||% NA_character_,
      categories = paste(.x$categories, collapse = "; "),
      event = .x$event %||% NA_character_,
      path = path,
      idioma = idioma,
      id = .y
    )
  }) |>
  tibble::rowid_to_column()



tabela_talks |>
  dplyr::filter(idioma == "pt", stringr::str_starts(id, "teaching")) |> View()


tabela_talks |>
  dplyr::filter(idioma == "pt", stringr::str_starts(id, "teaching")) |>
  tidyr::separate_longer_delim(categories, "; ") |>
  dplyr::count(categories, sort = TRUE)

