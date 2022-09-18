used_packages <- function(){

  arquivos <- list.files(pattern = ".R$|.qmd$", full.names = TRUE, recursive = TRUE)
  codigos <- purrr::map(arquivos, readLines)

  pacotes_utilizados <- codigos |>
    unlist() |>
    stringr::str_remove_all("#.*$") |>
    stringr::str_extract_all("[:alnum:]*(?=::)") |>
    unlist() |>
    unique() |>
    sort()

  pacotes_remover <- c("", "utils", "any", "posit", "rstudio")

  pacotes_utilizados <- pacotes_utilizados[!pacotes_utilizados %in% pacotes_remover]


  description <- readLines(".github/workflows/build-blog.yaml")

  pacotes_instalados <- description |>
    paste0(collapse = " ") |>
    stringr::str_extract_all(stringr::regex("any::.*")) |>
    stringr::str_remove_all(stringr::regex("- name:.*")) |>
    stringr::str_remove_all("any::") |>
    stringr::str_split(",| ") |>
    purrr::pluck(1) |>
    stringr::str_trim() |>
    unique() |>
    sort()

  pacotes_adicionar <- pacotes_utilizados[!pacotes_utilizados %in% pacotes_instalados]

  if(length(pacotes_adicionar) > 0 ){
    usethis::ui_stop("Adicionar os seguintes pacotes no workflow: \n {paste0('any::', pacotes_adicionar, collapse = '\n ')}")
  }
}
