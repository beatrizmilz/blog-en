multilang <- function() {
  folders <- fs::dir_ls("../", type = "directory")

  buttons_info <- folders |>
    tibble::tibble() |>
    dplyr::mutate(
      folders = paste0(folders, "/index.qmd"),
      lang = dplyr::case_when(
        stringr::str_detect(folders, "/en/") ~ "English",
        stringr::str_detect(folders, "/pt/") ~ "Português",
        stringr::str_detect(folders, "/es/") ~ "Español",
      ),
      button_html = glue::glue(
        '<a href="{folders}"> <button type="button" class="btn btn-primary">{lang}</button></a>'
      )
    )


 buttons_info$button_html
}
