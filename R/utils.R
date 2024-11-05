rename_md_to_qmd <- function(folder = "talks/pt"){
  # renomeando .md para .qmd
  folders <- list.dirs(folder, full.names = TRUE)

  md_files <- folders |>
    list.files(pattern = "\\.md", full.names = TRUE)

  qmd_files <- stringr::str_replace(md_files, "\\.md",".qmd")

  fs::file_move(md_files, qmd_files)

  qmd_files
}


alterar_yaml <- function(input_file) {
  # o que tem apos o yaml??
  input_lines <- readLines(input_file)
  delimiters <- grep("^---\\s*$", input_lines)
  apos_yaml <- input_lines[delimiters[2]:length(input_lines)]

  # trabalhando com o yaml atual

  yaml_atual <-  input_file |> read_yaml_matter()

  yaml_atual$show_post_time <- NULL
  yaml_atual$layout <- NULL
  yaml_atual$featured <- NULL
  yaml_atual$date_end <- NULL
  yaml_atual$excerpt <- NULL
  yaml_atual$draft <- NULL
  yaml_atual$publishDate <- NULL

  imagem_post <- list.files(fs::path_dir(input_file), pattern =  "*.png|*featured*" )[1]

  if (!is.null(imagem_post)) {
    yaml_atual$image <- imagem_post
  }

  novo_yaml <- yaml::as.yaml(yaml_atual)

  codigo_adicionar_fun_read_yaml_talks_pt <- '
```{r}
#| echo: false
#| results: asis
source(here::here("R/utils.R"))
read_yaml_talks_pt()
```

'

  output_lines <- c(
    "---
# THIS FILE WAS CONVERTED FROM BLOGDOWN!
toc: true",
    novo_yaml,
    "---",
    codigo_adicionar_fun_read_yaml_talks_pt,
    apos_yaml
  )

  output_lines |> writeLines(input_file)

  print(input_file)
}

read_yaml_matter <- function(input_file) {
  # https://stackoverflow.com/questions/62095329/how-to-edit-an-r-markdown-yaml-header-programmatically/62096216#62096216
  input_lines <- readLines(input_file)
  delimiters <- grep("^---\\s*$", input_lines)
  if (!length(delimiters)) {
    stop("unable to find yaml delimiters")
  } else if (length(delimiters) == 1L) {
    if (delimiters[1] == 1L) {
      stop("cannot find second delimiter, first is on line 1")
    } else {
      # found just one set, assume it is *closing* the yaml matter;
      # fake a preceding line of delimiter
      delimiters <- c(0L, delimiters[1])
    }
  }
  delimiters <- delimiters[1:2]
  yaml_list <- yaml::yaml.load(input_lines[ (delimiters[1]+1):(delimiters[2]-1) ])
}


read_yaml_talks_pt <- function(){
  cat('<script src="https://kit.fontawesome.com/1bb720cc6f.js" crossorigin="anonymous"></script>')

  yaml_atual <- fs::dir_ls(glob = "*.qmd") |>
    read_yaml_matter()

  if(!is.null(yaml_atual$event)){
    cat(paste0("<b>Evento: </b>", yaml_atual$event, "<br>"))
  }

  if(!is.null(yaml_atual$event_url)){
    cat(paste0("<b>Link:</b> <a href='", yaml_atual$event_url, "'>", yaml_atual$event_url, "</a><br>"))
  }

  if(!is.null(yaml_atual$location)){
    cat(paste0("<b>Lugar: </b>", yaml_atual$location, "<br>"))
  }



  if(!is.null(yaml_atual$links)){
    cat(paste0("<h2> Materiais</h2> <br><center>"))


    for (position in 1:length(yaml_atual$links)) {
      item <- yaml_atual$links[[position]]

      if(item$icon_pack %in% c("fas", "far", "fa")){
        item$icon_pack <- "solid"
      } else if(item$icon_pack == "fab"){
        item$icon_pack <- "brands"
      }

      cat(paste0("<a href='", item$url, "' target='_blank' rel='noopener'><button class='btn btn-primary' style='margin: 10px;'><i class='", "fa-", item$icon_pack , " fa-", item$icon, "'></i> ", item$name, "</a></button>  "))
    }

    if(!is.null(yaml_atual$image)){
      cat(paste0("<br><br><img src='", yaml_atual$image, "' class='img-fluid quarto-figure-center' style='max-width:80%'><br>"))
    }


    cat(paste0("</center>"))
  }
}


convert_apero_talks_to_quarto <- function(folder = "talks/pt") {
  arquivos <- rename_md_to_qmd()

  arquivos |>
    purrr::walk(alterar_yaml)

}

