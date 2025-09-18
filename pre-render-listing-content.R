read_yaml_matter <- function(file) {
  # Ler o texto do arquivo
  input_lines <- readLines(file)
  # Encontrar os delimitadores
  delimiters <- grep("^---\\s*$", input_lines)
  # Extrair as linhas do yaml
  yaml_lines <- input_lines[(delimiters[1] + 1):(delimiters[2] - 1)]
  yaml_lines <- yaml_lines[yaml_lines != ""]
  yaml_lines <- paste0(yaml_lines, collapse = "\n")

  # Convertendo o yaml para lista
  yaml_list <- yaml::yaml.load(yaml_lines)

  # Convertendo a data
  yaml_list$data <- as.Date(yaml_list$date)

  # Verificando se a data é futura
  yaml_list$data_futura <- yaml_list$data > Sys.Date()

  # Adicionando o caminho do arquivo na lista
  yaml_list$path <- file

  # Retornando o yaml
  yaml_list
}


preparar_yaml_para_salvar <- function(elemento_lista_yaml) {
  # Selecionando apenas os elementos necessários
  yaml_selecionado <- list(
    title = elemento_lista_yaml$title,
    author = elemento_lista_yaml$author,
    date = elemento_lista_yaml$date,
    path = elemento_lista_yaml$path #,
    #categories = elemento_lista_yaml$categories
  )
  # Retornando o yaml
  yaml_selecionado
}


criar_arquivo_yaml_data <- function(lista_yaml_data, tipo = "futura") {
  # Criando o arquivo
  arquivo <- fs::path(paste0("content_data_", tipo, ".yaml"))

  lista_preparada <- lista_yaml_data |>
    purrr::map(preparar_yaml_para_salvar) |>
    unname()

  # Convertendo a lista para yaml
  yaml::write_yaml(lista_preparada, arquivo)
}


prepare_file_listing_content <- function() {
  # listando arquivos
  qmd_files_talks <- fs::dir_ls(path = "talks", glob = "*.qmd", recurse = TRUE)
  qmd_files_teaching <- fs::dir_ls(
    path = "teaching",
    glob = "*.qmd",
    recurse = TRUE
  )
  qmd_files <- c(qmd_files_talks, qmd_files_teaching)

  # lendo yaml dos arquivos e buscando a data
  lista_yaml_qmd <- qmd_files |>
    purrr::map(read_yaml_matter)

  # removendo itens sem data
  lista_yaml_qmd_com_data <- lista_yaml_qmd |>
    purrr::discard(~ length(.x$data_futura) == 0)

  # identificando o conteúdo futuro
  lista_yaml_data_futura <- lista_yaml_qmd_com_data |>
    purrr::keep(~ .x$data_futura)

  # identificando o conteúdo do passado
  lista_yaml_data_passado <- lista_yaml_qmd_com_data |>
    purrr::discard(~ .x$data_futura)

  # Criando os arquivos auxiliares
  criar_arquivo_yaml_data(lista_yaml_data_futura, tipo = "futura")
  criar_arquivo_yaml_data(lista_yaml_data_passado, tipo = "passado")
}

# run functions
prepare_file_listing_content()
