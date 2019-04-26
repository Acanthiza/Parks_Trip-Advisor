

  library("bookdown")
  library("tidyverse")
  library("fs")
  
  render_book("rmd")
  
  outDir <- "S:/DEHTemp/Parks_Trip-Advisor/"
  
  folders <- dir_info(recursive = TRUE) %>%
    dplyr::filter(grepl("_book|out",path)
                  , type == "directory"
                  ) %>%
    dplyr::mutate(toDir = paste0(outDir,path)) %>%
    dplyr::pull(toDir) %>%
    dir_create()
  
  files <- dir_info(recursive = TRUE) %>%
    dplyr::filter(grepl("_book|out",path)
                  , type != "directory"
                  ) %>%
    dplyr::mutate(toDir = paste0(outDir,path))
  
  walk2(files$path
        , files$toDir
        , file_copy
        , overwrite = TRUE
        )
  
  file_delete("rPackages.bib")