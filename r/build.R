library(quarto)

original_wd <- getwd()

for(yr in 2023:2025){
  
  setwd(here::here('docs'))
  
  outputfl <- paste0('eda', yr, '.html')
  
  quarto_render(
    input = 'edatemplate.qmd',
    execute_params = list(yr = yr),
    output_file = outputfl
  )
  
  setwd(original_wd)
  
}