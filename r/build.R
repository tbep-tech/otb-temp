library(quarto)

for(yr in 2023:2025){

  outputfl <- paste0('eda', yr, '.html')

  # render document
  quarto_render(
    input = here::here('docs', 'edatemplate.qmd'),
    execute_params = list(yr = yr),
    output_file = outputfl
  )

  file.rename(
    from = outputfl, 
    to = here::here('docs', outputfl)
  )

}