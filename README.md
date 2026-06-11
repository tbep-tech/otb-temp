# otb-temp

Evaluation of the [TBEP Optical Model](https://drive.google.com/file/d/1qaM852mkVkRyjwU8yZjlmZqMy1ROpcZi/view) indicated that high water temperatures may be leading to unfavorable seagrass conditions and highlighted the need for continuous summertime temperature data on shallow seagrass beds in Old Tampa Bay. These are the materials for identifying locations for placing temperature loggers in Old Tampa Bay and initial data analyses.

View the logger web page: [link](https://tbep-tech.github.io/otb-temp/tempeval)

## Updating the logger web page

### One time prep 

1. Make sure you have git installed, see instructions [here](https://git-scm.com/install/).

1. Clone the repository to your computer. Use your IDE's tools (e.g., within RStudio) or do it from the terminal (as below). If using the terminal, navigate to a folder where the repository will be cloned using `cd` and use `git clone`.

   ```cmd
   cd path/to/your/directory
   git clone https://github.com/tbep-tech/otb-temp.git
   ```

1. Open the repository in your IDE. In R, make sure you have the required packages installed:

   ```r
   install.packages(c("tidyverse", "googlesheets4", "googledrive", "janitor", "lubridate", "sf", "here", "knitr", "leaflet", "highcharter", "viridis"))
   ```

1. Make sure you have Quarto installed, see instructions [here](https://quarto.org/docs/get-started/).

### Update every time

1. Pull the latest version of the repository to your computer.  Use your IDE's tools (e.g., within RStudio) or use the terminal:

   ```cmd
   cd path/to/your/directory/otb-temp
   git pull origin main
   ```

1. In your IDE, source the file `R/dat_proc.R` or run it line by line.

1. Add the markdown text and R code chunk to the bottom of `docs/tempeval.qmd` for the appropriate year and deployment. Number the header for the deployments sequentially for each year and increase the counter for the `ddin` by 1.  For example, the first 2026 deployment content will look like this, starting with "1" for the first deployment and increasing the counter for `ddin` regardless of year:

   ````
   ### Deploy 1

   ```{{r}}
   ddin <- orgs[[28]]
   tsplo_dd(tempdat, ddin)
   mpplo_dd(metadat, ddin)
   ```   
   ````

1. Render the quarto file `docs/tempeval.qmd` to create the HTML web page.  Do this in your IDE or from the terminal: 

   ```cmd
   quarto render docs/tempeval.qmd --to html
   ```

1. Stage the changes and add a commit message.  Use your IDE's tools (e.g., within RStudio) or from the terminal:

   ```cmd
   git add .
   git commit -m "update with first 2026 deployment"
   ```

1. Push the changes to GitHub.  Use your IDE's tools (e.g., within RStudio) or from the terminal:

   ```cmd
   git push origin main
   ```

1. The updated Quarto file should be online in a minute or two.  Make sure to view the data for accuracy and completeness.

## EDA links

-   [2023](https://tbep-tech.github.io/otb-temp/eda2023)

-   [2024](https://tbep-tech.github.io/otb-temp/eda2024)

-   [2025](https://tbep-tech.github.io/otb-temp/eda2025)