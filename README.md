# Repository `mirt-parameters`

Paper "A generalized definition of multidimensional item response
theory parameters" (working title)


# Main output

The main file is
[`output/paper_draft/paper_M2PLM_parameters.Rmd`][paper],
which renders the article main text output.

[paper]: output/paper_draft/paper_M2PLM_parameters.Rmd

The output of this file is the document that will eventually
be submitted to a journal,
along with the necessary additional files (i.e., cover letter).

# Rendering the output

The easiest way to render the project output is by sourcing the script
[src/Build_project.R](src/Build_project.R).
Prior to this step, some pre-requisites must be met
(which imply manual installation of software components).

**NOTE:** When opening the output document,
Word will prompt to "Update the fields".
This is [set by design in the R package `officer`][update_prompt],
used to render the document, for security reasons.
It is enough to click on "Yes" in the pop-up window.

[update_prompt]: https://ardata-fr.github.io/officeverse/faq.html#update-fields

## Manual installation of software components:

- Install [R version 4.2.1][R]:
  In Windows, using the [binary installer][inst] is recommended.

[R]: https://cran.rstudio.com/bin/windows/base/old/4.2.1/
[inst]: (https://cran.rstudio.com/bin/windows/base/old/4.2.1/R-4.2.1-win.exe)

- [Rstudio Desktop][RS]: Although not strictly necessary, it is recommended
  to install the Rstudio IDE; for strict reproducibility, use build
  [2022.07.1+554 for Windows 10/11][RSv].
  However, if Rstudio is not installed,
  then [Pandoc][P] will need to be downloaded and installed
  (Use [v2.18 for Windows][Pv] for strict reproducibility).

[RS]: https://www.rstudio.com/products/rstudio/download/#download

[RSv]: https://download1.rstudio.org/desktop/windows/RStudio-2022.07.1-554.exe

[P]: https://pandoc.org/installing.html

[Pv]: https://github.com/jgm/pandoc/releases/download/2.18/pandoc-2.18-windows-x86_64.msi

- [Git client][G]: Install the Git client in order to be able to clone locally
  the project repository.
  On Windows, use the [64-bit Windows installer][GW].

[G]: https://git-scm.com/download

[GW]: https://github.com/git-for-windows/git/releases/download/v2.37.1.windows.1/Git-2.37.1-64-bit.exe

- [Node.js][N]: It is necessary for rendering the equations in the table headers
  with the [`flextable` package][FT]. When installing Node.js,
  one of the Windows of the installer will prompt the user to
  "Automatically install the necessary tools";
  tick this checkbox to ensure all the necessary tools are installed.
  For strict reproducibility, install the
  [64-bit Windows intaller, v16.16.0][NW].

[FT]: https://cran.r-project.org/package=flextable

[NW]: https://nodejs.org/dist/v16.16.0/node-v16.16.0-x64.msi

## Installing the project locally

This project is hosted as a GitHub repository.
It can be cloned as a local Git repository following [this instructions][CR]
(steps 2 through 7).
Note that this will create a local copy of the GitHub repository as an
Rstudio project in the folder specified.
The URL that must be entered into the `Repository URL` text box is:

```
https://github.com/DaniMori/mirt-parameters.git
```

[CR]: https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2

After cloning the repository,
the Rstudio project will open automatically in the Rstudio IDE.
If it doesn't, or you want to return later to the project in Rstudio,
you can do so by double clicking on the file `mapping-initiatives.Rproj`
that has been created in the project folder when cloning the repository.

**NOTE:** It is common practice to avoid using and versioning `.Rprofile` files.
Hoever, this project uses [package `renv`][renv]
to create a reproducible environment,
which needs the `.Rprofile` file that lives in the root directory of the
project. **Please DO NOT delete or edit this file**; it will install and
activate the `renv` package and make it ready for restoring the environment.

[renv]: https://cran.r-project.org/package=renv

## Building the project

In order to build the project, simply source the script
[`src/Build_project.R`](src/Build_project.R) with the following command,
which will render the project output.

```r
source("src/Build_project.R", encoding = 'UTF-8')
```

The output file generated will be in the following path:

`output/paper_draft/paper_M2PLM_parameters.docx`

**NOTE:** Rendering the project may take a while, especially the first time.
This is due to the `renv` package restoring the environment, which implies
downloading and installing several R packages. Please, be patient!
