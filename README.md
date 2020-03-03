# Risk Parity Implied Returns

This repository's output is a document in both pdf and html format:

* The pdf output is found in ...
* The html output is accessible in GitHub pages at ...

The document's goal is to provide didactic material on financial markets data manipulation and portfolio optimization. The document is written in Spanish.

## How to run

To run bookdown this repo's Dockerfile uses [rocker/tidyverse](https://hub.docker.com/r/rocker/) and installs all texlive-full. Another bookdown docker creation is found in https://hub.docker.com/r/0xcaff/bookdown/. But not tested with the content in [./book](./book).

To build a risk-parity-implied-returns image run

```
cd $(pwd)
docker build -t risk-parity-implied-returns .
```

To run the image use

```
docker run --rm -e PASSWORD=book -p 8787:8787 -v $(pwd)/book:/home/rstudio/book risk-parity-implied-returns
```

Then go to `localhost:8787` in the browser. 

Within Rstudio use rstudio/book when asked for user/password and open /home/rstudio/book/book.Rproj 

The output will be found in book/_book/book.pdf after building the book. The book can be built by clicking the `build book` button in RStudio's `build` tab.
