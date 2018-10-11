# Risk Parity Implied Returns

To run bookdown: this repo's Dockerfile uses [rocker/tidyverse](https://hub.docker.com/r/rocker/) and installs all texlive-full.

Another bookdown docker creation is found in https://hub.docker.com/r/0xcaff/bookdown/. But not tested with the content in [./book](./book).

To create a risk-parity-implied-returns image run

```
docker build -t risk-parity-implied-returns .
docker run --rm -e PASSWORD=book -p 8787:8787 -v $(pwd)/book:/home/rstudio/book risk-parity-implied-returns
```

Then go to `localhost:8787` in the browser. 

Within rstudio use rstudio/book when asked for user/password and open /home/rstudio/book/book.Rproj 

The output will be found in book/_book/book.pdf

 




