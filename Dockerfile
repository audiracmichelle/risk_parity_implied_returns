FROM rocker/tidyverse

RUN apt-get update

# RUN apt-get purge texlive

RUN apt-get install -y texlive-full

RUN install2.r --error --deps TRUE bookdown

RUN install2.r --error --deps TRUE xts

RUN install2.r --error --deps TRUE quantmod

RUN install2.r --error --deps TRUE nloptr

RUN install2.r --error --deps TRUE mltools

RUN install2.r --error --deps TRUE tinytex

RUN install2.r --error --deps TRUE devtools
