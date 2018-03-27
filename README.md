[![CircleCI](https://circleci.com/gh/primitybio/cellengine-r-toolkit.svg?style=svg&circle-token=8d7119878c06e63cb77f1743afc0782db13d7ce1)](https://circleci.com/gh/primitybio/cellengine-r-toolkit)

 
CellEngine R API Toolkit
-----

Installing:
```R
library("devtools")
install_github("primitybio/cellengine-r-toolkit")
```

Quick start:

```R
library("cellengine")
setServer("https://cellengine.com")
authenticate("username", Sys.getenv("CELLENGINE_PASSWORD"))

experiments = getExperiments()
```
