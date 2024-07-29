library(plumber)
r <- plumb(file='APIDocker/myAPI.R')
r$run(port = 8000)
