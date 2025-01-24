# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
library(ggplot2)

compare_functions <- function(functions, min=100, max=1e5, step=1000){
    # generate data
    data <- list()
    data_sizes <- seq(min, max, step)
    for(n in data_sizes){
        data[[n]] <- rnorm(n)
    }

    # compute run times
    compute_time_data <- data.frame(matrix(ncol = 3, nrow = 0))
    for(n in data_sizes){
        for(i in seq_along(functions)){
            compute_time_data <- rbind(
                compute_time_data,
                list(n,compute_run_time(functions[[names(functions)[i]]], data[[n]]), names(functions)[i])
                )
        }
    }
    colnames(compute_time_data) <- c("size","time","func")
    return(compute_time_data)
}

compute_run_time <- function(func, data){
    start <- Sys.time()
    func(data)
    end <- Sys.time()
    return(end-start)
}

plot <- function(compute_times){
    ggplot(compute_times, aes(x=size, y=time, color=func)) +
        geom_line() +
        theme_classic()
}
