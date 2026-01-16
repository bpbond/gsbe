# nn_topology.R

# Curiosity: how does performance and time change with NN topology?

library(neuralnet)
library(dplyr)

# This is the dataset used by the 'neuralnet' code chunk in 2-gsbe.qmd
x <- readRDS("nn_topology/x.RDS")
if(file.exists("nn_topology/results.RDS")) results <- readRDS("nn_topology/results.RDS")

K_FOLD <- 3

# Utility function: very simple R2 computation
r2 <- function(preds, obs) {
    stopifnot(length(preds) == length(obs))
    rss <- sum((preds - obs) ^ 2)
    tss <- sum((obs - mean(obs)) ^ 2)
    return(1 - rss / tss)
}
fit_and_test_nn <- function(x_train, x_val, hidden) {
    # Algorithm is not guaranteed to converge; if this happens, returns NAs
    out <- tibble(training_r2 = NA_real_, validation_r2 = NA_real_, fit_time = NA_real_)

    try({
        tm <- system.time({
            m_nn <- neuralnet(sqrt_Rs_annual ~ ., data = x_train,
                              hidden = hidden, rep = 2, threshold = 0.4)
        })
        out$fit_time <- tm["elapsed"]

        pred_train <- predict(m_nn, newdata = x_train)
        pred_val <- predict(m_nn, newdata = x_val)
        out$training_r2 <- r2(pred_train, pull(x_train[1]))
        out$validation_r2 <- r2(pred_val, pull(x_val[1]))
    })
    return(out)
}

# Do a k-fold cross-validation
# x = data; f = function to call; k = number of folds; ... = params for model
do_k_fold <- function(x, f, k = K_FOLD, quiet = TRUE, ...) {
    if(!all(complete.cases(x))) {
        stop("x should not have any NAs at this stage!")
    }

    groups <- sample.int(n = k, size = nrow(x), replace = TRUE)
    out <- list()
    for(i in seq_len(k)) {
        x_val <- x[groups == i,]
        x_train <- x[groups != i,]
        if(!quiet) {
            message("\tk-fold ", i, ":")
            message("\t\tTraining is ", nrow(x_train), " x ", ncol(x_train))
            message("\t\tValidation is ", nrow(x_val), " x ", ncol(x_val))
        }

        out[[i]] <- f(x_train = x_train, x_val = x_val, ...)
    }
    out <- bind_rows(out)
    success <- sum(!is.na(out$fit_time)) / nrow(out)
    out %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        mutate(success = success)
} # do_k_fold


expand.grid(layer1 = seq(2, 20, by = 3),
            layer2 = seq(0, 10, by = 2),
            layer3 = seq(0, 10, by = 2)) %>%
    as_tibble() %>%
    # can't have a zero layer 2 with anything in layer 3
    filter(layer2 > 0 | (layer2 == 0 & layer3 == 0))->
    trials

if(!exists("results")) results <- list()
for(i in seq_len(nrow(trials))) {
    i_str <- as.character(i)
    message(i, " / ", nrow(trials))
    if(!is.null(results[[i_str]])) next()

    hidden <- c(trials$layer1[i], trials$layer2[i], trials$layer3[i])
    hidden <- hidden[hidden > 0] # get rid of any trailing zeroes
    message("\tHidden nodes = {", paste(hidden, collapse = ", "), "}")
    results[[i_str]] <- bind_cols(trials[i,],
                         do_k_fold(x, fit_and_test_nn, hidden = hidden))
    print(results[[i_str]])

}

resultdf <- bind_rows(results)
ggplot(resultdf, aes(layer1, layer2, fill = training_r2)) + geom_tile() + facet_wrap(~layer3)
ggplot(resultdf, aes(layer1, layer2, fill = validation_r2)) + geom_tile() + facet_wrap(~layer3)
ggplot(resultdf, aes(layer1, layer2, fill = validation_r2 - training_r2)) + geom_tile() + facet_wrap(~layer3)
ggplot(resultdf, aes(layer1, layer2, fill = success)) + geom_tile() + facet_wrap(~layer3)
ggplot(resultdf, aes(layer1, layer2, fill = fit_time)) + geom_tile() + facet_wrap(~layer3)
