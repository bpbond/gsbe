# Utility functions
# BBL January 2025

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
    bind_rows(out, .id = "k") %>%
        mutate(k = as.integer(k))
} # do_k_fold

# Utility function: simple R2 computation
r2 <- function(preds, obs) {
    stopifnot(length(preds) == length(obs))
    rss <- sum((preds - obs) ^ 2)
    tss <- sum((obs - mean(obs)) ^ 2)
    return(1 - rss / tss)
}

# Utility function: root mean square error
rmse <- function(preds, obs) sqrt(mean((preds - obs) ^ 2))

# Utility function: extract and round linear model R2
lm_r2 <- function(m) round(summary(m)$adj.r.squared, 4)

# Utility function: ANOVA comparison of two linear models
anova_p_twomodels <- function(m1, m2) round(anova(m1, m2)$`Pr(>F)`[2], 4)
