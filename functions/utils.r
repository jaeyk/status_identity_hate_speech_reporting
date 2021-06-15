# Copied and pasted from this URL: https://community.rstudio.com/t/computing-confidence-intervals-with-dplyr/31868
lower_ci <- function(mean, se, n, conf_level = 0.95){
    lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
    upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# Custom difference in means function
custom_mean_diff <- function(assignment, outcome, control = 1) {

    out <- difference_in_means(get(outcome) ~ group, 
                        condition1 = control,
                        condition2 = assignment,
                        data = df) %>% 
        tidy()
    
    out$outcome <- outcome 
    
    return(out)
}

# Apply mean diff across groups and vectors 
apply_mean_diff <- function(group, vec){

    map2_dfr(rep(group, times = length(vec)), 
         rep(vec, each = length(group)), custom_mean_diff)
    
}

# Reply dv is an exception to this rule
apply_mean_diff_reply <- function(order){
    
    out <- 
        bind_rows(
            custom_mean_diff(3, reply_vec[order], 2), # female upvote 
            custom_mean_diff(5, reply_vec[order], 4), # male upvote 
            custom_mean_diff(2, reply_vec[order], 4), # base comparison 
            custom_mean_diff(3, reply_vec[order], 5)
            ) # upvote comparison 
    
    return(out)
    
}