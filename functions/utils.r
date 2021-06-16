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

# Only applied to comm_report

custom_mean_report <- function(assignment, df) {
    
    out <- difference_in_means(comm_report ~ group, 
                               condition1 = 1,
                               condition2 = assignment,
                               data = df) %>% 
        tidy()
    
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
            custom_mean_diff(2, reply_vec[order], 4), # base comparison (male = base)
            custom_mean_diff(3, reply_vec[order], 5)
            ) # upvote comparison (male = base)
    
    return(out)
    
}

# Custom ggplot function 

test_group_plot <- function(df, plot_title) {
    
    df %>%
        mutate(term = recode(term,
                             "group2" = "Female",
                             "group3" = "Female + Upvotes",
                             "group4" = "Male",
                             "group5" = "Male + Upvotes")) %>%
        ggplot(aes(x = fct_reorder(outcome, estimate), y = estimate, ymax = conf.high, ymin = conf.low)) +
        geom_pointrange() +
        labs(title = plot_title, 
             y = "Estimate",
             x = "") +
        facet_wrap(~term, ncol = 4) +
        coord_flip() +
        geom_hline(yintercept = 0, linetype = 'dashed', col = 'red')
}

custom_group_plot <- function(df, plot_title, x_labels, n_col = 4) {
 
    df %>%
        mutate(term = recode(term,
                     "group2" = "Female",
                     "group3" = "Female + Upvotes",
                     "group4" = "Male",
                     "group5" = "Male + Upvotes")) %>%
        ggplot(aes(x = fct_reorder(outcome, estimate), y = estimate, ymax = conf.high, ymin = conf.low)) +
        geom_pointrange() +
        labs(title = plot_title, 
         y = "Estimate",
         x = "",
         subtitle = "Difference in means between control and treatment groups") +
        scale_x_discrete(labels = x_labels) +
        facet_wrap(~term, ncol = n_col) +
        coord_flip() +
        geom_hline(yintercept = 0, linetype = 'dashed', col = 'red') +
        theme(panel.spacing.x = unit(8, "mm"))
}

# Estimating conditional treatment effects

est_cate <- function(nested_df, assignment) {

    nested_df %>%
        mutate(model = map(data, ~custom_mean_report(assignment, df = .x))) %>%
        mutate(group = assignment)
           
               }