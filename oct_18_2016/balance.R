
# libraries
libs <- c('dplyr', 'foreign', 'RItools')
suppressMessages(lapply(libs, require, character.only = TRUE))

## read data
df <- read.dta('./data/analytic_sample_old.dta') %>% tbl_df()

## collapse data
cdf <- df %>%
    mutate(treat = ifelse(t_assign == 'C', 0, 1),
           daynum = ifelse(daynum == 'm', 1,
                    ifelse(daynum == 'tu', 2,
                    ifelse(daynum == 'w', 3,
                    ifelse(daynum == 'th', 4, 5))))) %>%
    group_by(treattestdate) %>%
    summarise(pell = sum(pell),
              female = sum(female),
              age = sum(age),
              firstgen = sum(firstgen),
              hsgpa = sum(hsgpa),
              treat = max(treat),
              daynum = max(daynum),
              count = n())

## save data
write.table(cdf,
            file = './data/clustered_version.csv',
            sep = ',', quote = FALSE, row.names = FALSE)

## view data
cdf %>% data.frame(.)

## perform balance check
check <- xBalance(treat ~ pell + female + age,
                  strata = as.factor(cdf$daynum),
                  data = cdf,
                  report = c('all'))

## overall chi^2
check

## plot
plot(check)
