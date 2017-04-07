
library(dplyr)

## function to standardize variables
stan_var <- function(x) { ( x - mean(x) ) / sd(x) }

## subset to kindergarten
df <- haven::read_spss('STAR_Students.sav') %>%
    select(stdntid, gender, race, starts_with('gk')) %>%
    na.omit() %>%
    mutate(female = as.integer(gender == 2),
           black = as.integer(race == 2),
           asian = as.integer(race == 3),
           hispc = as.integer(race == 4),
           natam = as.integer(race == 5),
           orace = as.integer(race == 6),
           t_smc = as.integer(gkclasstype == 1),
           t_rgc = as.integer(gkclasstype == 2),
           t_rga = as.integer(gkclasstype == 3),
           schid = as.integer(gkschid),
           tchid = as.integer(gktchid),
           incit = as.integer(gksurban == 1),
           subur = as.integer(gksurban == 2),
           rural = as.integer(gksurban == 3),
           urban = as.integer(gksurban == 4),
           frpl = as.integer(gkfreelunch == 1),
           spced = as.integer(gkspeced == 1),
           math = as.numeric(gktmathss),
           read = as.numeric(gktreadss),
           math_std = stan_var(math),
           read_std = stan_var(read)) %>%
    select(-starts_with('gk'), -gender, -race)

## make new school ID that is ordered integer
new_schid <- df %>%
    distinct(schid) %>%
    arrange(schid) %>%
    mutate(n_schid = row_number())

## make new teacher ID that is ordered integer
new_tchid <- df %>%
    distinct(tchid) %>%
    arrange(tchid) %>%
    mutate(n_tchid = row_number())

## merge new school and teacher IDs back in
df <- df %>%
    left_join(new_schid) %>%
    left_join(new_tchid) %>%
    select(-schid, -tchid) %>%
    rename(teacher = n_tchid,
           school = n_schid)

## write
write.table(df, file = 'tn_star_k.csv', quote = F, sep = ',', row.names = F)

