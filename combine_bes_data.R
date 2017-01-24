library(tidyverse)
library(readxl)

# Read in modulo basico data
mb <- read.csv('data/modulo_basico/Compiled_Dados Provincia Maputo.csv', skip = 2)

# Clean it up ----------

# clean up column names
names(mb)[1:3] <- c('year', 'district', 'age')
names(mb)[4:ncol(mb)] <-
  gsub('X',
       'week_',
       names(mb)[4:ncol(mb)])

# interpolate year
for (i in 1:nrow(mb)){
  mb$year[i] <-
    ifelse(is.na(mb$year[i]),
           mb$year[i-1],
           mb$year[i])
}

# make long
mb <- gather(mb,
             key,
             value,
             week_1:week_53)

# Clean up week
names(mb)[names(mb) == 'key'] <- 'week'
mb$week <- as.numeric(gsub('week_', '', mb$week))

# Read in the dhes2 data ---------
d2016 <- read_excel('data/dhes2/BES de 2016 SIS-MA.xls', skip = 1)
d2017 <- read_excel('data/dhes2/BES de 2017 SIS-MA.xls', skip = 1)
d2 <- bind_rows(d2016, d2017)
rm(d2016, d2017)

# Fix column names
names(d2) <- c('period',
               'district',
               'age_0_4',
               'age_5_plus')

# Generate more columns
d2$year <- as.numeric(substr(d2$period, 1, 4))
d2$week <- as.numeric(substr(d2$period, 6, nchar(d2$period)))

# Make long
d2 <- gather(d2,
             key, value, age_0_4:age_5_plus)

# Fix names
names(d2)[names(d2) == 'key'] <- 'age'
d2$age <- ifelse(d2$age == 'age_0_4', '0-4 anos',
                 ifelse(d2$age == 'age_5_plus', '5 anos +',
                        NA))

d2 <- d2[,names(mb)]


# Specify sources
d2$src <- 'DHES2'
mb$src <- 'MB'

# Combine data
df <- bind_rows(d2, mb)

# Make uppercase district
df$district <- toupper(df$district)

# Standardize district names
df$district[df$district == 'MANHIÇA'] <- 'MANHICA'
df$district[df$district == 'MATUTUÌNE'] <- 'MATUTUINE'



# Get the overlap period
df <- df %>%
  left_join(
    df %>%
      group_by(year, district, age, week) %>%
      summarise(overlap = length(unique(src)) > 1),
    by = c("year", "district", "age", "week")
  )

# Arrange by date
df <- df %>%
  arrange(year, week)

# Get a "date" column (approximate)
df$date <- as.Date(paste0(df$year, '-01-01')) + (7 * (df$week -1))
  
# Write data
write_csv(df, 'data/cleaned/cases.csv')

# # View the overlap period
# 
# ggplot(data = df %>%
#          filter(overlap) %>%
#          group_by(date, district, src) %>%
#          summarise(value = sum(value, na.rm = TRUE)),
#        aes(x = date,
#            y = value,
#            color = src)) +
#   geom_line() +
#   facet_wrap(~district,
#              scales = "free") +
#   labs(title = 'Concordance of DHES2 and MB data',
#        subtitle = 'During the period for which both systems were active',
#        x = 'Date',
#        y = 'Cases') +
#   scale_color_manual(name = 'Source',
#                      values = c('red', 'green')) 
#   
