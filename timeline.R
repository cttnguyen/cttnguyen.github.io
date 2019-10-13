# timeline.R

library(tibble)
library(lubridate)
library(ggplot2)
library(dplyr)

events = c("Crystal and Daniel meet", 
           "The first date", 
           "Move to Boston",
           "The proposal!", 
           "We say 'I do'")
categories = c(1, 0, 1, 0, 1)
months = c(8, 9, 6, 9, 2)
years = c(2014, 2016, 2019, 2019, 2021)


df = tibble(month = months, 
            year = years, 
            milestone = events, 
            category = categories)

df$date = with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df = df %>% arrange(desc(date))

positions = c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions = c(1, -1)

line_pos = data.frame(
  "date" = unique(df$date),
  "position" = rep(positions, length.out=length(unique(df$date))),
  "direction" = rep(directions, length.out=length(unique(df$date)))
)

df = merge(x = df, y = line_pos, by = "date", all = TRUE)
df = df[with(df, order(date, category)), ]

text_offset = 0.05

df$month_count = ave(df$date == df$date, df$date, FUN = cumsum)
df$text_position = (df$month_count * text_offset * df$direction) + df$position

month_buffer = 2

month_date_range = seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by = 'month')
month_format = format(month_date_range, '%b')
month_df = data.frame(month_date_range, month_format)

year_date_range = seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by = 'year')
year_date_range = as.Date(
  intersect(
    ceiling_date(year_date_range, unit = "year"),
    floor_date(year_date_range, unit = "year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)


timeline_plot = ggplot(df, aes(x = date, y = 0)) + 
  theme_classic()

# Plot horizontal black line for timeline
timeline_plot = timeline_plot + geom_hline(yintercept = 0, 
                                           color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot = timeline_plot + 
  geom_segment(data = df[df$month_count == 1, ], aes(y = position, yend = 0, xend = date), color = 'black', size = 0.2)

# Plot scatter points at zero and date
timeline_plot = timeline_plot + geom_point(aes(y = 0), size = 3)

# Don't show axes, appropriately position legend
timeline_plot = timeline_plot + theme(axis.line.y = element_blank(),
                                      axis.text.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_blank(),
                                      axis.ticks.x = element_blank(),
                                      axis.line.x = element_blank()
)

# Show year text
timeline_plot = timeline_plot + 
  geom_text(data = year_df, aes(x = year_date_range, 
                                y = -0.2, 
                                label = year_format, 
                                fontface = "bold"), 
            size = 2.5, 
            color = 'black')

# Show text for each milestone
timeline_plot = timeline_plot + 
  geom_text(aes(y = text_position, label=milestone), size = 2.5)

# flip to vertical
timeline_plot = timeline_plot + coord_flip()
print(timeline_plot)
