# An optional custom script to run before Hugo builds your site.
# You can delete it if you do not need it.
## @knitr makeplot

library(plotly)
library(tidyverse)
library(googlesheets4)
library(openintro)

#sheets_auth(email = "crystalmarriesdaniel@gmail.com")
gs4_auth(email = "crystalmarriesdaniel@gmail.com")
invitations = read_sheet("https://docs.google.com/spreadsheets/d/1mwJ75RelvJ55I4E0B-CNApGICnRlaSuTCFX8s9OgSAQ/edit#gid=409297580", 
                         sheet = 'Invitations', skip = 3) %>% 
  filter(!is.na(`Short Name`)) %>% 
  unite(col = "Address", contains("Address Line"), sep = " ", na.rm = T) %>% 
  transmute(`Inv #`,
            State = str_extract(Address, "([A-Z]{2}|Norway|Vietnam)$"),
            Category = case_when(
              grepl("Family Friends", Category) ~ "Friends",
              grepl("Family", Category) ~ "Family",
              grepl("Friends", Category) ~ "Friends",
              grepl("Party", Category) ~ "Friends",
              grepl("friends", Category) ~ "Friends"))
invitations$State[which(invitations$`Inv #` == 30)] <- "Peru"
crosswalk = read_sheet("https://docs.google.com/spreadsheets/d/1mwJ75RelvJ55I4E0B-CNApGICnRlaSuTCFX8s9OgSAQ/edit#gid=409297580", 
                       sheet = 'Invitations Crosswalk')
rsvp_df <- read_sheet("https://docs.google.com/spreadsheets/d/1ZwOWDuDJZc7m_LoD3RqU4FnDdfDZyvVn9jHjTQxp8JY/edit#gid=999873728")

# Extract name field
rsvp_names = rsvp_df$`Please list the first and last names of those attending, separated by commas (for example, "John Smith, Jane Smith, Jenny Smith").`

# Create variable for full name of first person on each RSVP
rsvp_df$full_name = rsvp_names %>% 
  strsplit(",") %>% 
  sapply(function(x) return(x[1]))

# Create variables for first and last name
rsvp_df = rsvp_df %>% separate(full_name, into = c("First Name", 
                                                   "Last Name"))

df <- rsvp_df %>%
  transmute(`First Name`, 
            `Last Name`,
            Attending = `Number Attending Wedding and Reception on Saturday, February 26th, 2022`) %>%
  left_join(crosswalk, by = c("First Name", "Last Name")) %>% 
  filter(!is.na(`Inv #`)) %>% 
  left_join(invitations, by = "Inv #") %>% 
  select(Attending, State, Category, `Inv #`) %>% 
  add_row(State = 'MA', Category = 'Bride & Groom', Attending = 2, `Inv #` = 2) %>% 
  distinct() %>% 
  group_by(State, Category) %>% 
  summarise(Attending = sum(Attending)) %>% 
  mutate(hover = paste0(Category, ": ", Attending)) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  summarise(Attending = sum(Attending), hover = paste(hover, collapse = "<br>")) %>% 
  add_row(State = state.abb[which(!(state.abb %in% .$State))],
          Attending = 0,
          hover = "") %>% 
  mutate(hover = paste0(abbr2state(State), "<br>", hover))

l <- list(color = toRGB("#343a40"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'usa',
                    scale = 1),
  showlakes = F,
  # lakecolor = toRGB('white'),
  subunitcolor = toRGB('white'),
  bgcolor = "#343a40"
)
p <- plot_ly(df, z = ~Attending, text = ~hover, locations = ~State,
             type = 'choropleth', locationmode = 'USA-states', 
             width = 500, 
             color = ~Attending, colors = 'Blues', 
             marker = list(line = l), hoverinfo = 'text',
             colorbar = list(title = list(text = "RSVPs"), 
                             titlefont = list(color = '#ffffff'), 
                             len = 0.75, y = 0.875, x = 0.9,
                             tickfont = list(color = "#ffffff"),
                             outlinecolor = 'transparent'),
             colorscale = list(c(0, "white"), list(1, "#001933"))
) %>% 
  colorbar(title = 'RSVPs', limits = c(0, 25)) %>%
  add_annotations(xref = 'paper', yref = 'paper', x = 0.5, y = 0.1, 
                  text = paste("*Last updated", format(Sys.Date(), "%m/%d/%Y")),
                  showarrow = F, font = list(size = 10, color = 'white')) %>% 
  layout(geo = g,
         paper_bgcolor  = "#343a40",
         margin=list(t=0,l=0,r=0,b=0,pad=0))
p
htmlwidgets::saveWidget(
  as_widget(p), selfcontained = T,
  file = "./static/rsvpmap.html",
  background = "#343a40"
)

saveRDS(p, 'rsvp.rds')

