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
rsvp_names = rsvp_df$`Please list the first and last names of those attending the Saturday wedding, separated by commas (for example, "John Smith, Jane Smith, Jenny Smith").`

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
            Attending = pmax(`Number Attending the Friday Tea Ceremony`,
                            `Number Attending the Saturday Wedding & Reception`)) %>%
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

l <- list(color = toRGB("lightgray"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = F,
  # lakecolor = toRGB('white'),
  subunitcolor = toRGB('white')
)
p <- plot_ly(df, z = ~Attending, text = ~hover, locations = ~State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = ~Attending, colors = 'Blues', 
        marker = list(line = l), hoverinfo = 'text',
        colorbar = list(title = "RSVPs", len = 1),
        colorscale = list(c(0, "white"), list(1, "rgb(0,51,102)")),
        height = "100%", width = "100%"
) %>% 
  colorbar(title = 'RSVPs', limits = c(0, 25)) %>%
  layout(geo = g) %>% 
  add_annotations(xref = 'paper', yref = 'paper', x = 0.5, y = -0.2, 
                  text = paste("*Last updated", Sys.Date()),
                  showarrow = F, font = list(size = 10)) #%>% 
  #plotly_build()
p
#saveRDS(p, 'rsvp.rds')
