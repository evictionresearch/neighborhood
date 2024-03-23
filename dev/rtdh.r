# ==========================================================================
# The Rent's Too Damn High script
# Tool to download and analyze apartment list and zillow data. 
# ApartmentList Data source: https://www.apartmentlist.com/research/category/data-rent-estimates
# Zillow Data source: https://www.zillow.com/research/data/
# ==========================================================================

# ==========================================================================
# Download the data: 
# so far, this is a manual process because of the dropdown menu. 
# Apartment List: 
#   1. go to https://www.apartmentlist.com/research/category/data-rent-estimates
#   2. Select Historic Rent Estimates and download
#   3. Save the file to the repo
# ==========================================================================

options(width=Sys.getenv("COLUMNS"), setWidthOnResize=TRUE) 
librarian::shelf(timathomas/colorout, janitor, qs, tidyverse)
data_path <- "/Users/timthomas/Downloads/"

csv <- sort(list.files(data_path, "Apartment_List", full.names = TRUE), decreasing = TRUE)
al_data <- 
  read_csv(csv) %>% 
  pivot_longer(cols = c(`2017_01`:last_col()), names_to = "date") %>% 
  mutate(date = ym(date))
qsave(al_data, "~/git/evictionresearch/neighborhood/data/al_data.qs")

glimpse(al_data)

# ==========================================================================
# Subset to specified area: Oregon is an example here
# ==========================================================================

check <- 
  al_data %>% 
  filter(
    state == "California", 
    location_type == "County", 
    # location_name == "Multnomah County, OR", 
    bed_size == "overall", 
    # year(date) > 2018
    ) %>% 
  group_by(location_name) %>%
  mutate(
    max_rent = max(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate( 
    location_name = reorder(location_name, -max_rent)
    ) 
levels(check$location_name)
# ==========================================================================
# Create plot
# ==========================================================================

ggplot() +
  theme_minimal() +
  geom_line(data = check,
            aes(x = date, y = value, col = location_name)) +
  # scale_color_brewer(palette = "Set2") +
  geom_rect(aes(xmin = as.Date("2020-04-01"), xmax = as.Date("2022-07-30"), 
                ymin = -Inf, ymax = Inf), 
            fill = "grey80", alpha = 0.5) +
  labs(title = paste0(check$state, " Overall Monthly Rents"),
       y = "Rent",
       x = "Year"
       # caption = "Urban Displacement Project\nData source: HUD Fair Market Rent data &\nthe Bureau of Labor Statistics Consumer Price Index"
       ) +
  guides(color = guide_legend(title = "County")) +
  # coord_cartesian(ylim = c(1500, 2500)) +
  scale_y_continuous(labels = scales::dollar,
                     sec.axis = sec_axis(~(.*12)/.3, 
                                         name = "Income needed to afford rent", 
                                         labels = scales::dollar)
  ) +
  # scale_x_continuous(breaks=c(min(fmr$year):max(fmr$year))) +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0),
    axis.title.x = element_text(margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10)),
        panel.grid.minor.x = element_blank())

# ==========================================================================
# Describe increase
# ==========================================================================

check %>% filter(date < "2020-04-01") %>% group_by(location_name) %>% reframe(mean_rent_pre = mean(value, na.rm = TRUE)) %>% 
left_join(
  check %>% filter(date > "2022-07-30") %>% group_by(location_name) %>% reframe(mean_rent_post = mean(value, na.rm = TRUE))
  ) %>% 
mutate(change_in_rent = (mean_rent_post-mean_rent_pre)/mean_rent_pre) %>% arrange(desc(change_in_rent)) %>% data.frame()