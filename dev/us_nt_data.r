# ==========================================================================
# Create neighborhood typology for the entire country
# ==========================================================================

if (!require("librarian")) install.packages("librarian")
librarian::shelf(timathomas/neighborhood, sf, tigris, tidycensus, tidyverse, leaflet, rmapshaper,htmlwidgets,htmltools,leaflet.extras)
options(tigris_use_cache = TRUE)

# ==========================================================================
# Create and save data
# ==========================================================================

us_states <- 
  states(cb = TRUE) %>% 
  st_set_geometry(NULL) %>% 
  filter(!STUSPS %in% c("AS", "GU", "MP", "VI")) %>% # have to omit
  pull(STUSPS) %>% 
  sort()

us_tracts <-
  map_df(us_states, function(states){
      ntdf(state = states, year = 2021, geometry = TRUE) %>%
      mutate(
        county_fips = str_sub(GEOID, 1, 5),
        year = 2021
        )
    }) %>% glimpse()

### LEFT OFF ###
  left_join(
    us_tracts,
    fips_codes %>%
      mutate(county_fips = paste0(state_code, county_code))
    ) %>%
  ms_simplify(keep = .3) %>%
  glimpse()

saveRDS(us_tracts, "~/git/timathomas/neighborhood/data/us_nt_tracts.rds")
write_csv(st_drop_geometry(us_tracts), "~/git/timathomas/neighborhood/data/us_nt_tracts.csv.bz2")

# ==========================================================================
# Create map
# ==========================================================================

nt_pal <-
    colorFactor(c(
        '#33a02c', # 'Mostly Asian', green
        '#1f78b4', # 'Mostly Black', blue
        '#e31a1c', # 'Mostly Latinx', red
        '#9b66b0', # 'Mostly Other', purple
        '#C95123', # 'Mostly White',
        '#1fc2ba', # 'Asian-Black',
        '#d6ae5c', # 'Asian-Latinx',
        '#91c7b9', # 'Asian-Other',
        '#b2df8a', # 'Asian-White',
        '#de4e4b', # 'Black-Latinx',
        '#71a1f5', # 'Black-Other',
        '#a6cee3', # 'Black-White',
        '#f0739b', # 'Latinx-Other',
        '#fb9a99', # 'Latinx-White',
        '#c28a86', # 'Other-White',
        '#fdbf6f', # '3 Group Mixed',
        '#cab2d6', # '4 Group Mixed',
        '#1d5fd1', # 'Diverse',
        '#FFFFFF'),  # 'Unpopulated Tract'
      domain = us_tracts$nt_conc,
      na.color = 'transparent'
        )

map <-
  leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles(
    "Stamen.TonerLines",
    options = providerTileOptions(opacity = 0.2),
    group = "Roads"
  ) %>%
  addProviderTiles(
    "OpenRailwayMap",
    options = leafletOptions(pane = "rail"),
    group = "Transit") %>%
  addProviderTiles(
    "CartoDB.PositronOnlyLabels",
    options = leafletOptions(pane = "maplabels"),
    group = "map labels") %>%
  addMiniMap(tiles = providers$CartoDB.Positron,
     toggleDisplay = TRUE, position = 'bottomleft') %>%
  addEasyButton(
    easyButton(
      icon='fa-crosshairs',
      title='My Location',
      onClick=JS('function(btn, map){ map.locate({setView: true}); }')
      )
    ) %>%
  addSearchOSM() %>%
  addLayersControl(baseGroups = "Segregation") %>%
  addPolygons(
    data = us_tracts,
    group = "Segregation",
    label = lapply(
      paste0('<b>', #state_name, '<br>',
                    county, '</b><br>',
                    'Tract: ', GEOID, '<br>',
                    NeighType, '<br>',
                    '% Asian ', scales::percent(pAsian), '<br>',
                    '% Black ', scales::percent(pBlack), '<br>',
                    '% Latino ', scales::percent(pLatinx), '<br>',
                    '% Other ', scales::percent(pOther), '<br>',
                    '% White ', scales::percent(pWhite)
        ), HTML
      ),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .6,
    color = ~nt_pal(nt_conc),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions =
      highlightOptions(
              color = "#ff4a4a",
              weight = 3,
              bringToFront = TRUE),
      # popup = ~popup,
      # popupOptions = popupOptions(maxHeight = 215, minWidth = 280, closeOnClick = TRUE),
    options = pathOptions(pane = "polygons")
    ) %>%
  leaflet::addLegend(
    data = us_tracts,
    position = "topright",
    pal = nt_pal,
    values = ~nt_conc,
    group = 'Segregation',
    className = 'Segregation',
    title = "Neighborhood Racial Segregation"
    )

map