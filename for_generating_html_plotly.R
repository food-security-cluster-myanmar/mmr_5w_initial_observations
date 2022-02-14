# I think you need to coalesce the states and townships with pcode3_shape or start with pcode3_shape
# see if this works better if you used plotly instead of ggplotly?
tsp_map <- pcode3_shape %>% 
  left_join(ben %>%
              group_by(admin3_pcode) %>% 
              summarise(beneficiaries = sum(beneficiaries),
                        partners = n_distinct(implementing_partners),
                        activities = n_distinct(activity)), by = "admin3_pcode") %>% 
  left_join(pin %>%
              select(admin3_pcode, idps, target_2022, pin_new), by = "admin3_pcode") %>% 
  replace(is.na(.), 0) %>% 
  mutate(target_2022 = round(target_2022)) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1,
          aes(fill = target_2022,
              text = paste0(township, ",", "\n",
                            state, "\n",
                            "PIN 2022: ", pin_new, "\n",
                            "target 2022: ", target_2022, "\n",
                            "IDPs: ", idps, "\n",
                            "org count: ", partners, "\n",
                            "beneficiaries: ", beneficiaries))) +
  scale_fill_viridis_c(option = "mako", direction = -1, trans = "log10") + 
  labs(fill = "target 2022",
       title = "Map of townships by 2022 target") +
  theme_void() + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12)) 

plotly_map_targets <- ggplotly(tsp_map, tooltip = c("text")) %>%
  layout(showlegend = TRUE, legend = list(font = list(size = 6))) %>% 
  # plotly::style(hoveron = "fill") %>% # this does make all tooltips appear, but catching the edges is difficult sometimes
  layout(title = list(text = paste0("Map of townships by 2022 target",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; click and drag to select and zoom","</sup>")))

saveWidget(plotly_map_targets, "plotly_map_targets.html", selfcontained = FALSE, libdir = "lib")

saveWidget(frameableWidget(plotly_map_targets), "plotly_map_beneficiaries.html", selfcontained = FALSE, libdir = "lib")


# I think you need to coalesce the states and townships with pcode3_shape or start with pcode3_shape
tsp_map_ben <- pcode3_shape %>%
  # st_make_valid() %>% # not exactly sure what effect this is having 
  left_join(fsc %>%
              filter(unique_beneficiaries == "Yes") %>% 
              group_by(admin3_pcode) %>% 
              summarise(beneficiaries = sum(beneficiaries),
                        partners = n_distinct(implementing_partners),
                        activities = n_distinct(activity)), by = "admin3_pcode") %>% 
  left_join(pin %>% 
              select(admin3_pcode, target_2022), by = "admin3_pcode") %>% 
  ggplot() + 
  geom_sf(size = 0.1,
          aes(fill = beneficiaries,
              text = paste0(township, ",", "\n",
                            state, "\n",
                            "beneficiaries: ", beneficiaries, "\n",
                            "org count: ", partners, "\n",
                            "act count: ", activities, "\n",
                            "target 2022: ", target_2022))) +
  scale_fill_viridis_c(option = "mako", trans = "log10", direction = -1, begin = 0.15) + 
  labs(fill = "beneficiaries",
       title = "Map of townships by beneficiaries reached in 2021") +
  theme_void() + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12)) 

plotly_map_beneficiaries <- ggplotly(tsp_map_ben, tooltip = c("text")) %>%
  #   # this doesn't exactly work they way I'd like it to
  #   # to be very clear, hoveron = "fills" will cause the thing to crash 
  # style( 
  #   hoveron = "fills",
  #   # override the color mapping
  #   line.color = toRGB("white"),
  #   # don't apply these style rules to the first trace, which is the background graticule/grid
  #   traces = seq.int(2, length(tsp_map_ben$x$data))
  # ) %>%
  hide_legend() %>% 
  layout(showlegend = TRUE, legend = list(font = list(size = 6))) %>% 
  # plotly::style(hoveron = "fill") %>% # this does make all tooltips appear, but catching the edges is difficult sometimes
  layout(title = list(text = paste0("Map of townships by beneficiaries reached in 2021",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; click and drag to select and zoom","</sup>")))

saveWidget(plotly_map_beneficiaries, "plotly_map_beneficiaries.html", selfcontained = FALSE, libdir = "lib")

saveWidget(frameableWidget(plotly_map_beneficiaries), "plotly_map_beneficiaries.html", selfcontained = FALSE, libdir = "lib")


```{r beneficiaries-map-ggplotly, fig.height=10}

# frameWidget("plotly_map_beneficiaries.html", width = "100%", height = "400px")

# htmltoools::includeMarkdown("plotly_map_beneficiaries.html")

htmltools::includeHTML("plotly_map_beneficiaries")

```

<br>
  
  ### 7.1 Map of 2022 targets by township
  
  
  ```{r targets-map-ggplotly, fig.height=10}
# I think you need to coalesce the states and townships with pcode3_shape or start with pcode3_shape
# see if this works better if you used plotly instead of ggplotly?
tsp_map <- pcode3_shape %>% 
  left_join(ben %>%
              group_by(admin3_pcode) %>% 
              summarise(beneficiaries = sum(beneficiaries),
                        partners = n_distinct(implementing_partners),
                        activities = n_distinct(activity)), by = "admin3_pcode") %>% 
  left_join(pin %>%
              select(admin3_pcode, idps, target_2022, pin_new), by = "admin3_pcode") %>% 
  replace(is.na(.), 0) %>% 
  mutate(target_2022 = round(target_2022)) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1,
          aes(fill = target_2022,
              text = paste0(township, ",", "\n",
                            state, "\n",
                            "PIN 2022: ", pin_new, "\n",
                            "target 2022: ", target_2022, "\n",
                            "IDPs: ", idps, "\n",
                            "org count: ", partners, "\n",
                            "beneficiaries: ", beneficiaries))) +
  scale_fill_viridis_c(option = "mako", direction = -1, trans = "log10") + 
  labs(fill = "target 2022",
       title = "Map of townships by 2022 target") +
  theme_void() + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12)) 

ggplotly(tsp_map, tooltip = c("text")) %>%
  layout(showlegend = TRUE, legend = list(font = list(size = 6))) %>% 
  # plotly::style(hoveron = "fill") %>% # this does make all tooltips appear, but catching the edges is difficult sometimes
  layout(title = list(text = paste0("Map of townships by 2022 target",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; click and drag to select and zoom","</sup>")))

```


<br><br>
  
  ### 7.2 Map of 2021 beneficiaries by township
  
  ```{r beneficiaries-map-ggplotly, fig.height=10}

# I think you need to coalesce the states and townships with pcode3_shape or start with pcode3_shape
tsp_map_ben <- pcode3_shape %>%
  # st_make_valid() %>% # not exactly sure what effect this is having 
  left_join(fsc %>%
              filter(unique_beneficiaries == "Yes") %>% 
              group_by(admin3_pcode) %>% 
              summarise(beneficiaries = sum(beneficiaries),
                        partners = n_distinct(implementing_partners),
                        activities = n_distinct(activity)), by = "admin3_pcode") %>% 
  left_join(pin %>% 
              select(admin3_pcode, target_2022), by = "admin3_pcode") %>% 
  ggplot() + 
  geom_sf(size = 0.1,
          aes(fill = beneficiaries,
              text = paste0(township, ",", "\n",
                            state, "\n",
                            "beneficiaries: ", beneficiaries, "\n",
                            "org count: ", partners, "\n",
                            "act count: ", activities, "\n",
                            "target 2022: ", target_2022))) +
  scale_fill_viridis_c(option = "mako", trans = "log10", direction = -1, begin = 0.15) + 
  labs(fill = "beneficiaries",
       title = "Map of townships by beneficiaries reached in 2021") +
  theme_void() + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12)) 

ggplotly(tsp_map_ben, tooltip = c("text")) %>%
  #   # this doesn't exactly work they way I'd like it to
  #   # to be very clear, hoveron = "fills" will cause the thing to crash 
  # style( 
  #   hoveron = "fills",
  #   # override the color mapping
  #   line.color = toRGB("white"),
  #   # don't apply these style rules to the first trace, which is the background graticule/grid
  #   traces = seq.int(2, length(tsp_map_ben$x$data))
  # ) %>%
  hide_legend() %>% 
  layout(showlegend = TRUE, legend = list(font = list(size = 6))) %>% 
  # plotly::style(hoveron = "fill") %>% # this does make all tooltips appear, but catching the edges is difficult sometimes
  layout(title = list(text = paste0("Map of townships by beneficiaries reached in 2021",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; click and drag to select and zoom","</sup>")))

```

