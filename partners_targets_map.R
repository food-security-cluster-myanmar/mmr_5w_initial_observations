# map-partners-target-township

logp3_trans <- trans_new(
  name = "logp",
  trans = function(x) log(x + 3),
  inverse = function(x) exp(x) - 3,
  breaks = log_breaks()
)

ben %>% 
  group_by(admin3_pcode) %>% 
  summarise(partners = n_distinct(implementing_partners)) %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = partners), size = 0.1) +
  # scale_fill_gradient(trans = "reverse", breaks = c(1, 3, 5, 7, 9,11)) +
  scale_fill_viridis_c(option = "mako", direction = -1, breaks = c(1, 3, 5, 7, 9, 11)) +
  theme_bw() + 
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.7, 'cm')) +
  labs(title = "Map of number of partners by township",
       subtitle = "townships in grey do not have any partners present", 
       fill = "Partners") +
  
pin %>%    
    group_by(admin3_pcode) %>% 
    summarise(target_2022 = sum(target_2022)) %>% 
    mutate(target_2022 = round(target_2022, digits = 0), 
           target_2022 = recode(target_2022, 
                                '0' = NA_real_)) %>%
    right_join(pcode3_shape, by = "admin3_pcode") %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = target_2022), size = 0.1) +
  scale_fill_viridis_c(option = "mako", trans = "log10", direction = -1, begin = 0.15) +
    # scale_fill_gradient(trans = revlog_trans(10), breaks = c(100, 1000, 10000, 100000)) +
    theme_bw() +
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.key.size = unit(0.7, 'cm')) +
    labs(title = "Map of 2022 targets by township",
         subtitle = "townships in grey do not have any targets", 
         fill = "Targets")
  