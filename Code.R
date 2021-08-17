df_netflix <- read.csv(list.files(path = "../input", recursive = T, full.names = T)[1])
df_netflix$date_added <- as.Date(df_netflix$date_added, format = "%B %d, %Y")
head(df_netflix)
df_by_date <- df_netflix %>% group_by(date_added,type) %>% summarise(addedToday = n()) %>% 
            ungroup() %>% group_by(type) %>% mutate(Total_Number_of_Shows = cumsum(addedToday), label = if_else(date_added == max(date_added,na.rm = T), as.character(type), NA_character_))


df_by_date  %>% 
                ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + geom_line(size = 2) + 
                    theme_bw(base_size = 20) + 
                    scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
                    theme(legend.position = 'none') +
                    geom_text_repel(aes(label = label), size = 8,na.rm = TRUE, nudge_y = 100)
   df_netflix %>% group_by(type) %>% mutate(country = fct_infreq(country)) %>% ggplot(aes(x = country)) + 
            geom_histogram(stat = 'count') + facet_wrap(~type, scales = 'free_x') + 
            theme_custom_sk_90 + coord_cartesian(xlim = c(1,10)) + scale_x_discrete(labels = function(x){str_wrap(x,20)}, breaks = function(x) {x[1:10]})
            df_show_categories <- df_netflix %>% 
                        select(c('show_id','type','listed_in')) %>% 
                        separate_rows(listed_in, sep = ',') %>%
                        rename(Show_Category = listed_in)
df_show_categories$Show_Category <- trimws(df_show_categories$Show_Category)
head(df_show_categories)
df_unique_categories <- df_show_categories %>% group_by(type,Show_Category) %>%  summarise()
df_category_correlations_movies <- data.frame(expand_grid(type = 'Movie', 
                                             Category1 = subset(df_unique_categories, type == 'Movie')$Show_Category,
                                             Category2 = subset(df_unique_categories, type == 'Movie')$Show_Category))
                                  
df_category_correlations_TV <-data.frame(expand_grid(type = 'TV Show', 
                                             Category1 = subset(df_unique_categories, type == 'TV Show')$Show_Category,
                                             Category2 = subset(df_unique_categories, type == 'TV Show')$Show_Category))
                                 
df_category_correlations <- rbind(df_category_correlations_movies,df_category_correlations_TV)
df_category_correlations$matched_count <- apply(df_category_correlations, MARGIN = 1,FUN = function(x) {
                                            length(intersect(subset(df_show_categories, type == x['type'] & Show_Category == x['Category1'])$show_id,
                                            subset(df_show_categories, type == x['type'] & Show_Category == x['Category2'])$show_id))})

df_category_correlations <- subset(df_category_correlations, (as.character(Category1) < as.character(Category2)) & (matched_count > 0))
# Change plot size to 8 x 3
options(repr.plot.width=14, repr.plot.height=10)

ggplot(subset(df_category_correlations, type == 'Movie'), aes(x = Category1, y = Category2, fill = matched_count)) + 
        geom_tile() + facet_wrap( ~type, scales = 'free') + theme_custom_sk_90 + scale_fill_distiller(palette = "Spectral") + 
            theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))
            ggplot(subset(df_category_correlations, type == 'TV Show'), aes(x = Category1, y = Category2, fill = matched_count)) + 
        geom_tile() + facet_wrap( ~type, scales = 'free') + theme_custom_sk_90 + scale_fill_distiller(palette = "Spectral") + 
            theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))
            df_netflix %>% select(c('show_id','cast','director')) %>% 
        gather(key = 'role', value = 'person', cast, director) %>% 
             filter(person != "") %>% separate_rows(person, sep = ',') -> df_show_people

df_show_people$person <- trimws(df_show_people$person)
head(df_show_people)
df_people_freq<- df_show_people %>% group_by(person,role) %>% 
                    summarise(count = n()) %>% arrange(desc(count))

df_people_freq %>% group_by(role) %>% top_n(10,count) %>% ungroup() %>% ggplot(aes(x = fct_reorder(person,count,.desc = T), y = count, fill = role)) + 
            geom_bar(stat = 'identity') + scale_x_discrete() + facet_wrap(~role, scales = 'free_x') + 
            theme_custom_sk_90 + theme(legend.position = 'none') + labs(x = 'Name of the actor / director')

            
