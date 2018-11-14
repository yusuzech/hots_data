library(tidyverse)
file_cn <- read_lines("data/conversationstrings_cn.txt")

#tidy texts ----------------
file_table <- str_split_fixed(file_cn,"=",2) %>%
    as.tibble() %>%
    rename(key = V1, value = V2) %>%
    separate(key,into = c("key1","key2","key3"),sep = "/")


#get mapping table --------
#get the table that maps english to chinese
name_table <- file_table %>%
    dplyr::filter(key1 == "Character" & key2 == "Name") %>%
    mutate(english = key3,chinese = value) %>%
    select(english,chinese)

#some items have different names, so I need to add a table manually
extra_name_en <- c("Barbarian","Crusader","Gazlowe","Illidan","Kerrigan","Mira",
                 "TyraelMecha","Volskaya")
extra_name_cn <- c("桑娅","乔汉娜","加兹鲁维","伊利丹","凯瑞甘","米拉韩",
                        "机甲泰瑞尔","雅典娜")
name_table <- name_table %>%
    bind_rows(tibble(english = extra_name_en,
              chinese = extra_name_cn))
# get announcer rows---------------
same_player_announcer <- file_table %>%
    dplyr::filter(str_detect(key3,"HeroSelectPlayer"))


same_player_announcer_full <- same_player_announcer %>%
    mutate(hero = str_remove(key2,"A$|Announcer$")) %>%
    left_join(name_table,by = c("hero"="english")) %>%
    distinct(key2,key3,.keep_all = T)

text_table_same <- same_player_announcer_full %>%
    select(chinese,value)

output_table <- text_table_same %>%
    mutate(text_hyphen = str_c("• ",value,"\n")) %>%
    group_by(chinese) %>%
    summarise(text = reduce(text_hyphen,str_c)) %>%
    mutate(text = str_trim(text))
    

write_csv(output_table,"test.csv")

