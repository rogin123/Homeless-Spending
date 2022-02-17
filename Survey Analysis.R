### Load in packages
library(tidyverse)
library(janitor)
library(usdata)
library(urbnthemes)
library(ggrepel)
library(Cairo)
library(qdap)
library(scales)
set_urbn_defaults()

# Read in PHA data
pha <- read_csv('PHA Survey Final 2.8.csv') %>% 
  mutate(Progress = case_when(
    Progress <= 25 ~ "<25", 
    Progress > 25 & Progress <=50 ~ "25-50", 
    Progress >50 & Progress <= 75 ~ "50-75", 
    Progress > 75 ~ "75-100"
  )) %>% 
  relocate("State.y", .after = `What other resources do you plan to use to complement EHVs? Pick all that apply. - Selected Choice`) %>% 
  relocate("PHA Code.y", .after = `What other resources do you plan to use to complement EHVs? Pick all that apply. - Selected Choice`) %>% 
  relocate("EHV Vouchers.y", .after = `What other resources do you plan to use to complement EHVs? Pick all that apply. - Selected Choice`) 

# read in coc data
coc <- read_csv('CoC Survey Final.csv', skip = 1) %>% 
  filter(rowSums(is.na(pha[6:29])) != ncol(pha[6:29])) %>% 
  mutate(Progress = case_when(
    Progress <= 25 ~ "<25", 
    Progress > 25 & Progress <=50 ~ "25-50", 
    Progress >50 & Progress <= 75 ~ "50-75", 
    Progress > 75 ~ "75-100"
  )) 
# get list of pha and coc that fully completed the survey
pha_complete_codes <- pha$`PHA Code.y`


# Read in HUD Picture data with PHA sizes
hud_picture <- read_csv('hud_picture.csv') %>% 
  filter(Code %in% pha_complete_codes) %>% 
  select(c(2:3)) %>% 
  mutate(hud_picture = case_when(
    `Subsidized units available` < 500 ~ "Very Small", 
    `Subsidized units available` >= 500 &  `Subsidized units available` < 2000 ~ "Small", 
    `Subsidized units available` >= 2000 &  `Subsidized units available` < 4000 ~ "Medium", 
    `Subsidized units available` >= 4000 ~ "Large"
  ))
 

# Add in column with hud picture data
pha <- pha %>% left_join(hud_picture, by = c("PHA Code.y" = "Code")) 
# check which codes from survey phas aren't in the HUD pictures data 
pha_complete_codes[c(which(!pha_complete_codes %in% hud_picture$Code))]

# find quartiles to bin hud picture data
quantile(hud_picture$`Subsidized units available`)

###########################################################
##### MAKE BAR CHARTS FOR PHA AND COC SURVEY DATA #########
###########################################################
# PHA Bar Charts  -----------------------------------------------------

x <- data.frame(table(unlist(strsplit(coc[[]], ","))))

tmp_var <- data.frame(table(unlist(strsplit(pha[[10]], ","))))

a <- wfm(pha[[10]], grouping.var = c(pha$small_rural))

# Test the code before running the full loop 
data.frame(table(unlist(strsplit(pha[[29]], ",")))) %>%  
  filter(Var1 != "") %>%
  ggplot()+
  geom_bar(aes(x = reorder(Var1, -Freq), y= Freq), stat = "identity")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  labs(title  = str_wrap(paste(gsub( "(\\?).*", "\\1", names(pha[10])))),
       subtitle = paste("n = ",length(which(!is.na(pha[[10]]))),
                        "\nOmitted = ", length(which(is.na(pha[[10]])))),  
       x = NULL)

ggsave(tmp, file=paste0("plot_27_pct.pdf"), width = 28, height = 14, units = "cm", device = cairo_pdf)

# loop through each column and create frequency bar chart
# first 5 columns are descriptive data
for (i in 6:29) {
  # # filter data based on size
  # tmp_v_small <- pha %>% filter(hud_picture == "Very Small") 
  # tmp_small <-  pha %>% filter(hud_picture == "Small")  
  # tmp_med <- pha %>% filter(hud_picture == "Medium") 
  # tmp_large <-  pha %>% filter(hud_picture == "Large")  
  # 
  # # get word frequency for each size 
  # 
  # tmp_v_small <-as.data.frame(table(unlist(strsplit(tmp_v_small[[i]], ",")))) %>% 
  #   rename("Very Small" = Freq)
  # 
  # tmp_small <-as.data.frame(table(unlist(strsplit(tmp_small[[i]], ",")))) %>% 
  #   rename(Small = Freq)
  # 
  # tmp_med <-as.data.frame(table(unlist(strsplit(tmp_med[[i]], ",")))) %>% 
  #   rename(Medium = Freq)
  # 
  # tmp_large <- data.frame(table(unlist(strsplit(tmp_large[[i]], ","))))%>% 
  #   rename(Large = Freq)
  # 
  # 
  # temp_facet <- left_join(tmp_v_small, tmp_small, by = c("Var1")) %>% 
  #   left_join(tmp_med, by = c("Var1")) %>% 
  #   left_join(tmp_large, by = c("Var1")) %>% 
  #   pivot_longer(cols = c(2:5), names_to = "size")
  # 
  # temp_facet <- temp_facet %>% 
  #   ggplot()+
  #   geom_bar(aes(x = reorder(Var1, -value), y= value), stat = "identity")+
  #   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  #   labs(title  = str_wrap(paste(gsub( "(\\?).*", "\\1", names(pha[i])))),
  #        subtitle = paste("n = ",length(which(!is.na(pha[[i]]))),
  #                         "\nOmitted = ", length(which(is.na(pha[[i]])))),  
  #        x = NULL)+
  #   facet_wrap(~size, ncol = 1)
  #split the row values by each multiple choice selection, which are seperated by commas
  temp <- data.frame(table(unlist(strsplit(pha[[i]], ",")))) %>%
    filter(Var1 != "") %>%
    ggplot()+
    geom_bar(aes(x = reorder(Var1, -Freq), y= Freq), stat = "identity")+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(title  = str_wrap(paste(gsub( "(\\?).*", "\\1", names(pha[i])))),
         subtitle = paste("n = ",length(which(!is.na(pha[[i]]))),
                          "\nOmitted = ", length(which(is.na(pha[[i]])))),
         x = NULL)
  
  ggsave(temp, file=paste0("plot_", i,".pdf"), width = 28, height = 14, units = "cm", device = cairo_pdf)
}

temp

# CoC Bar Charts ----------------------------------------------------------


coc %>% 
  group_by(Progress) %>% 
  summarise(n = n())

# number of groups selected (not unique) 
test <- data.frame(table(str_count(coc[[9]], ",")))
tmp

tmp <- data.frame(prop.table(table(unlist(strsplit(coc_survey[[15]], ","))))) %>% 
  filter(Var1 != "") %>% 
  ggplot()+
  geom_bar(aes(x = reorder(Var1, -Freq), y= Freq), stat = "identity")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1))+
  labs(title  = str_wrap(paste(gsub( "(\\?).*", "\\1", names(coc_survey[15])))),
       subtitle = paste("n = ",length(which(!is.na(coc_survey[[15]]))),
                     "\nNA = ", length(which(is.na(coc_survey[[15]])))),  
       x = NULL)
ggsave(tmp, file=paste0("coc_15.pdf"), width = 28, height = 14, units = "cm", device = cairo_pdf)


for (i in 6:length(coc)) {
  #split the row values by each multiple choice selection, which are seperated by commas
  temp <- data.frame(table(unlist(strsplit(coc[[i]], ",")))) %>%  
    filter(Var1 != "") %>%
    ggplot()+
    geom_bar(aes(x = reorder(Var1, -Freq), y= Freq), stat = "identity")+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    labs(title = paste(gsub( "(\\?).*", "\\1", names(coc[i])),"\n n = ",length(which(!is.na(coc[[i]])))),  
         x = NULL)
  
  ggsave(temp, file=paste0("plot_", i,".pdf"), width = 28, height = 14, units = "cm", device = cairo_pdf)
}


# small_rural <- crosswalk %>% filter(Small_Rural_PHA == "Yes") %>% pull(CoCName)
# 
# coc <- coc %>% 
#   mutate(small_rural = case_when(
#     coc$`Basic Information - What CoC?` %in% small_rural ~ "Yes", 
#     TRUE ~ "No"))
# 
# x <- data.frame(table(unlist(strsplit(coc[[]], ",")))) 
# 
# wfm(coc[[6]], grouping.var =coc$small_rural,char2space = "," )
# 
# 

###### DATA CLEANING ##########
# read in crosswalk of pha and coc
crosswalk <- read_csv('pha_coc_crosswalk_clean.csv')

# Filter the crosswalk for EHV coverage (coc or pha have filled out survey)
ehv_coverage <- crosswalk %>% 
  filter(PHA_Code %in% pha_complete | CoCName %in% coc_complete) %>% 
  group_by(PHA_Code) %>% 
  distinct(PHA_Code) 

# Filter the crosswalk for EHV coverage (coc and pha have filled out survey)
pha_coc_coverage <- crosswalk %>% 
  filter(PHA_Code %in% pha_complete & CoCName %in% coc_complete) %>% 
  group_by(PHA_Code) %>% 
  distinct(PHA_Code) 

# CoC and corresponding PHA that confirmed match through email or MOU checks
confirm_backfill <- read_csv('Backfill_mou_list_2.9.csv') %>% 
  select(CoC, PHA)

# CoCs that we have a confirmed match for (email/mou)
coc_match <- unique(confirm_backfill$CoC)
# PHAs that we have a confirmed match for (email/mou)
pha_match <- unique(confirm_backfill$PHA)


codes <- crosswalk %>% 
  relocate(State, .before = PHA_Code) %>%  
  relocate(`EHV Vouchers`, .after = PHA_Code)

# # read in pha survey and filter out fully empty rows
coc_survey <- read_csv('CoC Survey Clean 1.5.csv', skip = 1)

pha_survey <- read_csv('PHA Survey Clean 1.4.csv')
  filter(rowSums(is.na(pha[6:29])) != ncol(pha[6:29]))

# get list of pha and coc that fully completed the survey
pha_complete <- pha$`PHA Code.y`

coc_complete <- coc$`Basic Information - What CoC?`

# Find the PHA that have a coc survey complete but not a pha survey
coc_not_pha <- setdiff(crosswalk$PHA_Name, pha_complete)

# test to make sure that all of the CoC in this list are CoC that completed the survey
coc_not_pha <- crosswalk %>% 
  filter(PHA_Name %in% coc_not_pha)

# result is that yes, all CoC left over completed a survey 
intersect(coc_not_pha$PHA_Code, pha_complete)

# pivot wider the list of PHAs that have a CoC that completed the survey 
backfill <- coc_not_pha %>% 
  dplyr::select("PHA_Code","PHA_Name", "CoCName", "Small_Rural_PHA") %>% 
  group_by(PHA_Code) %>%
  mutate(id = 1:n())  %>% 
  ungroup() 
  # pivot_wider(id_cols = c(starts_with("PHA"), Small_Rural_PHA),
  #           names_from = id,
  #           values_from = CoCName) # max # of CoCs a PHA is associated with is 23


duplicates <- backfill %>% 
  group_by(PHA_Name) %>% 
  mutate(pha_flag = case_when(n()>1 ~ 1, 
                              TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(CoCName) %>% 
  mutate(
    coc_flag = case_when(n()>1 ~ 1, 
                         TRUE ~ 0),
    coc_backfill_flag = case_when(
      CoCName %in% coc_match ~ 1,
      TRUE ~ 0),
    pha_backfill_flag = case_when(
      PHA_Name %in% pha_match ~ 1, 
      TRUE ~ 0),
      backfill_flag = case_when(
        pha_flag == 0 & coc_flag == 0 ~ 1, 
        pha_backfill_flag ==1 ~ 1,
        pha_backfill_flag ==0 & coc_backfill_flag==1 ~ 2,
        TRUE ~ 0)
    ) %>% 
  ungroup() %>% 
  group_by(PHA_Name) %>% 
  mutate(statustype = mean(backfill_flag)) 

# Names of CoCs/PHAs with one to one match to PHA for backfill 
coc_one_backfill <- duplicates$CoCName
pha_one_backfill <- duplicates$PHA_Name

test <- crosswalk %>% filter(CoCName %in% coc_one_backfill & PHA_Name %in% pha_one_backfill) %>% 
  relocate(State, .before = PHA_Code) %>%  
  relocate(`EHV Vouchers`, .after = PHA_Code)

coc_backfill <- coc %>% filter(`Basic Information - What CoC?` %in% coc_one_backfill)

write.csv(coc_backfill, "coc_backfill.csv")

# 36 PHAs have more than 1 CoC 
ptest <- duplicates %>% filter(pha_flag==1) %>% group_by(PHA_Name) %>% distinct(PHA_Name)

# 54 CoC answered for more than 1 PHA
ctest <- duplicates %>% filter(coc_flag==1) %>% group_by(CoCName) %>% distinct(CoCName)

mou_check <- ctest$CoCName

mou <- coc_survey %>% filter(`Basic Information - What CoC?` %in% mou_check) %>% 
  filter(!is.na(`Please upload a copy of your MOU (PDF or Word Document) - Id`))

outreach_list <- mou$`Basic Information - What CoC?` %>% 
  as.data.frame()



#  72 - PHAs that we have no corresponding CoC for (remove from list)
duplicates%>% 
  filter(statustype == 2) %>% 
  distinct(PHA_Name) %>% 
  nrow()

# 84 - PHAs that we backfilled for 
duplicates%>% 
  filter(backfill_flag == 1) %>% 
  distinct(PHA_Name) %>% 
  nrow()

# 16 that we have neither CoC nor PHA information for 
duplicates%>%  
  filter(backfill_flag == 0) %>% 
  distinct(PHA_Name) %>% 
  nrow()


#### MOUs Received
coc_mou_count <- read_csv('CoC Survey Clean 1.5.csv') %>% 
  filter(!is.na(Q22_Type))

pha_mou_count <- read_csv('PHA Survey Clean 1.4.csv') %>% 
  filter(!is.na(`Please upload a copy of your MOU (PDF or Word Document) - Size`))

# CoC Universe
coc_outreach <- read_csv('CoC_Contacts_notes (2).csv') %>% 
  group_by(OrgName) %>% 
  distinct(OrgName)

###########################################
####### MISC DATA ANALYSIS FOLLOW-UP#######
###########################################
joined_question <- pha %>% select(c(19:20)) %>% 
  rename(`Do you plan to use available waivers to not require receipt and verification of personal information` = `Do you plan to use available waivers to not require receipt and verification of personal information (such as Social Security Numbers, citizenship, disability status, date of birth) at entry (rather than within 90 or 180 days, as specified by the waiver)?`,
         `Do you plan to use any of the other available waiver opportunities?` = `Do you plan to use any of the other available waiver opportunities? - Selected Choice`) %>% 
  pivot_longer(cols = c(1:2), names_to = "question") 

joined_question[is.na(joined_question)] <- "NA"


joined_table <- data.frame(table(joined_question)) %>% 
  ggplot()+
  geom_bar(aes(x = question, y = Freq, fill = value),stat = "identity", )+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(x = NULL)

ggsave(joined_table, file=paste0("joined19_20.pdf"), width = 28, height = 14, units = "cm", device = cairo_pdf)

