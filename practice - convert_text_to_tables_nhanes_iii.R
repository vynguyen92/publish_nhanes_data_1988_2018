string_variables <- "How long since doctor took blood press ...... HAE1 1597 Doctor ever told had hypertension/HBP ....... HAE2 1598 Told 2+ times you had hypertension/HBP ...... HAE3 1599 Doctor told take prescribed med for HBP ..... HAE4A 1600 Doctor told control/lose weight for HBP ..... HAE4B 1601 Doctor told cut salt intake for HBP ......... HAE4C 1602 Doctor told do anything else for HBP ........ HAE4D 1603 Doctor told exercise more for HBP ........... HAE4D1 1604 Doctor told restrict alcohol for HBP ........ HAE4D2 1605 Doctor told stop smoking for HBP ............ HAE4D3 1606 Doctor told reduce tension for HBP .......... HAE4D4 1607 Doctor told change diet for HBP ............. HAE4D5 1608 Doctor told make other changes for HBP ...... HAE4D6 1609 Now taking prescribed medicine for HBP ...... HAE5A 1610 Now controlling or losing weight for HBP .... HAE5B 1611 Now using less salt or sodium for HBP ....... HAE5C 1612 Now exercising for high blood pressure ...... HAE5D1 1613 Now restricting alcohol for HBP ............. HAE5D2 1614 Now quit smoking for high blood pressure .... HAE5D3 1615 Now reduced tension for HBP ................. HAE5D4 1616 Now made diet changes for HBP ............... HAE5D5 1617 Now made other changes for HBP .............. HAE5D6 1618 Ever had blood cholesterol checked .......... HAE6 1619 Doctor told blood cholesterol level high .... HAE7 1620 Doctor told eat less fat for HBC ............ HAE8A 1621 Doctor told lose weight for HBC ............. HAE8B 1622 Doctor told exercise for HBC ................ HAE8C 1623 Doctor told take medicine for HBC ........... HAE8D 1624 Now eat less high fat foods for HBC ......... HAE9A 1625 Now losing weight to lower cholesterol ...... HAE9B 1626 Now exercising to lower cholesterol ......... HAE9C 1627 Take prescribed med to lower cholesterol .... HAE9D 1628 Check item. Mark first applicable box ....... HAE10 1629 On own for HBC, eat fewer high fat foods .... HAE11A 1630 On own for HBC, controlled/lost weight ...... HAE11B 1631 On own for HBC, exercise more ............... HAE11C 1632"

delimit_digit <- unlist(strsplit(string_variables, "(?<=\\D)(?=\\d{4})|(?<=\\d{4})(?=\\D)", perl = TRUE))

delimit_periods <- unlist(strsplit(delimit_digit, "\\.\\.+", perl = FALSE))

trim_space_ends <- trimws(delimit_periods)

my_matrix <- matrix(trim_space_ends, ncol = 3, byrow = TRUE) %>%
  data.frame() %>%
  relocate(X2) %>%
  select("X2", "X1")

string_rh <- "Age/sex check item used in skip pattern ..... MAPF1 5133
Age when menstrual cycles started - yrs ..... MAPF2 5134-5135
Age category menstrual cycles started ....... MAPF3 5136
Have you ever been pregnant ................. MAPF4 5137
How many times have you been pregnant ....... MAPF5 5138-5139
Total number of live births ................. MAPF6 5140-5141
Age at first live birth recoded - years ..... MAPF7R 5142-5143
Age at the time of last live birth - yrs .... MAPF8 5144-5145
Did you breastfeed any of your children ..... MAPF9 5146
How many children did you breastfeed ........ MAPF10 5147-5148
Age check item used in skip pattern ......... MAPF11 5149
Are you now pregnant ........................ MAPF12 5150
Pregnancy status recode ..................... MAPF12R 5151
Month of current pregnancy .................. MAPF13 5152-5153
Have you been pregnant in the last 2 yrs .... MAPF14 5154
Months since last pregnancy ended ........... MAPF15 5155
Receive benefits from WIC in last 12 mos .... MAPF16 5156
Receiving benefits from WIC now ............. MAPF17 5157
How many months received WIC benefits ....... MAPF18S 5158-5160
F6 check item used in skip pattern .......... MAPF19 5161
Are you now breastfeeding a child ........... MAPF20 5162
Have you had a period in past 12 months ..... MAPF21 5163
How long ago was your last period ........... MAPF22 5164-5165
How old when had last period - years ........ MAPF23 5166-5168
Age category at last period ................. MAPF24 5169-5170
Have you had a hysterectomy ................. MAPF25 5171
How old when you had hysterectomy ........... MAPF26 5172-5174
Have you had one or both ovaries removed .... MAPF27 5175
Were both ovaries removed or only one ....... MAPF28 5176
How old when ovary removed - years .......... MAPF29 5177-5179
Ever taken birth control pills .............. MAPF30 5180
Age began taking birth control pills-yrs .... MAPF31 5181-5182
How many months ago stop taking BC pills .... MAPF32S 5183-5185
How many months took birth control pills .... MAPF33S 5186-5188
Brand of birth control pills code ........... MAPF34R 5189-5191
F30-F32 check item used in skip patterns .... MAPF34CK 5192
Ever have NORPLANT implant .................. MAPF34A 5193
Have NORPLANT implant now ................... MAPF34B 5194
How many months ago NORPLANT implanted ...... MAPF34CS 5195-5196
F-section check item for skip pattern ....... MAPF35 5197
Did periods stop due to radiation/chemo ..... MAPF36 5198
Ever take estrogen by mouth ................. MAPF37 5199
Age when first took estrogen pills - yrs .... MAPF38 5200-5202
How many months ago stop estrogen pills ..... MAPF39S 5203-5206
How many years took estrogen pills .......... MAPF40 5207-5208
Ever used estrogen cream, supp, inject ...... MAPF41 5209
Age first used estrogen crm,sup,inj -yrs .... MAPF42 5210-5212
Months since used estrogen crm,sup,inj ...... MAPF43S 5213-5216
Years used estrogen cream, supp, inject ..... MAPF44 5217-5218
Ever used female hormone patches ............ MAPF45 5219
Age when first used hormone patches -yrs .... MAPF46 5220-5222
Months since used hormone patches ........... MAPF47S 5223-5225
How many years used hormone patches ......... MAPF48 5226-5227
Age check item used in skip pattern ......... MAPF49 5228
Age at first sexual intercourse - years ..... MAPF50 5229-5230
How many different sex partners ever ........ MAPF51 5231-5234
Sex/F51 check item used in skip pattern ..... MAPF52 5235
Was this partner female or male ............. MAPF53 5236
How many partners have been female .......... MAPF54R 5237-5240
How many partners have been male ............ MAPF55 5241-5244
# of different sex partners in past year .... MAPF56 5245-5247 
Has doctor told you had genital herpes ...... MAPF57 5248"

delimit_enter <- unlist(strsplit(string_rh, "\\n", perl = TRUE))

delimit_periods <- unlist(strsplit(delimit_enter, "\\.\\.+", perl = FALSE))

delimit_space_5 <- unlist(strsplit(delimit_periods, "\\s5", perl = FALSE))

trim_space_ends <- trimws(delimit_space_5)

my_matrix <- matrix(trim_space_ends, ncol = 3, byrow = TRUE) %>%
  data.frame() %>%
  relocate(X2) %>%
  select("X2", "X1")
