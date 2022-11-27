library(ggplot2)
#install.packages("webr")
library(webr)
library(dplyr)


pop <- read_csv("poppinuser (1).csv")
pop$birthdate <- as.Date(pop$birthdate)
str(pop)
attach(pop)

venmos <- c("Ella-Lee-37", "Libby-Curiel", "okthenjoss",   "jeremywoo22",  "rhiannacarl",  
            "Erik-Jonas17", "Shayans66", "xyzsage", "rozahad","varun-venna", "Enter here", 
            "Gee-Caitlin", "alinamho", "Dorfmanalex8", "sse173", "ashishe", "palaceucla","jaynoire")
which(venmo %in% venmos) # index
View(data.frame(pop[which(venmo %in% venmos), ])) # players who have their venmo posted

unique(teamTitle)

library(dplyr)
View(as.data.frame(bio[which(!is.na(bio))]))
pop_sub <- pop %>% dplyr::select(displayName, gender, birthdate, bio, showAge)

pop_sub <- pop_sub %>% drop_na()

no_bio_index <- which(pop_sub$bio == "(No bio set)")
blank_index1 <- which(pop_sub$bio == "\n")
blank_index2 <- which(pop_sub$bio == "\n\n")
blank_index3 <- which(pop_sub$bio == "\n\n\n")
blank_index4 <- which(pop_sub$bio == "\n\n\n\n")
blank_index5 <- which(pop_sub$bio == "\n\n\n\n\n")
blank_index6 <- which(pop_sub$bio == "\n\n\n\n\n\n")
blank_index8 <- which(pop_sub$bio == "\n\n\n\n\n\n\n\n")
blank_index9 <- which(pop_sub$bio == "\n\n\n\n\n\n\n\n\n")
blank_index10 <- which(pop_sub$bio == "\n\n\n\n\n\n  \n")
nobio_blank <- c(no_bio_index, 
                 blank_index1, blank_index2, blank_index3, blank_index4, 
                 blank_index5, blank_index6,blank_index8, blank_index9, blank_index10)
  
pop_yesbio <- pop_sub[-nobio_blank, ]
pop_nobio <- pop_sub[nobio_blank,]
nrow(pop_yesbio) / nrow(pop_sub) # 46%
nrow(pop_nobio) / nrow(pop_sub) # 54%

# calculate age
pop_yesbio <- pop_yesbio %>% 
  mutate(age = as.numeric(difftime(Sys.Date(), pop_yesbio$birthdate, units = "weeks"))/52.25)
View(pop_yesbio)
pop_yesbio_plt <- pop_yesbio
pop_yesbio_plt$bio <- "Yes"
pop_yesbio_plt <- pop_yesbio_plt %>% mutate(`Valid Age` = pop_yesbio_plt$age >= 18 & pop_yesbio_plt$age < 40)

pop_nobio <- pop_nobio %>% 
  mutate(age = as.numeric(difftime(Sys.Date(), pop_nobio$birthdate, units = "weeks"))/52.25)
pop_nobio$bio <- "No"
pop_nobio <- pop_nobio %>% mutate(`Valid Age` = pop_nobio$age >= 18 & pop_nobio$age < 40)


pop_nobio_validage <- pop_nobio[which(pop_nobio$age >= 18 & pop_nobio$age < 40), ]
nrow(pop_nobio_validage) / nrow(pop_nobio) 
# only around 43.6% user who left the bio blank have valid ages
nobio_showage <- nrow(pop_nobio_validage[which(pop_nobio_validage$showAge == TRUE), ])/nrow(pop_nobio_validage)
# and 70.7% of them are willing to show their ages.

pop_yesbio_validage <- pop_yesbio[which(18 <= pop_yesbio$age & pop_yesbio$age < 40), ]
nrow(pop_yesbio_validage)/ nrow(pop_yesbio) 
# For user who has content for their bios, 99.9% of them has valid ages
yeabio_showage <- nrow(pop_yesbio_validage[which(pop_yesbio_validage$showAge == TRUE), ])/nrow(pop_yesbio_validage)
# and 68.4% of them are willing to show their ages.


bio_age <- data.frame(Bio = c(pop_yesbio_plt$bio, pop_nobio$bio),
                     `Valid Age` = c(pop_yesbio_plt$`Valid Age`, pop_nobio$`Valid Age`),
                      user = c(pop_yesbio_plt$displayName, pop_nobio$displayName))

bio_showAge <- data.frame(Bio = c(pop_yesbio_plt$bio, pop_nobio$bio),
                          showAge = c(pop_yesbio_plt$showAge, pop_nobio$showAge),
                          user = c(pop_yesbio_plt$displayName, pop_nobio$displayName))

bio_age_plt <- bio_age %>% group_by(Bio, Valid.Age) %>% 
  summarise(n = length(user))

bio_showAge_plt <- bio_showAge %>% group_by(Bio, showAge) %>% 
  summarise(m = length(user))



bio_age_plt$Valid.Age[2] <- "Valid Age"
bio_age_plt$Valid.Age[4] <- "Valid Age"
bio_age_plt$Valid.Age[1] <- "Invalid Age"
bio_age_plt$Valid.Age[3] <- "Invalid Age"

bio_age_plt

PieDonut(bio_age_plt, aes(Bio, Valid.Age, count = n),
         #start=3*pi/2,
         explode = c(1,2),
         labelposition = 2.1,
         showRatioThreshold = 0.1,
         explodeDonut = T,
         title = "Bio & Valid Age")

PieDonut(bio_showAge_plt, aes(Bio, showAge, count = m),
         #start=3*pi/2,
         explode = c(1,2),
         labelposition = 2.1,
         showRatioThreshold = 0.1,
         explodeDonut = T,
         title = "Bio & Show Age")

# Suggestion to provide options for users to choose their mood, vibe, interest, what are they looking for;
# similar to the "Status" feature


party <- read_csv("party.csv")
objects(party)
attach(party)
party_sub <- party %>% 
  dplyr::select(address, attendeesCount, autoApproveGuests, 
                date, dateCreated, legalAgeOnly, visibility)

party_sub$date <- as.Date(party_sub$date)
party_sub$dateCreated <- as.Date(party_sub$dateCreated)
diff_days <- format(difftime(date, dateCreated, units = "days"), scientific = F, digits = 1)
str_extract(diff_days, "[:digit:][:digit:].")




