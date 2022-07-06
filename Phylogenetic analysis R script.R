setwd("C:\\Users\\EUGENE\\Desktop\\R Projects")

# The raw file was downloaded 30th January 2021
metadata <- read_tsv("Metadata_hcov.tsv")

str(metadata)
names(metadata)
library(tidyverse)

# Selecting useful variables e.g Virus name, Collection date etc.
met <- select(metadata, 1:5,11,13,14 )

# Obtaining the complete cases
com <- complete.cases(met)
metadata_complete <- met[com,]

# Incomplete rows are rows 1,2,14:21,33
incomplete <- met[c(1,2,14:21,33),]

# Exporting the file with incomplete cases
write.csv(incomplete, "C:\\Users\\EUGENE\\Desktop\\R Projects\\metadata_incomplete.csv")

# Exporting the file of complete cases
write.csv(metadata_complete, file = "C:\\Users\\EUGENE\\Desktop\\R Projects\\completemetadata.csv")

# Recoding the "Specimen" variable for uniformity
# Some Ambiguities: The Environmental swab is not specific enough to know exactly
# what part of the environment the specimen was isolated from. Some additional information 
# under the 'Additional location information' is available, but not for all 
# cases, therefore the ambiguity. We cannot ascertain the precise environmental 
# location where some of the swabs were taken. "Environmental swab" was therefore
# recoded as "Swab". The recoding done is below.

metadata_complete$Specimen_recode <- recode(metadata_complete$Specimen, "Raw Sewage, PEG Precipitation"= "Sewage",
       "Enviromental swab"="Swab", "swab"="Swab", "Wastewater"="Sewage",
       "Breathing air using VIVAs Air sampler"="Air","Air sample"="Air",
       "Outer packaging of cold chain products"="Cold chain product swab",
       "Outer packaging of cold chain products isolated from Vero cells"=
               "Cold chain product swab")
# Recoding Locations to countries

recode(metadata_complete, "Asia / China / Beijing"="China", "North America / USA / Florida")

g <- ggplot(metadata_complete, aes(Specimen_recode))
g + geom_bar() + labs(x="Environmental Source", y="Frequency", title = "Sources of SARS-CoV-2 From the Environment")+ col=c("blue","magenta","pink","yellow","gold")


metacountry <- group_by(metadata_complete, Country)%>% summarise(count=n())

barplot(table(metadata_complete$Country), col=c("blue","magenta","pink","brown","gold")
)


data <- data.frame(
        name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
        val=sample(seq(1,10), 8 )
)
install.packages("cowplot")
install.packages("gridExtra")

?combine
#SARS-CoV-2 Sequences From Different Countries
#SARS-CoV-2 Environmental Sequences From Different Countries
# Obtaining the frequency count for each country

metacountry <- group_by(metadata_complete, Country)%>% summarise(count=n())
metadata_complete$Countrycount <- recode(metadata_complete$Country, "Austria"=65,"China"=14, "Brazil"=2, "Liechtenstein"=3,"USA"=6)

?colSums
perce

Percentcountry <- metadata_complete %>%
        group_by(Country) %>%
        summarise(count=n()) %>%
        mutate(Percent=count/sum(count)*100)
sum(Percentcountry$Percent)
(65/90)*100
6/90*100
percentclade <- metadata_complete %>%
        group_by(Clade) %>%
        summarise(count=n())%>%
        mutate(Percent=count/sum(count)*100)
percentlineage <- metadata_complete %>%
        group_by(Lineage) %>%
        summarise(count=n())%>%
        mutate(Percent=count/sum(count)*100)
percentlineage
percentspecimen <- metadata_complete %>%
        group_by(Specimen_recode) %>%
        summarise(count=n())%>%
        mutate(Percent=count/sum(count)*100)
percentspecimen

## Color Palette
pa <- colorRampPalette(c("blue", "red","green"))( 4 )
pa1 <- colorRamp(c("red", "green"))( (0:4)/4 )
d <- data.frame(co= sample(c("A","B","C","D"),100, replace = T))
d
barplot(table(d$co),col = pa)
