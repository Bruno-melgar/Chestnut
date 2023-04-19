rm(list = ls())     # clear objects  
graphics.off() 
##########################
###### Pasta  ############
##########################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","plot3D", "cluster", "readxl", "magrittr",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","ggstatsplot")
inst(packages)
theme_set(theme_minimal())




# Dataframe ---------------------------------------------------------------
(Pasta <- read_excel("Pasta.xlsx", sheet = "df"))





# Data wrangling ----------------------------------------------------------
describe(Pasta)
Pasta[, 2:39] <- lapply(Pasta[, 2:39], factor)
s1 <- summary(Pasta)
write.table(s1, "s1.csv", sep = ",")

perc <- prop.table(table(Pasta[, 2:39])) * 100

## Filtering the selected group --------------------------------------------
Star_group <- Pasta %>% 
  select(ID:New_Pasta_Products_Frequency, New_Pasta_Products_Availability,
         Oat:High_Intensity) %>% 
  filter(Chestnut %in% c("Light_taste","Intense_taste"),
         Banana == "Dont_feel",
         Almond %in% c("Dont_feel","Light_taste"),
         Peanut %in% c("Dont_feel","Light_taste"),
         Oat %in% c("Dont_feel","Light_taste")) %>% 
  arrange(desc(Sex))

s2<-summary(Star_group)
write.table(s2, "s2.csv", sep = ",")

star <- Star_group[-3, c(1,14:16)]
star2<- star[,-1]
str(star2)
write.table(star2, "star2.csv", sep = ",")



# Calcular la tabla de recuento de valores únicos para cada nivel de intensidad
recuento_low <- c(sum(star2$Low_Intensity == "Cooked"),
                  sum(star2$Low_Intensity == "Roasted"),
                  sum(star2$Low_Intensity == "Dried"))

recuento_mid <- c(sum(star2$Mid_Intensity == "Cooked"),
                  sum(star2$Mid_Intensity == "Roasted"),
                  sum(star2$Mid_Intensity == "Dried"))

recuento_high <- c(sum(star2$High_Intensity == "Cooked"),
                   sum(star2$High_Intensity == "Roasted"),
                   sum(star2$High_Intensity == "Dried"))

# Combinar los resultados en una tabla
tabla_final <- rbind(recuento_low, recuento_mid, recuento_high)

# Rename rows and columns
rownames(tabla_final) <- c("Low", "Mid", "High")
colnames(tabla_final) <- c("Cooked", "Roasted", "Dried")

# Display final table
tabla_final
final <- as.data.frame(tabla_final)
final2 <- as.data.frame(t(tabla_final))

# Reordering table
x <- c()
for (row in rownames(final2)) {
  for (col in colnames(final2)) {
    x <- rbind(x, matrix(rep(c(row, col), final2[row, col]), ncol = 2, byrow = TRUE))
  }
}
df <- as.data.frame(x)
colnames(df) <- c("Chestnut Treatment Recieved", "Intensity Felt")
df

# Fisher's exact test with raw data
test <- fisher.test(table(df))

# Reshape dataframe --------------------
star2 <- rowid_to_column(star2) # agrega una columna de índice
star2_long <- pivot_longer(star2, cols = -rowid) # convierte las columnas a filas
star2_wide <- pivot_wider(star2_long, names_from = value, values_from = name, values_fn = list) # convierte las filas a columnas
star2_wide <- star2_wide %>% unnest(everything()) # convierte las columnas de listas en columnas individuales
star2_wide <- select(star2_wide, -rowid) # elimina la columna de índice
## Re-coding values
star_new <- star2_wide %>%
  mutate_all(recode, "Low_Intensity" = 0, "Mid_Intensity" = 0, "High_Intensity" = 1)

# Post hoc test
test_2 <- wilcox.test(star_new$Cooked,star_new$Dried, paired = TRUE, alternative = c("two.sided"))
test_2.1 <- wilcox.test(star_new$Cooked,star_new$Roasted, paired = TRUE, alternative = c("two.sided"))
test_2.2 <- wilcox.test(star_new$Roasted,star_new$Dried, paired = TRUE, alternative = c("two.sided"))


# combine plot and statistical test with ggbarstats
#library(ggstatsplot)
ggbarstats(
  df, "Intensity Felt", "Chestnut Treatment Recieved",
  results.subtitle = FALSE,
  title = "Chestnut Flavour Intensity Felt by Selected Panelist",
  package = "RColorBrewer",
  palette = "OrRd",
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3)), 
    ", *Post hoc Wilcox test", ", p-value = ",
    ifelse(test_2$p.value < 0.001, "< 0.001", round(test_2$p.value, 3))
  )
) +
  theme(axis.title.x = element_text(size = 12))




# Test de aceptación --------------------
subset1 <- Pasta[, 19:25]
Treatment <- rep("Cooked", nrow(subset1))
subset1 <- cbind(Treatment, subset1)
names(subset1) <- gsub("Cocked_", "", names(subset1))
subset2 <- Pasta[, 26:32]
Treatment <- rep("Dried", nrow(subset2))
subset2 <- cbind(Treatment, subset2)
names(subset2) <- gsub("Dried_", "", names(subset2))
subset3 <- Pasta[, 33:39]
Treatment <- rep("Roasted", nrow(subset3))
subset3 <- cbind(Treatment, subset3)
names(subset3) <- gsub("Roasted_", "", names(subset3))

subset4 <- rbind(subset1, rbind(subset2, subset3))
org <- subset4[,1:6]

# rownames
rownames <- org[,1]

## Re-coding values
dat <- org %>%
  select(Visual:Sweetness) %>% 
  mutate_all(recode, "I liked it extremely" = 9,"I really liked" = 8, "I liked moderately" = 7,
             "I liked it slightly" = 6,"Indifferent" = 5, "Disadvantaged" = 4,
             "Moderately" = 3,"I disliked a lot" = 2,"I disliked extremely" = 1)

#Whole dataframe recoded
data <- cbind(rownames,dat)

# subsetting
cooked <- data[1:61,]
dried <- data[62:122,]
roasted <- data[123:183,]

# Histogramas
########################
par(mfrow = c(2,3))
hist(data$Visual, breaks = 10)
hist(data$Flavour, breaks = 10)
hist(data$Aroma, breaks = 10)
hist(data$Texture, breaks = 10)
hist(data$Sweetness, breaks = 10)

par(mfrow = c(2,3))
hist(cooked$Visual, breaks = 10)
hist(cooked$Flavour, breaks = 10)
hist(cooked$Aroma, breaks = 10)
hist(cooked$Texture, breaks = 10)
hist(cooked$Sweetness, breaks = 10)

par(mfrow = c(2,3))
hist(dried$Visual, breaks = 10)
hist(dried$Flavour, breaks = 10)
hist(dried$Aroma, breaks = 10)
hist(dried$Texture, breaks = 10)
hist(dried$Sweetness, breaks = 10)

par(mfrow = c(2,3))
hist(roasted$Visual, breaks = 10)
hist(roasted$Flavour, breaks = 10)
hist(roasted$Aroma, breaks = 10)
hist(roasted$Texture, breaks = 10)
hist(roasted$Sweetness, breaks = 10)
########################