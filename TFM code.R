# MASTER THESIS

############################## Packages ########################################
library(easypackages)
libraries(c("readr", "readxl", "tidyverse", "ggplot2", "fda", "fda.usc", "demography", "roahd", 
            "MASS", "dplyr", "tidyr", "stringr", "mapSpain", "sf", "viridis", "scales", "ftsa", 
            "forecast", "purrr", "cluster", "ganimate", "gifski", "transformr","patchwork",
            "gridExtra","fdaoutlier","funFEM","fdaoutlier","ggrepel"))

############### Evolution of total population of the province ##################

le <- read_excel("C:/Users/Usuario/Downloads/2852.xlsx")

data <- le %>%
  pivot_longer(cols = everything(),
               names_to = "year",
               values_to = "population") %>%
  mutate(
    year = as.numeric(year),      
    population = as.numeric(population)
  ) %>%
  arrange(year) 

all_years <- 1996:2021

# COmplete 1997
data <- data %>%
  complete(year = all_years) %>%
  arrange(year)

# 1997 as a mean between 1996 and 1998
if (is.na(data$population[data$year == 1997])) {
  val96 <- data$population[data$year == 1996]
  val98 <- data$population[data$year == 1998]
  data$population[data$year == 1997] <- mean(c(val96, val98), na.rm = TRUE)
}

ggplot(data, aes(x = year, y = population)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_point(color = "#E74C3C", size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Evolution of population of the province (1996–2021)",
    x = "Year",
    y = "Population"
  )

############################## Datasets ########################################


# Total population 2003-2024 by municip #
#
# Total men and women every year until 2022
tot1 <- read.csv2("C:/Users/Usuario/Desktop/TFM/Database/total prov 03-21 sin totales 2 csv.csv",
                  skip = 0)
#
datostot1 <- tot1 %>%
  mutate(across(-c(municipio, year), ~ as.numeric(.))) %>%
  mutate(
    municipio = municipio %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_remove("^\\d{5} "))
#
# Long format
datos_long_tot1 <- datostot1 %>%
  pivot_longer(
    cols = -(1:2),  
    names_to = "Edad",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = parse_number(Edad),
    Personas = as.numeric(Personas),
    year=as.numeric(year)
  )
#
# Add 2023 and 2024

df_raw <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/munic leon  21-24 año a año y sexo.xlsx", 
                     sheet = "TOTAL", skip = 1)
#
names(df_raw)[1] <- "municipio"
#
# Adapt the dataset to work with it
# 
df_long <- df_raw %>%
  pivot_longer(
    cols = -municipio,
    names_to = c("Edad", "year"),
    names_pattern = "(\\d+\\s?años?)\\s(\\d{4})",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = str_extract(Edad, "\\d+"), 
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(municipio, year, Edad)
#
# Name of municipalities
municipios <- unique(df_long$municipio)
#
# Patterns per municipality (408 rows)
Edad_seq <- rep(0:101, each = 4)       # 102 ages * 4 years = 408
year_seq <- rep(c(2024, 2023, 2022, 2021), times = 102)
#
# Apply to dataframe
df_long <- df_long %>%
  group_by(municipio) %>%
  mutate(
    year = rep(c(2024, 2023, 2022, 2021), length.out = n()), 
    Edad = rep(0:(n() %/% 4), each = 4, length.out = n())     
  ) %>%
  ungroup()
#
df_long <- df_long %>%
  separate(municipio, into = c("codigo_municipio", "municipio"), sep = " ", extra = "merge")
#
# 
#
df_long <- df_long %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(Edad = as.numeric(Edad)) 
#

df_long <- df_long %>%
  arrange(desc(year)) %>%
  arrange(municipio)
#
df_long_nolab <- df_long %>%
  dplyr::select(-codigo_municipio)
#
# Combine
datos_combinados <- bind_rows(datos_long_tot1, df_long_nolab)
#
datos_combinados <- datos_combinados %>%
  arrange(municipio, desc(year), Edad)
#
datos_combinados <- na.omit(datos_combinados)
#
###### Men 2003-2024 by municip #
#
# Men 2003-22
hom1 <- read.csv2("C:/Users/Usuario/Desktop/TFM/Database/hombres 03-21 sin totales 2 csv.csv",
                  skip = 0)
#
datoshom1 <- hom1 %>%
  mutate(across(-c(municipio, year), ~ as.numeric(.))) %>%
  mutate(
    municipio = municipio %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_remove("^\\d{5} "))
#

datos_long_hom1 <- datoshom1 %>%
  pivot_longer(
    cols = -(1:2), 
    names_to = "Edad",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = parse_number(Edad),
    Personas = as.numeric(Personas),
    year=as.numeric(year)
  )
 #
# Add 2023 and 2024
#
df_rawh <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/munic leon  21-24 año a año y sexo.xlsx", 
                            sheet = "HOMBRES", skip = 1)
#
names(df_rawh)[1] <- "municipio"
#
df_longh <- df_rawh %>%
  pivot_longer(
    cols = -municipio,
    names_to = c("Edad", "year"),
    names_pattern = "(\\d+\\s?años?)\\s(\\d{4})",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = str_extract(Edad, "\\d+"), 
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(municipio, year, Edad)
#
municipios <- unique(df_longh$municipio)
#
Edad_seqh <- rep(0:101, each = 4)   
year_seqh<- rep(c(2024, 2023, 2022, 2021), times = 102)
#
df_longh <- df_longh %>%
  group_by(municipio) %>%
  mutate(
    year = rep(c(2024, 2023, 2022, 2021), length.out = n()),  # ciclo de años
    Edad = rep(0:(n() %/% 4), each = 4, length.out = n())     # edad ajustada
  ) %>%
  ungroup()
#
df_longh <- df_longh %>%
  separate(municipio, into = c("codigo_municipio", "municipio"), sep = " ", extra = "merge")
#
df_longh <- df_longh %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(Edad = as.numeric(Edad)) 
#
df_longh <- df_longh %>%
  arrange(desc(year)) %>%
  arrange(municipio)
#
df_long_nolabh <- df_longh %>%
  dplyr::select(-codigo_municipio)
#
datos_combinadosh <- bind_rows(datos_long_hom1, df_long_nolabh)
#
datos_combinadosh <- datos_combinadosh %>%
  arrange(municipio, desc(year), Edad)
#
datos_combinadosh <- na.omit(datos_combinadosh)
#
##### Women 2003-2024 by municip #
#
muj1 <- read.csv2("C:/Users/Usuario/Desktop/TFM/Database/mujeres 03-21 sin totales 2 csv.csv",
                  skip = 0)
#
datosmuj1 <- muj1 %>%
  mutate(across(-c(municipio, year), ~ as.numeric(.))) %>%
  mutate(
    municipio = municipio %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_remove("^\\d{5} "))
#
datos_long_muj1 <- datosmuj1 %>%
  pivot_longer(
    cols = -(1:2),
    names_to = "Edad",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = parse_number(Edad),
    Personas = as.numeric(Personas),
    year=as.numeric(year)
  )
#
df_rawm <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/munic leon  21-24 año a año y sexo.xlsx", 
                      sheet = "MUJERES", skip = 1)
#
names(df_rawm)[1] <- "municipio"
#
df_longm <- df_rawm %>%
  pivot_longer(
    cols = -municipio,
    names_to = c("Edad", "year"),
    names_pattern = "(\\d+\\s?años?)\\s(\\d{4})",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = str_extract(Edad, "\\d+"),
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(municipio, year, Edad)
#
municipios <- unique(df_longm$municipio)
#
Edad_seqm <- rep(0:101, each = 4)    
year_seqm<- rep(c(2024, 2023, 2022, 2021), times = 102)
#
df_longm <- df_longm %>%
  group_by(municipio) %>%
  mutate(
    year = rep(c(2024, 2023, 2022, 2021), length.out = n()),
    Edad = rep(0:(n() %/% 4), each = 4, length.out = n())     
  ) %>%
  ungroup()
#
df_longm <- df_longm %>%
  separate(municipio, into = c("codigo_municipio", "municipio"), sep = " ", extra = "merge")
#

df_longm <- df_longm %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(Edad = as.numeric(Edad)) 
#
df_longm <- df_longm %>%
  arrange(desc(year)) %>%
  arrange(municipio)
#
df_long_nolabm <- df_longm %>%
  dplyr::select(-codigo_municipio)
#
datos_combinadosm <- bind_rows(datos_long_muj1, df_long_nolabm)
#
datos_combinadosm <- datos_combinadosm %>%
  arrange(municipio, desc(year), Edad)
#
datos_combinadosm <- na.omit(datos_combinadosm)
#
#
########## COMBINE THE THREE OF THEM #############
#
datos_combinadosm <- datos_combinadosm %>%
  mutate(Sex = "Female")
#
datos_combinadosh <- datos_combinadosh %>%
  mutate(Sex = "Male")
#
datos_combinados <- datos_combinados %>%
  mutate(Sex = "Total")
#
age.sex.munic.year <- bind_rows(datos_combinados, datos_combinadosh, datos_combinadosm)
#
#
##### Total province without municip ##
#
tot_prov_sin_munic <- read.csv2("C:/Users/Usuario/Desktop/TFM/Database/evol total prov por edad (año a año) csv.csv",
                                skip = 0)
#
datossinmunic <- tot_prov_sin_munic %>%
  mutate(across(-c(group, year), ~ as.numeric(.))) 
#
# Transform
datos_long_sinmunic <- datossinmunic %>%
  pivot_longer(
    cols = -(1:2),  
    names_to = "Edad",
    values_to = "Personas"
  ) %>%
  mutate(
    Edad = parse_number(Edad),
    Personas = as.numeric(Personas),
    year=as.numeric(year)
  )
#
###### Men ##
#
# Add 23-24
#
tot_prov_23_24h <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/tot prov 23-24.xlsx", sheet = "HOMBRES",col_names = FALSE)
#
df_long_th <- as.data.frame(t(tot_prov_23_24h)) %>% 
  rename(Edad = V1, year = V2, Personas = V3) %>% 
  mutate(
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(year) 
#
datos_long_sinmunic_th <- datos_long_sinmunic %>%
  filter(group=="Men")
#
#
df_long_th <- df_long_th[, c("year", "Edad", "Personas")]
#
# Combine
datos_combinados_th <- bind_rows(datos_long_sinmunic_th, df_long_th)
#
# Order
datos_combinados_th <- datos_combinados_th %>%
  arrange(desc(year), Edad) %>%
  dplyr::select(-group)
#
#
###### Women ###
#
tot_prov_23_24m <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/tot prov 23-24.xlsx", sheet = "MUJERES",col_names = FALSE)
#
df_long_tm <- as.data.frame(t(tot_prov_23_24m)) %>% 
  rename(Edad = V1, year = V2, Personas = V3) %>% 
  mutate(
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(year)
#
df_long_tm <- df_long_tm[, c("year", "Edad", "Personas")]
#
datos_long_sinmunic_tm <- datos_long_sinmunic %>%
  filter(group=="Women")
#
# Combine
datos_combinados_tm <- bind_rows(datos_long_sinmunic_tm, df_long_tm)
#
# Order
datos_combinados_tm <- datos_combinados_tm %>%
  dplyr::select(-group) %>%
  arrange(year)
#
#
##### Total of total pop ###
#
#
tot_prov_23_24t <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/tot prov 23-24.xlsx", sheet = "TOTAL",col_names = FALSE)
#
df_long_tt <- as.data.frame(t(tot_prov_23_24t)) %>% 
  rename(Edad = V1, year = V2, Personas = V3) %>% 
  mutate(
    Edad = as.numeric(Edad),
    year = as.numeric(year),
    Personas = as.numeric(Personas)
  ) %>%
  arrange(year)
#
df_long_tt <- df_long_tt[, c("year", "Edad", "Personas")]
#
rownames(df_long_tt) <- NULL
#
datos_long_sinmunic_tt <- datos_long_sinmunic %>%
  filter(group=="Total")
#
datos_combinados_tt <- bind_rows(datos_long_sinmunic_tt, df_long_tt)
#
datos_combinados_tt <- datos_combinados_tt %>%
  dplyr::select(-group) %>%
  arrange(year)
#
########## Create a dataset with all sexs ############
#
datos_combinados_tm <- datos_combinados_tm %>%
  mutate(Sex = "Female")
#
datos_combinados_th <- datos_combinados_th %>%
  mutate(Sex = "Male")
#
datos_combinados_tt <- datos_combinados_tt %>%
  mutate(Sex = "Total")
#
age.sex.prov.year <- bind_rows(datos_combinados_tt, datos_combinados_th, datos_combinados_tm)
#
#
##### Pop for selecting groups and groups ##########
#
# Total pop per municipality until 2024
data.hist2 <- read_excel("C:/Users/Usuario/Desktop/TFM/Database/total pobl municipio año a año 96-24 .xlsx")
#
# Reorder
data.hist2 <- data.hist2 %>%
  mutate(
    municipio = Municipios %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_remove("^\\d{5} ")) %>%
  dplyr::select(-Municipios)
colnames(data.hist2)[2] <- "Todas.las.edades"
colnames(data.hist2)[1] <- "year"
#
# Select population of last year 2024
data.hist2024 <- data.hist2 %>%
  filter(year == 2024) 
#
# Select names of munciipalities
mun.names <- data.hist2024 %>%
  dplyr::select(municipio)
#
data.hist2024$Todas.las.edades <- as.numeric(data.hist2024$Todas.las.edades)
#
# Create a new variable that are groups
data.hist2024 <- data.hist2024 %>%
  mutate(Todas.las.edades = as.numeric(Todas.las.edades)) %>%
  mutate(rango_poblacion = cut(`Todas.las.edades`,
                               breaks = c(0, 300, 1000, 5000, 15000, 150000),
                               labels = c("[0,300]", "[301,1000]", "[1001,5000]", "[5001,15000]", "[15001,150000]"),  include.lowest = TRUE))
#
# Now create a dataset with each group to work better with them
#
mun.gr1 <- data.hist2024 %>%
  filter(Todas.las.edades<301) %>%
  dplyr::select(municipio, Todas.las.edades,rango_poblacion)
mun.gr2 <- data.hist2024%>%
  filter(Todas.las.edades>300) %>%
  filter(Todas.las.edades<1001) %>%
  dplyr::select(municipio, Todas.las.edades,rango_poblacion)
mun.gr3 <- data.hist2024 %>%
  filter(Todas.las.edades>1000)%>%
  filter(Todas.las.edades <5001)%>%
  dplyr::select(municipio, Todas.las.edades,rango_poblacion)
mun.gr4 <- data.hist2024%>%
  filter(Todas.las.edades>5000) %>%
  filter(Todas.las.edades<15001) %>%
  dplyr::select(municipio, Todas.las.edades,rango_poblacion)
mun.gr5 <- data.hist2024%>%
  filter(Todas.las.edades>15000)%>%
  dplyr::select(municipio, Todas.las.edades,rango_poblacion)
# 
# Otras ctcas
#
mean(mun.gr1$Todas.las.edades, na.rm = TRUE)
median(mun.gr1$Todas.las.edades, na.rm = TRUE)
nrow(mun.gr1)
sd(mun.gr1$Todas.las.edades, na.rm = TRUE)
mean(mun.gr2$Todas.las.edades, na.rm = TRUE)
median(mun.gr2$Todas.las.edades, na.rm = TRUE)
nrow(mun.gr2)
sd(mun.gr2$Todas.las.edades, na.rm = TRUE)
mean(mun.gr3$Todas.las.edades, na.rm = TRUE)
median(mun.gr3$Todas.las.edades, na.rm = TRUE)
nrow(mun.gr3)
sd(mun.gr3$Todas.las.edades, na.rm = TRUE)
mean(mun.gr4$Todas.las.edades, na.rm = TRUE)
median(mun.gr4$Todas.las.edades, na.rm = TRUE)
nrow(mun.gr4)
sd(mun.gr4$Todas.las.edades, na.rm = TRUE)
mean(mun.gr5$Todas.las.edades, na.rm = TRUE)
median(mun.gr5$Todas.las.edades, na.rm = TRUE)
nrow(mun.gr5)
sd(mun.gr5$Todas.las.edades, na.rm = TRUE)
#
mun.gr1$group <- "G1"
mun.gr2$group <- "G2"
mun.gr3$group <- "G3"
mun.gr4$group <- "G4"
mun.gr5$group <- "G5"
#
mun.gr.todos <- bind_rows(mun.gr1, mun.gr2, mun.gr3, mun.gr4, mun.gr5)
#
#Select the info by age and year (total, male and female)
#
# Group 1
#
# This group requires to group the ages because there are few people
#
tot_gr1 <- datos_combinados %>%
  filter(municipio %in% mun.gr1$municipio)
#
men_gr1 <- datos_combinadosh %>%
  filter(municipio %in% mun.gr1$municipio)
#
wom_gr1 <- datos_combinadosm %>%
  filter(municipio %in% mun.gr1$municipio)
#
# Group age in 10 year
#
tot_gr1_agrupado <- tot_gr1 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 10, by = 10),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop")  %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 5 )
#
men_gr1_agrupado <- men_gr1 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 10, by = 10),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 5)
#
wom_gr1_agrupado <- wom_gr1 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 10, by = 10),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 5 )
#
# Group 2
#
tot_gr2 <- datos_combinados %>%
  filter(municipio %in% mun.gr2$municipio)
#
men_gr2 <- datos_combinadosh %>%
  filter(municipio %in% mun.gr2$municipio)
#
wom_gr2 <- datos_combinadosm %>%
  filter(municipio %in% mun.gr2$municipio)
#
tot_gr2_agrupado <- tot_gr2 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 5, by = 5),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop")  %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 2.5
  )
#
men_gr2_agrupado <- men_gr2 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 5, by = 5),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop")  %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 2.5
  )
#
wom_gr2_agrupado <- wom_gr2 %>%
  mutate(grupo_edad = cut(Edad,
                          breaks = seq(0, max(Edad, na.rm = TRUE) + 5, by = 5),
                          right = FALSE,
                          include.lowest = TRUE)) %>%
  group_by(municipio, year, grupo_edad) %>%
  summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Edad_central = as.numeric(str_extract(grupo_edad, "(?<=\\[)\\d+")) + 2.5
  )
#
#Group 3
#
tot_gr3 <- datos_combinados %>%
  filter(municipio %in% mun.gr3$municipio)
#
men_gr3 <- datos_combinadosh %>%
  filter(municipio %in% mun.gr3$municipio)
#
wom_gr3 <- datos_combinadosm %>%
  filter(municipio %in% mun.gr3$municipio)
#
# No grouping because they are big
#
# Group 4
#
tot_gr4 <- datos_combinados %>%
  filter(municipio %in% mun.gr4$municipio)
#
men_gr4 <- datos_combinadosh %>%
  filter(municipio %in% mun.gr4$municipio)
#
wom_gr4 <- datos_combinadosm %>%
  filter(municipio %in% mun.gr4$municipio)
#
# Group 5
#
tot_gr5 <- datos_combinados %>%
  filter(municipio %in% mun.gr5$municipio)
#
men_gr5 <- datos_combinadosh %>%
  filter(municipio %in% mun.gr5$municipio)
#
wom_gr5 <- datos_combinadosm %>%
  filter(municipio %in% mun.gr5$municipio)
#
# Normalized popuation curves
pob.norm <- age.sex.munic.year %>%
  group_by(municipio, Sex, year) %>%  
  mutate(
    moda_valor = Personas[which.max(Personas)],
    normalized = Personas / moda_valor
  ) %>%
  ungroup()
#
################################ Smooth ########################################
#
######### Group 1 ##########
#
# Obtain unique ages and range for B-spline
edad_fina_grp1 <- sort(unique(tot_gr1_agrupado$Edad_central))
age_range_grp1 <- range(edad_fina_grp1)
#
# Create B-spline
n_basis1 <- 6
basis1 <- create.bspline.basis(rangeval = age_range_grp1, nbasis = n_basis1)
#
lambda1 <- 1e3  # smooth
#
# Iterate per year
for (año in unique(tot_gr1_agrupado$year)) {
  datos_año1 <- tot_gr1_agrupado %>%
    filter(year == año) %>%
    filter(!is.na(Edad_central) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad_central) %>%
    group_split()
#
# Smooth each municipality
suavizados1 <- lapply(datos_año1, function(df) {
    x <- df$Edad_central
    y <- df$Personas
    fdParobj <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))  
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad_central = edad_fina_grp1,
      Personas = eval.fd(edad_fina_grp1, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  suavizados_df1 <- bind_rows(suavizados1)
# Plot
p_smoothw1 <- suavizados_df1 %>%
    ggplot(aes(x = Edad_central, y = Personas, group = municipio, color = municipio)) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Smoothed total population by age and municipality group 1 in", año),
      x = "Age (center of group)", y = "Number of people"
    ) +
    theme(legend.position = "none")
  
  #print(p_smoothw1)
}
#
######### Group 2 ##########
#
edad_fina_grp2 <- sort(unique(tot_gr2_agrupado$Edad_central))
age_range_grp2 <- range(edad_fina_grp2)
#
n_basis2 <- 11
basis2 <- create.bspline.basis(rangeval = age_range_grp2, nbasis = n_basis2)
#
lambda2 <- 1500  
#
for (año in unique(tot_gr2_agrupado$year)) {
  datos_año2 <- tot_gr2_agrupado %>%
    filter(year == año) %>%
    filter(!is.na(Edad_central) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad_central) %>%
    group_split()
  
suavizados2 <- lapply(datos_año2, function(df) {
    x <- df$Edad_central
    y <- df$Personas
    fdParobj <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad_central = edad_fina_grp2,
      Personas = eval.fd(edad_fina_grp2, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  suavizados_df2 <- bind_rows(suavizados2)

p_smoothw2 <- suavizados_df2 %>%
    ggplot(aes(x = Edad_central, y = Personas, group = municipio, color = municipio)) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Smoothed total population by age and municipality group 2 in", año),
      x = "Age (center of group)", y = "Number of people"
    ) +
    theme(legend.position = "none")
  
  #print(p_smoothw2)
}
#
######### Group 3 ############
#
edad_fina_grp3 <- sort(unique(tot_gr3$Edad))
age_range_grp3 <- range(edad_fina_grp3)
#
n_basis3 <- 19
basis3 <- create.bspline.basis(rangeval = age_range_grp3, nbasis = n_basis3)
#
lambda3 <- 1e-3 
#
for (año in unique(tot_gr3$year)) {
  datos_año3 <- tot_gr3 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()
#
suavizados3 <- lapply(datos_año3, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))  
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp3,
      Personas = eval.fd(edad_fina_grp3, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  suavizados_df3 <- bind_rows(suavizados3)

p_smoothw3 <- suavizados_df3 %>%
    ggplot(aes(x = Edad, y = Personas, group = municipio, color = municipio)) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Smoothed total population by age and municipality group 3 in", año),
      x = "Age (center of group)", y = "Number of people"
    ) +
    theme(legend.position = "none")
  
  #print(p_smoothw3)
}
#
suavizados_todos3 <- list()

for (año in unique(tot_gr3$year)) {
  datos_año3 <- tot_gr3 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()
  
  suavizados3 <- lapply(datos_año3, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp3,
      Personas = eval.fd(edad_fina_grp3, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  
  suavizados_todos3[[as.character(año)]] <- bind_rows(suavizados3)
}

# Combine
suavizados_final3 <- bind_rows(suavizados_todos3)
suavizados_df3 <- suavizados_final3 %>%
  filter(year==2024)
#
######## Group 4 ########
#
edad_fina_grp4 <- sort(unique(tot_gr4$Edad))
age_range_grp4 <- range(edad_fina_grp4)
#
n_basis4 <- 14
basis4 <- create.bspline.basis(rangeval = age_range_grp4, nbasis = n_basis4)
#
lambda4 <- 1e-3 
#
for (año in unique(tot_gr4$year)) {
  datos_año4 <- tot_gr4 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()
#
  suavizados4 <- lapply(datos_año4, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))  
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp4,
      Personas = eval.fd(edad_fina_grp4, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  suavizados_df4 <- bind_rows(suavizados4)

p_smoothw4 <- suavizados_df4 %>%
    ggplot(aes(x = Edad, y = Personas, group = municipio, color = municipio)) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Smoothed total population by age and municipality group 4 in", año),
      x = "Age (center of group)", y = "Number of people"
    ) +
    theme(legend.position = "none")
  
  #print(p_smoothw4)
}
#
suavizados_todos4 <- list()

for (año in unique(tot_gr4$year)) {
  datos_año5 <- tot_gr4 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()
  
  suavizados4 <- lapply(datos_año4, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp4,
      Personas = eval.fd(edad_fina_grp4, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  

  suavizados_todos4[[as.character(año)]] <- bind_rows(suavizados4)
}


suavizados_final4 <- bind_rows(suavizados_todos4)
suavizados_df4 <- suavizados_final4 %>%
  filter(year==2024)
#
######### Group 5 ########
#
edad_fina_grp5 <- sort(unique(tot_gr5$Edad))
age_range_grp5 <- range(edad_fina_grp5)
#
n_basis5 <- 16
basis5 <- create.bspline.basis(rangeval = age_range_grp5, nbasis = n_basis5)
#
lambda5 <- 1e-3  
#
for (año in unique(tot_gr5$year)) {
  datos_año5 <- tot_gr5 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()


suavizados5 <- lapply(datos_año5, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))  
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp5,
      Personas = eval.fd(edad_fina_grp5, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  suavizados_df5 <- bind_rows(suavizados5)
  
p_smoothw5 <- suavizados_df5 %>%
    ggplot(aes(x = Edad, y = Personas, group = municipio, color = municipio)) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Smoothed total population by age and municipality group 5 in", año),
      x = "Age (center of group)", y = "Number of people"
    ) +
    theme(legend.position = "none")
  
  #print(p_smoothw5)
}
#
suavizados_todos5 <- list()

for (año in unique(tot_gr5$year)) {
  datos_año5 <- tot_gr5 %>%
    filter(year == año) %>%
    filter(!is.na(Edad) & !is.na(Personas)) %>%
    group_by(municipio) %>%
    arrange(Edad) %>%
    group_split()
  
  suavizados5 <- lapply(datos_año5, function(df) {
    x <- df$Edad
    y <- df$Personas
    fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)
    smooth <- tryCatch({
      smooth.basis(x, y, fdParobj)
    }, error = function(e) return(NULL))
    
    if (is.null(smooth)) return(NULL)
    
    data.frame(
      Edad = edad_fina_grp5,
      Personas = eval.fd(edad_fina_grp5, smooth$fd),
      municipio = unique(df$municipio),
      year = año
    )
  })
  
  suavizados_todos5[[as.character(año)]] <- bind_rows(suavizados5)
}

suavizados_final5 <- bind_rows(suavizados_todos5)
suavizados_df5 <- suavizados_final5 %>%
  filter(year==2024)
#
########################### Deepest curve per group ############################
#
###### Grupo 1 ########
#
# Filter for the year
df_smooth_1 <- suavizados_df1 %>%
  arrange(municipio, Edad_central)
#
# Create a matrix
mat_curvas1 <- df_smooth_1 %>%
  tidyr::pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  dplyr::select(-municipio, -year) %>%
  as.matrix()
#
# Create fdata object
edades1 <- sort(unique(df_smooth_1$Edad_central))
fd_obj_1 <- fdata(mat_curvas1, argvals = edades1)
#
depths1 <- depth.mode(fd_obj_1)
median_index_1 <- which.max(depths1$dep)  
curva_median_1 <- fd_obj_1[median_index_1]
#
plot(fd_obj_1, col = alpha("gray", 0.4), lwd = 1,
     main = paste("Depth group 1"),
     xlab = "Central age", ylab = "People")
lines(curva_median_1, col = "red", lwd = 2)
legend("topright", legend = "Functional median (depth)", col = "red", lwd = 2)
#
# Vector of municipalities in same order as curves
municipios_orden_1 <- df_smooth_1 %>%
  tidyr::pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  pull(municipio)
#
# Municipality for the median curve (deepest)
municipio_median_1 <- municipios_orden_1[median_index_1]
municipio_median_1
#
######## Grupo 2 #########
#
df_smooth_2 <- suavizados_df2 %>%
  arrange(municipio, Edad_central)
#
mat_curvas2 <- df_smooth_2 %>%
  tidyr::pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  dplyr::select(-municipio, -year) %>%
  as.matrix()
#
edades2 <- sort(unique(df_smooth_2$Edad_central))
fd_obj_2 <- fdata(mat_curvas2, argvals = edades2)
#
depths2 <- depth.mode(fd_obj_2)
median_index_2 <- which.max(depths2$dep)  
curva_median_2 <- fd_obj_2[median_index_2]
#
plot(fd_obj_2, col = alpha("gray", 0.4), lwd = 1,
     main = paste("Depth group 2"),
     xlab = "Central age", ylab = "People")
lines(curva_median_2, col = "red", lwd = 2)
legend("topright", legend = "Functional median (depth)", col = "red", lwd = 2)
#
municipios_orden_2 <- df_smooth_2 %>%
  tidyr::pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  pull(municipio)
#
municipio_median_2 <- municipios_orden_2[median_index_2]
municipio_median_2
#
########## Grupo 3 ###########
#
df_smooth_3 <- suavizados_df3 %>%
  arrange(municipio, Edad)
#
mat_curvas3 <- df_smooth_3 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  dplyr::select(-municipio, -year) %>%
  as.matrix()
#
edades3 <- sort(unique(df_smooth_3$Edad))
fd_obj_3 <- fdata(mat_curvas3, argvals = edades3)
#
depths3 <- depth.mode(fd_obj_3)
median_index_3 <- which.max(depths3$dep)
curva_median_3 <- fd_obj_3[median_index_3]
#
plot(fd_obj_3, col = alpha("gray", 0.3), lwd = 1,
     main = paste("Depth 3"),
     xlab = "Age", ylab = "People")
lines(curva_median_3, col = "red", lwd = 2)
legend("topright", legend = "Functional median (depth)", col = "red", lwd = 2)
#
municipios_orden_3 <- df_smooth_3 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  pull(municipio)
#
municipio_median_3 <- municipios_orden_3[median_index_3]
municipio_median_3
#
######## Grupo 4 ##########
#
df_smooth_4 <- suavizados_df4 %>%
  arrange(municipio, Edad)
#
mat_curvas4 <- df_smooth_4 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  dplyr::select(-municipio, -year) %>%
  as.matrix()
#
edades4 <- sort(unique(df_smooth_4$Edad))
fd_obj_4 <- fdata(mat_curvas4, argvals = edades4)
#
depths4 <- depth.mode(fd_obj_4)
median_index_4 <- which.max(depths4$dep)  
curva_median_4 <- fd_obj_4[median_index_4]
#
plot(fd_obj_4, col = alpha("gray", 0.3), lwd = 1,
     main = paste("Depth 4"),
     xlab = "Age", ylab = "People")
lines(curva_median_4, col = "red", lwd = 2)
legend("topright", legend = "Functional median (depth)", col = "red", lwd = 2)
#
municipios_orden_4 <- df_smooth_4 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  pull(municipio)
#
municipio_median_4 <- municipios_orden_4[median_index_4]
municipio_median_4
#
######### Grupo 5 ##########
#
df_smooth_5 <- suavizados_df5 %>%
  arrange(municipio, Edad)
#
mat_curvas5 <- df_smooth_5 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  dplyr::select(-municipio, -year) %>%
  as.matrix()
#
edades5 <- sort(unique(df_smooth_5$Edad))
fd_obj_5 <- fdata(mat_curvas5, argvals = edades5)
#
depths5 <- depth.mode(fd_obj_5)
median_index_5 <- which.max(depths5$dep)
curva_median_5 <- fd_obj_5[median_index_5]
#
plot(fd_obj_5, col = alpha("gray", 0.3), lwd = 1,
     main = paste("Depth 5"),
     xlab = "Age", ylab = "People")
lines(curva_median_5, col = "red", lwd = 2)
legend("topright", legend = "Functional median (depth)", col = "red", lwd = 2)
#
municipios_orden_5 <- df_smooth_5 %>%
  tidyr::pivot_wider(names_from = Edad, values_from = Personas) %>%
  pull(municipio)
#
municipio_median_5 <- municipios_orden_5[median_index_5]
municipio_median_5
#
# Values of the depth of each group
#
mun_depth_gr1_smooth <- suavizados_df1 %>%
  filter(municipio=="Villamartín de Don Sancho")
#
mun_depth_gr2_smooth <- suavizados_df2 %>%
  filter(municipio=="Valderrey")
#
mun_depth_gr3_smooth <- suavizados_df3 %>%
  filter(municipio=="Congosto")
#
mun_depth_gr4_smooth <- suavizados_df4 %>%
  filter(municipio=="Valencia de Don Juan")
#
mun_depth_gr5_smooth <- suavizados_df5 %>%
  filter(municipio=="León")
#
### Obtain their values for men and women ###
#
# Obtain data for the depth of each group in 2003 and 2024 for men and women
#
# Women 2024
depth_gr5m <- wom_gr5 %>%
  filter(year == 2024, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr5m$Edad
y <- depth_gr5m$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

funct_smooth_5m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5m <- data.frame(
    Edad = edad_fina_grp5,
    Personas = eval.fd(edad_fina_grp5, funct_smooth_5m$fd),
    municipio = "León",
    year = 2024)
#
#Men 2024
#
depth_gr5h <- men_gr5 %>%
  filter(year == 2024, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr5h$Edad
y <- depth_gr5h$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

funct_smooth_5h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5h <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, funct_smooth_5h$fd),
  municipio = "León",
  year = 2024)
#
#
####################### Pyram evolution for depths #############################
#
# City of Leon
leon.city <- datos_combinados %>%
  filter(municipio=="León") %>%
  arrange(year, Edad)
#
# Parameters
lambda.leon.city <- 1e-02
n_basis <- 18  
age_range <- range(leon.city$Edad, na.rm = TRUE)
basis.city.leon <- create.bspline.basis(rangeval = age_range, nbasis = n_basis)
fdParobj.leon.city <- fdPar(basis.city.leon, Lfdobj = int2Lfd(2), lambda = lambda.leon.city)

edad_grid <- seq(age_range[1], age_range[2], length.out = 101)

curvas_suavizadas <- list()

for (año in unique(leon.city$year)) {
  datos_año <- leon.city %>% filter(year == año)
  
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.leon.city
  )$fd
  
  personas_suavizadas <- eval.fd(edad_grid, smooth_fd)
  
  df_suavizado <- data.frame(
    Edad = edad_grid,
    Personas = personas_suavizadas,
    year = año
  )
  
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

pevol5 <- ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in León (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
edad_fina_grp3 
age_range_grp3 
n_basis3 
basis3 
lambda3  
fd_obj_3
fdParobj.gr3 <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

curvas_suavizadas <- list()

for (año in unique(tot_gr3$year)) {
  datos_año <- tot_gr3 %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr3
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp3, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp3,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

pevol3 <- ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Congosto, G3 (2003–2024)",
    x = "Age", y = "Number of people"
  )

edad_fina_grp4 
age_range_grp4 
n_basis4 
basis4 
lambda4 
fd_obj_4
fdParobj.gr4 <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

curvas_suavizadas <- list()

for (año in unique(tot_gr4$year)) {
  datos_año <- tot_gr4 %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr4
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp4, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp4,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

pevol4 <- ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Valendia de D. Juan, G4 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
edad_fina_grp1 
age_range_grp1 
n_basis1 
basis1 
lambda1 
fd_obj_1
fdParobj.gr1 <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

curvas_suavizadas <- list()

for (año in unique(tot_gr1_agrupado$year)) {
  datos_año <- tot_gr1_agrupado %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad_central,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr1
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp1, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp1,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

pevol1 <-  ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Villamartín de D. Sancho, G1 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
edad_fina_grp2 
age_range_grp2 
n_basis2 
basis2 
lambda2  
fd_obj_2
fdParobj.gr2 <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

curvas_suavizadas <- list()

for (año in unique(tot_gr2_agrupado$year)) {
  datos_año <- tot_gr2_agrupado %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad_central,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr2
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp2, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp2,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

pevol2 <- ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Valderrey, G2 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
# For first two municipalites in raw data
#
sanchoevoltot <- tot_gr1_agrupado %>%
  filter(municipio=="Villamartín de Don Sancho")
#
pevol1raw <-  ggplot(sanchoevoltot, aes(x = Edad_central, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evol. of the distr. of pop. by age in V. de D. Sancho, G1 (2003–2024)",
    x = "Age", y = "Number of people"
  )
pevol1raw
#
reyevoltot <- tot_gr2_agrupado %>%
  filter(municipio=="Valderrey")

pevol2raw <-  ggplot(reyevoltot, aes(x = Edad_central, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  
  theme_minimal() +
  labs(
    title = "Evol. of the distr. of pop. by age in Valderrey, G1 (2003–2024)",
    x = "Age", y = "Number of people"
  )
pevol2raw
#
############################## Outliers ########################################
#
# One analysis per group first for data in 2024
#
######## Group 1 ########
#
suave_df1<- suavizados_df1 %>%
  mutate(municipio = paste(municipio, year, sep = "_")) %>%
  dplyr::select(municipio, Edad_central, Personas) %>%
  pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  arrange(municipio)
#
# Convert to matrix
suave_df1_matriz <- suave_df1 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(suave_df1_matriz) <- suave_df1$municipio
#
ms1 <- msplot(dts = suave_df1_matriz, return_mvdir = TRUE, plot = TRUE)
#
outliers1 <- ms1$outliers
#
# Outlyingness de magnitude and shape
head(ms1$mean_outlyingness)  
head(ms1$var_outlyingness)   
#
MO1 <- ms1$mean_outlyingness
VO1 <- ms1$var_outlyingness

clasificacion <- character(length(outliers1))

for (i in seq_along(outliers1)) {
  idx <- outliers1[i]
  if (MO1[idx]^2 > VO1[idx]) {
    clasificacion[i] <- "magnitude"
  } else if (VO1[idx] > MO1[idx]^2) {
    clasificacion[i] <- "shape"
  } else {
    clasificacion[i] <- "both"
  }
}
#
outliers1labels <- rownames(suave_df1_matriz)[ms1$outliers]
data.frame(index = outliers1,name=outliers1labels, type = clasificacion)
#
# Create long dataframe
long_df1 <- suave_df1_matriz %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(-id, names_to = "edad", values_to = "personas") %>%
  mutate(edad = as.numeric(edad))

# Add a column to lavel the outliers
long_df1<- long_df1 %>%
  mutate(tipo = ifelse(id %in% rownames(suave_df1_matriz)[ms1$outliers], "outlier", "normal"))
#
tot_outliers1 <- tot_gr1_agrupado %>%
  mutate(tipo = ifelse(id %in% rownames(suave_df1_matriz)[ms1$outliers], "outlier", "normal"))
#
ggplot(long_df1, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal()
#
######### Group 2 #########
#
suave_df2<- suavizados_df2 %>%
  mutate(municipio = paste(municipio, year, sep = "_")) %>%
  dplyr::select(municipio, Edad_central, Personas) %>%
  pivot_wider(names_from = Edad_central, values_from = Personas) %>%
  arrange(municipio)
#
suave_df2_matriz <- suave_df2 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(suave_df2_matriz) <- suave_df2$municipio
#
ms2 <- msplot(dts = suave_df2_matriz, return_mvdir = TRUE, plot = TRUE)
#
outliers2 <- ms2$outliers
#
head(ms2$mean_outlyingness) 
head(ms2$var_outlyingness)   
#
MO2 <- ms2$mean_outlyingness
VO2 <- ms2$var_outlyingness

clasificacion <- character(length(outliers2))

for (i in seq_along(outliers2)) {
  idx <- outliers2[i]
  if (MO2[idx]^2 > VO2[idx]) {
    clasificacion[i] <- "magnitude"
  } else if (VO2[idx] > MO2[idx]^2) {
    clasificacion[i] <- "shape"
  } else {
    clasificacion[i] <- "both"
  }
}
#
outliers2labels <- rownames(suave_df2_matriz)[ms2$outliers]
data.frame(index = outliers2,name=outliers2labels, type = clasificacion)
#
long_df2 <- suave_df2_matriz %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(-id, names_to = "edad", values_to = "personas") %>%
  mutate(edad = as.numeric(edad))

long_df2<- long_df2 %>%
  mutate(tipo = ifelse(id %in% rownames(suave_df2_matriz)[ms2$outliers], "outlier", "normal"))
#
ggplot(long_df2, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal()
#
####### Group 3 ########
#
suave_df3<- suavizados_df3 %>%
  mutate(municipio = paste(municipio, year, sep = "_")) %>%
  dplyr::select(municipio, Edad, Personas) %>%
  pivot_wider(names_from = Edad, values_from = Personas) %>%
  arrange(municipio)
#
suave_df3_matriz <- suave_df3 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(suave_df3_matriz) <- suave_df3$municipio
#
ms3 <- msplot(dts = suave_df3_matriz, return_mvdir = TRUE, plot = TRUE)
#
outliers3 <- ms3$outliers
#
head(ms3$mean_outlyingness)  
head(ms3$var_outlyingness)  
#
MO3 <- ms3$mean_outlyingness
VO3 <- ms3$var_outlyingness

clasificacion <- character(length(outliers3))

for (i in seq_along(outliers3)) {
  idx <- outliers3[i]
  if (MO3[idx]^2 > VO3[idx]) {
    clasificacion[i] <- "magnitude"
  } else if (VO3[idx] > MO3[idx]^2) {
    clasificacion[i] <- "shape"
  } else {
    clasificacion[i] <- "both"
  }
}
#
outliers3labels <- rownames(suave_df3_matriz)[ms3$outliers]
data.frame(index = outliers3,name=outliers3labels, type = clasificacion)
#
long_df3 <- suave_df3_matriz %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(-id, names_to = "edad", values_to = "personas") %>%
  mutate(edad = as.numeric(edad))

long_df3<- long_df3 %>%
  mutate(tipo = ifelse(id %in% rownames(suave_df3_matriz)[ms3$outliers], "outlier", "normal"))
#
ggplot(long_df3, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal()
#
######################### outliers with other method ###########################
#
# Filter 2024
datos_2024 <- datos_combinados %>%
  filter(year == 2024) %>%
  dplyr::select(municipio, Edad, Personas) %>%
  pivot_wider(names_from = Edad, values_from = Personas)
#
# G 1
#
#Create a new fdata
edades1 <- as.numeric(colnames(suave_df1_matriz))
fdata.outlier1 <- fData(grid = edades1, values =as.matrix(suave_df1_matriz))

# Boxplot funcional
fbplot(fdata.outlier1, main = "FBPlot - Outliers detection G1")
#
# Outliergram
outliergram(fdata.outlier1)
#
D1<-depthgram(fdata.outlier1) 
plot(D1)
#
# Indeces
out_shape_idx1 <- depthgram(fdata.outlier1)$shp.out.det[[1]]
out_magn_idx1  <- depthgram(fdata.outlier1)$mag.out.det[[1]]
#
# Combine unique indeces
outlier_idx1 <- unique(c(out_shape_idx1, out_magn_idx1))

# Indeces for normal municipalities
inlier_idx1 <- setdiff(seq_len(nrow(fdata.outlier1$values)), outlier_idx1)
#
# Name of muicipalities
outlier_municipios1 <- datos_2024$municipio[outlier_idx1]
inlier_municipios1 <- datos_2024$municipio[inlier_idx1]
#
# Show
outlier_municipios1
inlier_municipios1
#
# G 2
#
#Create a new fdata
edades2 <- as.numeric(colnames(suave_df2_matriz))
fdata.outlier2 <- fData(grid = edades2, values =as.matrix(suave_df2_matriz))

# Boxplot funcional
fbplot(fdata.outlier2, main = "FBPlot - Outliers detection G2")
#
# Outliergram
outliergram(fdata.outlier2)
#
D2<-depthgram(fdata.outlier2)
plot(D2)
#
# G 3
#
#Create a new fdata
edades3 <- as.numeric(colnames(suave_df3_matriz))
fdata.outlier3 <- fData(grid = edades3, values =as.matrix(suave_df3_matriz))

# Boxplot funcional
fbplot(fdata.outlier3, main = "FBPlot - Outliers detection G3")
#
# Outliergram
outliergram(fdata.outlier3)
#
D3<-depthgram(fdata.outlier3) 
plot(D3)
#
############################## Clustering ######################################
#
# General clustering (all municipalities normalized data)
#
# New dataframe
# Take 2024
pob.norm24 <- pob.norm %>% filter(year == 2024) %>% filter(Sex== "Total") %>% dplyr::select(municipio, Edad, normalized)
#
# Convert to wide
pob.norm24_wide <- pob.norm24  %>%
  pivot_wider(names_from = Edad, values_from = normalized)

# Plot
pob.norm24_long <- pob.norm24_wide %>%
  pivot_longer(
    cols = -municipio,
    names_to = "Edad",
    values_to = "normalized"
  ) %>%
  mutate(Edad = as.numeric(Edad))  
#
ggplot(pob.norm24_long, aes(x = Edad, y = normalized, group = municipio, color = municipio)) +
  geom_line(alpha = 0.6) +
  labs(
    title = "Curvas de población normalizada por municipio (2024)",
    x = "Edad",
    y = "Población normalizada"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#
# Pure matrix for fdta and names
municipios <- pob.norm24_wide$municipio
pob_mat <- as.matrix(pob.norm24_wide[,-1])
pob_mat <- pmax(pob_mat, 0)
rownames(pob_mat) <- municipios

# Define axis of ages (columns)
edad <- as.numeric(colnames(pob_mat))

# Functional object
fdobj <- fdata(pob_mat, argvals = edad, rangeval = range(edad))

#  Represent functional alternative 
fdobj_fd <- fdata2fd(fdobj, nbasis = 11)

set.seed(123)
kmf <- kmeans.fd(fdobj_fd,
                 ncl = 5,  # Number of clusters
                 metric = metric.lp,
                 par.metric = list(lp = 2),
                 draw = TRUE)

clust_labels <- kmf$cluster

n_clusters <- length(unique(kmf$cluster))

munic_cluster <- rownames(pob_mat)

df_clusters <- data.frame(
  Municipio = munic_cluster,
  Cluster = clust_labels
)

# Colors per cluster
colors <- rainbow(length(unique(clust_labels)))

par(mfrow = c(1,1))

# Plot each cluster
plot(fdobj_fd, col = colors[clust_labels], lty = 1, lwd = 1.5, 
     main = "Colored curves per cluster",
     xlab="age")

legend("topright",
       legend = paste("Cluster", 1:n_clusters),
       col = colors,
       lty = 1,
       lwd = 2,
       cex = 0.8,
       box.lty = 0)
#
# Truncate
#
# Evaluate smoothed curves
pob_mat_eval <- predict(fdobj_fd, newdata = edad)

# Truncate to 0 negative values
pob_mat_eval_trunc <- pmax(pob_mat_eval, 0)

# Functional object for truncated smoothed curves
fdobj_fd_trunc <- fdata(t(pob_mat_eval_trunc), 
                        argvals = edad, 
                        rangeval = range(edad))

# K-means clustering with truncated curves
set.seed(123)
kmf <- kmeans.fd(fdata2fd(fdobj_fd_trunc, nbasis = 15),
                 ncl = 5,
                 metric = metric.lp,
                 par.metric = list(lp = 2),
                 draw = FALSE)  
clust_labels <- kmf$cluster
n_clusters <- length(unique(clust_labels))

colors <- rainbow(n_clusters)

plot(fdobj_fd_trunc, col = colors[clust_labels], lty = 1, lwd = 1.5, 
     main = "Colored curves per cluster",
     xlab = "age", ylab = "value")

legend("topright",
       legend = paste("Cluster", 1:n_clusters),
       col = colors,
       lty = 1,
       lwd = 2,
       cex = 0.8,
       box.lty = 0)
#
par(mfrow = c(ceiling(n_clusters / 2), 2))  
for (i in 1:n_clusters) {
  idx <- which(clust_labels == i)
  fd_cluster <- fdobj_fd_trunc[idx]
  # Plot
  plot(fd_cluster,
       main = paste("Cluster", i),
       col = colors[i],
       lwd = 1.5,
       xlab = "Age",
       ylab = "Normalized Population",
       ylim = c(0, 1))  # same scale 0–1
}

par(mfrow = c(ceiling(n_clusters / 2), 2))  

for (i in 1:n_clusters) {
  idx <- which(clust_labels == i)
  fd_cluster <- fdobj_fd[idx]
  # Plot
  plot(fd_cluster,
       main = paste("Cluster", i),
       col = colors[i],
       lwd = 1.5,
       xlab = "Age",
       ylab = "Normalized Population",
       ylim = c(0, 1))
}
#
# Same but firstly smooth and the normalization
# USED METHOD
#
pob24 <- pob.norm %>%
  filter(year == 2024, Sex == "Total") %>%
  dplyr::select(municipio, Edad, Personas) 

pob24_wide <- pob24 %>%
  pivot_wider(names_from = Edad, values_from = Personas)

municipios <- pob24_wide$municipio
pob_mat <- as.matrix(pob24_wide[, -1])
pob_mat <- pmax(pob_mat, 0) 
rownames(pob_mat) <- municipios

edad <- as.numeric(colnames(pob_mat))

fdobj <- fdata(pob_mat, argvals = edad, rangeval = range(edad))

fdobj_fd <- fdata2fd(fdobj, nbasis = 11)

pob_mat_suav <- predict(fdobj_fd, newdata = edad)

pob_mat_suav <- pmax(pob_mat_suav, 0)

pob_mat_norm <- t(apply(pob_mat_suav, 2, function(x) x / max(x)))

fdobj_fd_norm <- fdata(pob_mat_norm,
                       argvals = edad,
                       rangeval = range(edad))

set.seed(123)
kmf <- kmeans.fd(fdata2fd(fdobj_fd_norm, nbasis = 15),
                 ncl = 5,
                 metric = metric.lp,
                 par.metric = list(lp = 2),
                 draw = FALSE)

clust_labels <- kmf$cluster
n_clusters <- length(unique(clust_labels))

colors <- rainbow(n_clusters)

plot(fdobj_fd_norm, col = colors[clust_labels], lty = 1, lwd = 1.5,
     main = "Colored curves per cluster",
     xlab = "Age", ylab = "Value")
legend("topleft",
       legend = paste("Cluster", 1:n_clusters),
       col = colors,
       lty = 1,
       lwd = 2,
       cex = 0.8,
       box.lty = 0)

par(mfrow = c(ceiling(n_clusters / 2), 2))  
for (i in 1:n_clusters) {
  idx <- which(clust_labels == i)
  fd_cluster <- fdobj_fd_norm[idx]
  plot(fd_cluster,
       main = paste("Cluster", i),
       col = colors[i],
       lwd = 1.5,
       xlab = "Age",
       ylab = "Normalized Population",
       ylim = c(0, 1))
}

# New dataframe with each cluster label
df_clusters <- data.frame(
  Municipio = municipios,
  Cluster = clust_labels
)

df_clusters %>% 
  arrange(Cluster)

conteo_clusters <- df_clusters %>% 
  group_by(Cluster) %>% 
  summarise(N_municipios = n(),
            Municipios = paste(Municipio, collapse = ", "))

conteo_clusters

####################### clustering inside ad-hoc groups ########################
#
tot_gr1_agr_24 <- tot_gr1_agrupado %>%
  filter(year == 2024) %>%   
  dplyr::select(municipio, Edad_central, Personas)  

tot_gr1_agr_24 <- tot_gr1_agr_24 %>%
  pivot_wider(names_from = Edad_central, values_from = Personas)

tot_gr1_agr_24_matrix <- tot_gr1_agr_24 %>%
  dplyr::select(-municipio) %>%  
  as.matrix()             

rownames(tot_gr1_agr_24_matrix) <- tot_gr1_agr_24$municipio

edades <- as.numeric(colnames(tot_gr1_agr_24_matrix))

nb <- 13
basis <- create.bspline.basis(range(edades), nbasis = nb)
fdParobj <- fdPar(basis, Lfdobj = int2Lfd(2), lambda = 1e-2)
fd_smooth.g1 <- smooth.basis(edades, t(tot_gr1_agr_24_matrix), fdParobj)$fd
plot(fd_smooth.g1, main = "Smoothed curves for group 1")

# Evaluate the smoothed object in age points 
eval_mat <- eval.fd(edades, fd_smooth.g1)

# Transpose (municipalities in rows)
eval_mat_t <- t(eval_mat)

# fdata object with the smoothed matrix
fdata.clust.g1 <- fdata(eval_mat_t, argvals = edades, rangeval = range(edades))

# Plot
plot(fdata.clust.g1, main = "Curvas evaluadas del objeto fd (suavizadas)")

km.g1 <- kmeans.fd(fd_smooth.g1, ncl = 3, draw = TRUE) 

clust_labels.g1 <- km.g1$cluster

colors <- c("#E57373", "#FFB74D", "#81C784") 
plot(fd_smooth.g1, col = colors[clust_labels.g1], lwd = 1.5,
     main = "Smoothed curves colored by cluster G1",
     xlab = "Age", ylab = "Population")

munic_cluster.g1 <- rownames(tot_gr1_agr_24_matrix)

df_clusters.g1 <- data.frame(
  Municipio = munic_cluster.g1,
  Cluster = clust_labels.g1
)

munic.g1 <- rownames(tot_gr1_agr_24_matrix)

####### Group 2 ########

tot_gr2_agr_24 <- tot_gr2_agrupado %>%
  filter(year == 2024) %>%  
  dplyr::select(municipio, Edad_central, Personas)  

tot_gr2_agr_24 <- tot_gr2_agr_24 %>%
  pivot_wider(names_from = Edad_central, values_from = Personas)

tot_gr2_agr_24_matrix <- tot_gr2_agr_24 %>%
  dplyr::select(-municipio) %>%  
  as.matrix()             

rownames(tot_gr2_agr_24_matrix) <- tot_gr2_agr_24$municipio

edades <- as.numeric(colnames(tot_gr2_agr_24_matrix))

for(nb in c(13,16,23)){
  basis <- create.bspline.basis(range(edades), nbasis = nb)
  fdParobj <- fdPar(basis, Lfdobj = int2Lfd(2), lambda = 1e-2)
  fd_smooth.g2 <- smooth.basis(edades, t(tot_gr2_agr_24_matrix), fdParobj)$fd
  plot(fd_smooth.g2, main = "Smoothed curves for group 2")
}

eval_mat <- eval.fd(edades, fd_smooth.g2) 

eval_mat_t <- t(eval_mat)

fdata.clust.g2 <- fdata(eval_mat_t, argvals = edades, rangeval = range(edades))

plot(fdata.clust.g2, main = "Curvas evaluadas del objeto fd (suavizadas)")

km.g2 <- kmeans.fd(fd_smooth.g2, ncl = 3, draw = TRUE) 

clust_labels.g2 <- km.g2$cluster

colors <- c("#E57373", "#FFB74D", "#81C784")  
plot(fd_smooth.g2, col = colors[clust_labels.g2], lwd = 1.5,
     main = "Smoothed curves colored by cluster G2",
     xlab = "Age", ylab = "Population")

munic_cluster.g2 <- rownames(tot_gr2_agr_24_matrix)

df_clusters.g2 <- data.frame(
  Municipio = munic_cluster.g2,
  Cluster = clust_labels.g2
)

munic.g2 <- rownames(tot_gr2_agr_24_matrix)

########## Group 3 ########

tot_gr3_agr_24 <- tot_gr3 %>%
  filter(year == 2024) %>%   
  dplyr::select(municipio, Edad, Personas) 

tot_gr3_agr_24 <- tot_gr3_agr_24 %>%
  pivot_wider(names_from = Edad, values_from = Personas)

tot_gr3_agr_24_matrix <- tot_gr3_agr_24 %>%
  dplyr::select(-municipio) %>%  
  as.matrix()             

rownames(tot_gr3_agr_24_matrix) <- tot_gr3_agr_24$municipio

edades <- as.numeric(colnames(tot_gr3_agr_24_matrix))

nb <- 16
basis <- create.bspline.basis(range(edades), nbasis = nb)
fdParobj <- fdPar(basis, Lfdobj = int2Lfd(2), lambda = 1e-2)
fd_smooth.g3 <- smooth.basis(edades, t(tot_gr3_agr_24_matrix), fdParobj)$fd
plot(fd_smooth.g3, main = "Smoothed curves for group 1")

eval_mat <- eval.fd(edades, fd_smooth.g3)  

eval_mat_t <- t(eval_mat)

fdata.clust.g3 <- fdata(eval_mat_t, argvals = edades, rangeval = range(edades))

plot(fdata.clust.g3, main = "Curvas evaluadas del objeto fd (suavizadas)")

km.g3 <- kmeans.fd(fd_smooth.g3, ncl = 3, draw = TRUE) 

clust_labels.g3 <- km.g3$cluster

colors <- c("#E57373", "#FFB74D", "#81C784")
plot(fd_smooth.g3, col = colors[clust_labels.g3], lwd = 1.5,
     main = "Smoothed curves colored by cluster G3",
     xlab = "Age", ylab = "Normalized population")

munic_cluster.g3 <- rownames(tot_gr3_agr_24_matrix)

df_clusters.g3 <- data.frame(
  Municipio = munic_cluster.g3,
  Cluster = clust_labels.g3
)

munic.g3 <- rownames(tot_gr3_agr_24_matrix)

for (k in 1:3) {
  plot(fd_smooth.g3[clust_labels.g3 == k],
       col = colors[k],
       lwd = 1.5,
       main = paste("Cluster", k),
       xlab = "Age", ylab = "Population")
}

# This is not very relevant, so the method is performed but with the normalized curves

#  Funcion that normalize row by row by max
row_max_normalize <- function(M){
  t(apply(M, 1, function(x){
    mx <- max(x, na.rm = TRUE)
    if (is.finite(mx) && mx > 0) x / mx else x
  }))
}

make_silhouette <- function(mat_rows_by_munic, clusters){
  d <- dist(mat_rows_by_munic, method = "euclidean")
  silhouette(clusters, d)
}

plot_clusters_fd <- function(fdobj, clusters, title){
  ncl <- length(unique(clusters))
  colors <- rainbow(ncl)
  plot(fdobj, col = colors[clusters], lwd = 1.5,
       main = title, xlab = "Age", ylab = "Normalized value")
  legend("topright", legend = paste("Cluster", 1:ncl),
         col = colors, lty = 1, lwd = 2, cex = 0.8, box.lty = 0)
}

####### Group 1 ######

tot_gr1_agr_24 <- tot_gr1 %>%
  filter(year == 2024) %>%
  dplyr::select(municipio, Edad, Personas) %>%
  pivot_wider(names_from = Edad, values_from = Personas)

tot_gr1_agr_24_matrix <- tot_gr1_agr_24 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(tot_gr1_agr_24_matrix) <- tot_gr1_agr_24$municipio
edades.g1 <- as.numeric(colnames(tot_gr1_agr_24_matrix))

# Smooth
nb.g1 <- 11
basis.g1 <- create.bspline.basis(range(edades.g1), nbasis = nb.g1)
fdParobj.g1 <- fdPar(basis.g1, Lfdobj = int2Lfd(2), lambda = 1e-2)
fd_smooth.g1 <- smooth.basis(edades.g1, t(tot_gr1_agr_24_matrix), fdParobj.g1)$fd
plot(fd_smooth.g1, main = "Smoothed curves for group 1")

# Evaluate, truncate and normalize
eval_mat.g1 <- eval.fd(edades.g1, fd_smooth.g1)
eval_mat_t.g1 <- t(eval_mat.g1)
eval_mat_t.g1 <- pmax(eval_mat_t.g1, 0)
norm_mat.g1 <- row_max_normalize(eval_mat_t.g1)

# Normalized functional object
fdata_norm.g1 <- fdata(norm_mat.g1, argvals = edades.g1, rangeval = range(edades.g1))
fd_norm.g1 <- fdata2fd(fdata_norm.g1, nbasis = nb.g1)

# Clustering
set.seed(123)
km.g1 <- kmeans.fd(fd_norm.g1, ncl = 5, draw = TRUE)
clust_labels.g1 <- km.g1$cluster

km.g1 <- kmeans.fd(fd_norm.g1, ncl = 5, draw = FALSE)  

clust_labels.g1 <- km.g1$cluster
table(clust_labels.g1)  

# Plot per cluster
plot_clusters_fd(fd_norm.g1, clust_labels.g1, "Clustering for normalized curves G1")

# Table of municipalities per cluster
df_clusters.g1 <- data.frame(Municipio = rownames(tot_gr1_agr_24_matrix),
                             Cluster = clust_labels.g1)
conteo.g1 <- df_clusters.g1 %>% group_by(Cluster) %>%
  summarise(N = n(), Municipios = paste(Municipio, collapse = ", "), .groups = "drop")
print(conteo.g1)

# Silhouette
sil.g1 <- make_silhouette(norm_mat.g1, clust_labels.g1)
cat("\nG1 - Silhouette average width:", mean(sil.g1[, "sil_width"]), "\n")
plot(sil.g1, main = "G1 - Silhouette")

####### Group 2 ######

tot_gr2_agr_24 <- tot_gr2 %>%
  filter(year == 2024) %>%
  dplyr::select(municipio, Edad, Personas) %>%
  pivot_wider(names_from = Edad, values_from = Personas)

tot_gr2_agr_24_matrix <- tot_gr2_agr_24 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(tot_gr2_agr_24_matrix) <- tot_gr2_agr_24$municipio
edades.g2 <- as.numeric(colnames(tot_gr2_agr_24_matrix))

nb.g2 <- 16
basis.g2 <- create.bspline.basis(range(edades.g2), nbasis = nb.g2)
fdParobj.g2 <- fdPar(basis.g2, Lfdobj = int2Lfd(2), lambda = 1e-2)
fd_smooth.g2 <- smooth.basis(edades.g2, t(tot_gr2_agr_24_matrix), fdParobj.g2)$fd
plot(fd_smooth.g2, main = "Smoothed curves for group 2")

eval_mat.g2 <- eval.fd(edades.g2, fd_smooth.g2)
eval_mat_t.g2 <- t(eval_mat.g2)
eval_mat_t.g2 <- pmax(eval_mat_t.g2, 0)
norm_mat.g2 <- row_max_normalize(eval_mat_t.g2)

fdata_norm.g2 <- fdata(norm_mat.g2, argvals = edades.g2, rangeval = range(edades.g2))
fd_norm.g2 <- fdata2fd(fdata_norm.g2, nbasis = nb.g2)

set.seed(123)
km.g2 <- kmeans.fd(fd_norm.g2, ncl = 5, draw = TRUE)
clust_labels.g2 <- km.g2$cluster

km.g2 <- kmeans.fd(fd_norm.g2, ncl = 5, draw = FALSE) 

clust_labels.g2 <- km.g2$cluster
table(clust_labels.g2)

plot_clusters_fd(fd_norm.g2, clust_labels.g2, 
                 "Clustering for normalized curves G2")

df_clusters.g2 <- data.frame(Municipio = rownames(tot_gr2_agr_24_matrix),
                             Cluster = clust_labels.g2)
conteo.g2 <- df_clusters.g2 %>% group_by(Cluster) %>%
  summarise(N = n(), Municipios = paste(Municipio, collapse = ", "), .groups = "drop")
print(conteo.g2)

sil.g2 <- make_silhouette(norm_mat.g2, clust_labels.g2)
cat("\nG2 - Silhouette average width:", mean(sil.g2[, "sil_width"]), "\n")
plot(sil.g2, main = "G2 - Silhouette")

######## Group 3 ########

tot_gr3_agr_24 <- tot_gr3 %>%
  filter(year == 2024) %>%
  dplyr::select(municipio, Edad, Personas) %>%
  pivot_wider(names_from = Edad, values_from = Personas)

tot_gr3_agr_24_matrix <- tot_gr3_agr_24 %>% dplyr::select(-municipio) %>% as.matrix()
rownames(tot_gr3_agr_24_matrix) <- tot_gr3_agr_24$municipio
edades.g3 <- as.numeric(colnames(tot_gr3_agr_24_matrix))

nb.g3 <- 16
basis.g3 <- create.bspline.basis(range(edades.g3), nbasis = nb.g3)
fdParobj.g3 <- fdPar(basis.g3, Lfdobj = int2Lfd(2), lambda = 1e-2)
fd_smooth.g3 <- smooth.basis(edades.g3, t(tot_gr3_agr_24_matrix), fdParobj.g3)$fd
plot(fd_smooth.g3, main = "Smoothed curves for group 3")

eval_mat.g3 <- eval.fd(edades.g3, fd_smooth.g3)
eval_mat_t.g3 <- t(eval_mat.g3)
eval_mat_t.g3 <- pmax(eval_mat_t.g3, 0)
norm_mat.g3 <- row_max_normalize(eval_mat_t.g3)

fdata_norm.g3 <- fdata(norm_mat.g3, argvals = edades.g3, rangeval = range(edades.g3))
fd_norm.g3 <- fdata2fd(fdata_norm.g3, nbasis = nb.g3)

set.seed(123)
km.g3 <- kmeans.fd(fd_norm.g3, ncl = 4, draw = TRUE)
clust_labels.g3 <- km.g3$cluster

km.g3 <- kmeans.fd(fd_norm.g3, ncl = 4, draw = FALSE) 

clust_labels.g3 <- km.g3$cluster
table(clust_labels.g3)

plot_clusters_fd(fd_norm.g3, clust_labels.g3, "Clustering for normalized curves G3")

df_clusters.g3 <- data.frame(Municipio = rownames(tot_gr3_agr_24_matrix),
                             Cluster = clust_labels.g3)
conteo.g3 <- df_clusters.g3 %>% group_by(Cluster) %>%
  summarise(N = n(), Municipios = paste(Municipio, collapse = ", "), .groups = "drop")
print(conteo.g3)

sil.g3 <- make_silhouette(norm_mat.g3, clust_labels.g3)
cat("\nG3 - Silhouette average width:", mean(sil.g3[, "sil_width"]), "\n")
plot(sil.g3, main = "G3 - Silhouette")

####################### Other used important plots #############################

# Maps

unique(long_df1$id[long_df1$id %in% outliers_g1])  

# column color_group
long_df1 <- long_df1 %>%
  mutate(color_group = ifelse(id %in% outliers_g1, id, "normal"))

long_df1$color_group <- factor(long_df1$color_group, levels = c(outliers_g1, "normal"))

colores_g1 <- c(
  setNames(c("blue", "green", "purple", "orange"), outliers_g1),
  "normal" = "gray70"
)

labels_g1 <- c(
  setNames(nombres_custom$nombre_mostrar, nombres_custom$id),
  "normal" = "Otros municipios"
)

# Plot
ggplot(long_df1, aes(x = edad, y = personas, group = id, color = color_group)) +
  geom_line() +
  scale_color_manual(
    values = colores_g1,
    labels = labels_g1,
    name = "Municipality",
    drop = FALSE
  ) +
  labs(
    title = "Smoothed population curves - Group 1",
    x = "Age", y = "Population"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(0.5, "cm") 
  )
#
# GROUP 2
#
# Names for 9 outliers
nombres_custom2 <- data.frame(
  id = c(
    "Alija del Infantado_2024",
    "Corullón_2024",
    "Hospital de Órbigo_2024",
    "Luyego_2024",
    "Molinaseca_2024",
    "Palacios del Sil_2024",
    "Villamanín_2024",
    "Villaquejida_2024"
  ),
  nombre_mostrar = c(
    "Alija del Infantado",
    "Corullón",
    "Hospital de Órbigo",
    "Luyego",
    "Molinaseca",
    "Palacios del Sil",
    "Villamanín",
    "Villaquejida"
  )
)

# Filter final points of each outlier and joint names
etiquetas2 <- long_df2 %>%
  filter(tipo == "outlier") %>%
  group_by(id) %>%
  filter(edad == max(edad)) %>%
  left_join(nombres_custom2, by = "id")

# Plot 
ggplot(long_df2, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(
    title = "Smoothed population curves with detected functional outliers",
    x = "Age", y = "Population",
    color = "Type"
  ) +
  theme_minimal() +
  geom_text_repel(
    data = etiquetas2,
    aes(label = nombre_mostrar),
    size = 3,
    color = "black",         
    fontface = "plain",       
    box.padding = 0.5,
    point.padding = 0.3,
    nudge_x = 10,              
    segment.color = "black",   
    segment.size = 0.4,
    arrow = arrow(length = unit(0.02, "npc")),
    max.overlaps = 20,      
    show.legend = FALSE
  )
#
#Group 3
#
# Vector outliers
outliers <- c("Fabero_2024", 
              "Pola de Gordón, La_2024", 
              "Villafranca del Bierzo_2024")

colores <- c(
  setNames(c("blue", "green", "purple"), outliers),
  "normal" = "gray70"
)

long_df3 <- long_df3 %>%
  mutate(color_group = ifelse(id %in% outliers, id, "normal"))

long_df3$color_group <- factor(long_df3$color_group, levels = c(outliers, "normal"))

labels <- c(
  "Fabero_2024" = "Fabero",
  "Pola de Gordón, La_2024" = "La Pola de Gordón",
  "Villafranca del Bierzo_2024" = "Villafranca del Bierzo",
  "normal" = "Otros municipios"
)

# Plot 
ggplot(long_df3, aes(x = edad, y = personas, group = id, color = color_group)) +
  geom_line() +
  scale_color_manual(
    values = colores,
    labels = labels,
    name = "Municipality",
    drop = FALSE
  ) +
  labs(
    title = "Smoothed population curves with detected functional outliers",
    x = "Age", y = "Population"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.5, "cm")
  )

#
#################### Raw pyramid of total #######################
#
# Filter 2003 and 2024
piramide_h <- datos_combinados_th %>%
  filter(year %in% c(2003, 2024)) %>%
  mutate(Sex = "Male",
         Personas = -Personas)  # Male to the left  (-)
#
piramide_m <- datos_combinados_tm %>%
  filter(year %in% c(2003, 2024)) %>%
  mutate(Sex = "Female")
#
# Combine
piramide_total <- bind_rows(piramide_h, piramide_m)
#
# Funcition to create a pyramid per year
plot_piramide <- function(año) {
  piramide_total %>%
    filter(year == año) %>%
    group_by(Sex, Edad) %>%
    summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Edad, y = Personas, fill = Sex)) +
    geom_col(width = 1, color = "white") +
    coord_flip() +
    scale_y_continuous(
      labels = abs,
      breaks = pretty(c(min(piramide_total$Personas, na.rm = TRUE), 
                        max(piramide_total$Personas, na.rm = TRUE)))
    ) +
    scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#e377c2")) +
    labs(
      title = paste("Population pyramid of the Province of León in", año),
      x = "Age",
      y = "People"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}
#
#Plot
p_2003 <- plot_piramide(2003)
p_2024 <- plot_piramide(2024)
#
p_2003
p_2024
#
# For g1 and g2 
ggplot(sancho_discr, aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(sancho_pyram$Personas), na.rm = TRUE),
                      max(abs(sancho_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Population pyramid in V. de D. Sancho (group 1)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
max_val <- max(abs(rey_discr$Personas))
#
ggplot(rey_discr, aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    limits = c(-max_val, max_val),
    name = "Number of people"
  )+
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Population pyramid in Valderry (group 2)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#############################
#
# Interpolation
#
# Men and women for 2003 and 2024 
piramide_h <- datos_combinados_th %>%
  filter(year %in% c(2003, 2024)) %>%
  mutate(Sex = "Male",
         Personas = -Personas)
#
piramide_m <- datos_combinados_tm %>%
  filter(year %in% c(2003, 2024)) %>%
  mutate(Sex = "Female")
#
piramide_total <- bind_rows(piramide_h, piramide_m)
#
plot_piramide_completa <- function(año) {
  df_year <- piramide_total %>%
    filter(year == año, !is.na(Edad), !is.na(Personas)) %>%  
    mutate(Edad = as.numeric(Edad)) %>%                     
    group_by(Sex, Edad) %>%
    summarise(Personas = sum(Personas, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_year, aes(x = Edad, y = Personas, fill = Sex)) +
    geom_col(alpha = 0.4, width = 1.5) +  
    geom_line(aes(color = Sex, group = Sex), size = 0.8) + 
    geom_point(aes(color = Sex), size = 1) +             
    geom_hline(yintercept = 0, color = "black", size = 0.3) + 
    coord_flip() +
    scale_y_continuous(
      labels = abs,
      breaks = pretty(c(min(df_year$Personas), max(df_year$Personas)))
    ) +
    scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#e377c2")) +
    scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#e377c2")) +
    labs(
      title = paste("Interpolated pyramid of the province of León in", año),
      x = "Age",
      y = "People"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}
#
p_2003 <- plot_piramide_completa(2003)
p_2024 <- plot_piramide_completa(2024)
#
p_2003
p_2024
#
################################### Maps ##############################
#
##### First necessary data ####
leon.map <- esp_get_munic_siane(region = "León") %>%
  mutate(Provincia = esp_dict_translate(ine.prov.name, "es")) %>%
  mutate(row_id = row_number()) %>%
  dplyr::select(cmun, name, geom) %>%
  slice(1:211)
#
rownames(leon.map) <- leon.map$row_id
#
leon.map$name==data.hist2024$municipio
#
munic <- esp_get_munic(region = "León") %>%
  # Datos de ejemplo: Población INE (solo tien 2019 en mapSpain)
  left_join(mapSpain::pobmun19, by = c("cpro", "cmun"))
#
munic <- munic %>%
  mutate(name.x = ifelse(name.x == "Candín", "Valle de Ancares", name.x))
#
##### First map about total population ###############
#
# join to perform the geometry
data.hist2024_sf <- left_join(data.hist2024, munic[, c("name.x", "geometry")], by =c("municipio"="name.x"))
#
# Cut every 100 inhab to obtain a better representaton of the situation of every municipality and to see groups
munic_bysize_24 <- data.hist2024_sf %>%
  mutate(
    pob_cat_auto = cut(
      Todas.las.edades,
      breaks = seq(0, max(Todas.las.edades, na.rm = TRUE) + 300, by = 300),
      include.lowest = TRUE,
      right = FALSE
    )
  )
#
# Define a gradient of colors
custom_colors <- c(
  "Menores de 300" = "#ffffcc",  # Amarillo muy claro
  "Entre 300 y 1000" = "#ffeda0", # Amarillo más intenso
  "Entre 1001 y 5000" = "#feb24c", # Naranja suave
  "Entre 5001 y 15000" = "#fd8d3c", # Naranja intenso
  "Mayores de 15001" = "#f03b20"   # Rojo intenso
)
#
# Convert to sf
munic_bysize_24 <- st_as_sf(munic_bysize_24)

# Plot
map1 <- ggplot(munic_bysize_24) +
  geom_sf(aes(fill = pob_cat_auto), alpha = 0.9, size = 0.2) +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(
    fill = "Population",
    title = "Population by municipalities in León (2024)",
    subtitle = "Cuts every 300 inhabitantes"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_blank(),  
    panel.background = element_blank(),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# pob_cat_auto as a factor
niveles <- levels(munic_bysize_24$pob_cat_auto)

# Choose first level, medium and last one
breaks_seleccionados <- c(
  niveles[1],
  niveles[floor(length(niveles)/3)],
  niveles[floor(2*length(niveles)/3)],
  niveles[length(niveles)]
)

# Map with simplified leyend
map1 <- ggplot(munic_bysize_24) +
  geom_sf(aes(fill = pob_cat_auto), alpha = 0.9, size = 0.2) +
  scale_fill_viridis_d(
    option = "plasma",
    direction = -1,
    name = "Population",
    breaks = breaks_seleccionados
  ) +
  labs(
    title = "Population by municipalities in León (2024)",
    subtitle = "Cuts every 300 inhabitants"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.background = element_blank(),
    panel.background = element_blank(),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

map1
#
######## Select some municipalities in different colors ########
munis_gris <- c(
  "Camponaraya", "Molinaseca", "Carracedelo", "Cabañas Raras",
  "León", "Cuadros", "San Andrés del Rabanedo", "Villaquilambre",
  "Valdefresno", "Onzonilla", "Sariegos", "Santovenia de la Valdoncina",
  "Valverde de la Virgen", "Villaturiel", "Ponferrada", "Garrafe de Torío", "Chozas de Abajo"
)

munis_rojo <- c(
  "Villablino", "Cistierna", "Pola de Gordón, La",
  "Fabero", "Robla, La", "Bembibre", "Astorga"
)

munis_verde <- c("Valencia de Don Juan", "Valdelugueros")

munic_bysize_24 <- munic_bysize_24 %>%
  mutate(color_group = case_when(
    municipio %in% munis_rojo ~ "rojo",
    municipio %in% munis_verde ~ "verde",
    municipio %in% munis_gris ~ "gris",
    TRUE ~ "blanco"
  ))

# Plot 
map_final <- ggplot(munic_bysize_24) +
  geom_sf(
    aes(fill = color_group),
    color = "black",   
    size = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "gris" = "grey80",
      "verde" = "#a1d99b",  
      "blanco" = "white",
      "rojo" = "#fcbba1"   
    ),
    guide = "none"
  ) +
  geom_sf_text(
    aes(label = municipio),
    size = 2,
    color = "black",
    check_overlap = TRUE
  ) +
  labs(
    title = "Indicated municipalities",
    subtitle = "Mentioned municipalities whith particularities"
  ) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

map_final
#
# Calculate 2003 and 2024 per municipality
pob_diff <- data.hist2 %>%
  filter(year %in% c(2003, 2024)) %>%
  dplyr::select(municipio, year, Todas.las.edades) %>%
  pivot_wider(names_from = year, values_from = Todas.las.edades, names_prefix = "pob_") %>%
  mutate(
    diferencia = pob_2024 - pob_2003,
    variacion_pct = 100 * ((pob_2024 - pob_2003) / pob_2003))
#
# Include geometry
pob_diff_sf <- left_join(pob_diff, munic[, c("name.x", "geometry")], by =c(  "municipio"="name.x")) %>%
  st_as_sf()
#
# Create categories 
pob_diff_sf <- pob_diff_sf %>%
  mutate(
    var_cat = cut(
      variacion_pct,
      breaks <- c(-Inf, -50, -40, -30, -20, -15, -10, -5, 0, 5, 10, 20, 30, Inf)
      ,
      labels <- c(
        "< -50%", "-50% to -40%", "-40% to -30%", "-30% to -20%", "-20% to -15%",
        "-15% to -10%", "-10% to -5%", "-5% to 0%",
        "0 to 5%", "5% to 10%", "10% to 20%", "20% to 30%", ">30%"),
      include.lowest = TRUE
    )
  )
#
# Define manual colors for each category
colores_var_cat <- c(
  "< -50%"        = "#0c0c0c",  # casi negro
  "-50% to -40%"  = "#67000d",  # rojo muy oscuro
  "-40% to -30%"  = "#a50f15",  # rojo intenso
  "-30% to -20%"  = "#cb181d",  # rojo fuerte
  "-20% to -15%"  = "#ef3b2c",  # rojo medio
  "-15% to -10%"  = "#fb6a4a",  # rojo claro
  "-10% to -5%"   = "#fc9272",  # salmón
  "-5% to 0%"     = "#fcbba1",  # rosado
  
  "0 to 5%"       = "#d9f0a3",  # verde muy claro
  "5% to 10%"     = "#addd8e",  # verde claro
  "10% to 20%"    = "#78c679",  # verde medio
  "20% to 30%"    = "#41ab5d",  # verde fuerte
  ">30%"          = "#005a32"   # verde muy oscuro
)
#
# Plot
map2_porcent_diff <- ggplot(pob_diff_sf) +
  geom_sf(aes(fill = var_cat), alpha = 0.9, size = 0.2) +
  scale_fill_manual(
    values = colores_var_cat,
    name = "% of variation"
  ) +
  labs(
    title = "Population growth (2003–2024)",
    fill = "% of variation"
  ) +
  theme_void() +
  theme(
    plot.background = element_blank(),  
    panel.background = element_blank() ,
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
#
# Create categories 
pob_diff_sf <- pob_diff_sf %>%
  mutate(
    var_abs = cut(
      diferencia,
      breaks <- c(-Inf, -7500, -4000, -1500, -1000, -750, -500,-250, 0, 250, 500, 750, 1000, 1500, 2500, 5000, Inf)
      ,
      labels <- c(
        "< -7.500", "-7.500 to -4.000",
        "-4.000 to -1.500", "-1.500 to -1.000", "-1.000 to -750", "-750 to -500",
        "-500 to -250", "-250 to 0", "0 to 250", "250 to 500", "500 to 750",
        "750 to 1.000", "1.000 to 1.500", "1.500 to 2.500", "2.500 to 5.000", "> 5.000"),
      include.lowest = TRUE
    )
  )
#
# Define manual colors for each category
colores_var_abs <- c(
  "< -7.500" = "#a50f15", "-7.500 to -4.000" = "#cb181d",  "-4.000 to -1.500"   = "#ef3b2c","-1.500 to -1.000"  = "#fc9272", "-1.000 to -750" = "#fcbba1", "-750 to -500" = "#fee0d2", "-500 to -250"  = "#fef0e0", "-250 to 0" = "#f7f7f7", "0 to 250" = "#e6f5d0","250 to 500" = "#c7e9b4","500 to 750" = "#a1d99b","750 to 1.000" = "#74c476", "1.000 to 1.500" = "#41ab5d", "1.500 to 2.500"  = "#238b45", "2.500 to 5.000"  = "#006d2c", "> 5.000"   = "#00441b")
#
# Plot
map3_diff <- ggplot(pob_diff_sf) +
  geom_sf(aes(fill = var_abs), alpha = 0.9, size = 0.2) +
  scale_fill_manual(
    values = colores_var_abs,
    name = "Absolute variation"
  ) +
  labs(
    title = "Total variation (2003–2024)",
    fill = "Abs"
  ) +
  theme_void() +
  theme(
    plot.background = element_blank(),  
    panel.background = element_blank(),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )
#
####### Map 4 per groups ########
#
mun.gr1$group <- "G1"
mun.gr2$group <- "G2"
mun.gr3$group <- "G3"
mun.gr4$group <- "G4"
mun.gr5$group <- "G5"
#
mun.gr.todos <- bind_rows(mun.gr1, mun.gr2, mun.gr3, mun.gr4, mun.gr5)
#
munic <- munic %>%
  left_join(mun.gr.todos, by=c("name.x"="municipio"))
#
map4_groups <- ggplot(munic) +
  geom_sf(aes(fill = group), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Set2", na.value = "grey90") + 
  labs(title = "Municipalities by groups",
       fill = "Group") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_blank(),
    panel.background = element_blank(),
  )
#
######## Map according to clustering #########
#
rownames(df_clusters) <- NULL
df_clusters <- df_clusters %>%
  rename(name.x = Municipio)
munic_clustered <- munic %>%
  left_join(df_clusters, by = "name.x")
#
map5_clusters <- ggplot(munic_clustered) +
  geom_sf(aes(fill = as.factor(Cluster)), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("1" = "#E57373",  
               "2" = "#FFCC80",  
               "3" = "#81C784",  
               "4" = "#64B5F6", 
               "5" = "#BA68C8"), 
    na.value = "grey90"
  ) +  
  labs(title = "Municipalities by clusters according to functional k-means",
       fill = "Group") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_blank(),  
    panel.background = element_blank()
  )
map5_clusters

#
####### Map marking depths#########
#
municipios_destacados <- c(
  "Villamartín de Don Sancho",
  "Valencia de Don Juan",
  "Congosto",
  "Valderrey",
  "León"
)
#
munic_bysize_24 <- munic_bysize_24 %>%
  mutate(
    destacado = ifelse(municipio %in% municipios_destacados, "Destacado", "Otros")
  )
#
mapa_resaltado <- ggplot(munic_bysize_24) +
  geom_sf(aes(fill = destacado), color = "grey50", size = 0.2) +
  scale_fill_manual(
    values = c("Destacado" = "#f03b20", "Otros" = "white"),
    guide = "none" 
  ) +
  labs(
    title = "Deepest municipalities in each group"
  ) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
mapa_resaltado
#
######################## Smooth pyramids at start #######################
#
# Obtain data for the depth of each group in 2003 and 2024 for men and women
#
# Group 1
#
# Women 2024
depth_gr1m <- wom_gr1_agrupado %>%
  filter(year == 2024, municipio == "Villamartín de Don Sancho") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
x <- depth_gr1m$Edad_central
y <- depth_gr1m$Personas
fdParobj <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

funct_smooth_1m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_1m <- data.frame(
  Edad_central = edad_fina_grp1,
  Personas = eval.fd(edad_fina_grp1, funct_smooth_1m$fd),
  municipio = "Villamartín de Don Sancho",
  year = 2024)%>%
  mutate(Sex="Female")
#
#Men 2024
#
depth_gr1h <- men_gr1_agrupado %>%
  filter(year == 2024, municipio == "Villamartín de Don Sancho") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
# Smooth
x <- depth_gr1h$Edad_central
y <- depth_gr1h$Personas
fdParobj <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

funct_smooth_1h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_1h <- data.frame(
  Edad_central = edad_fina_grp1,
  Personas = eval.fd(edad_fina_grp1, funct_smooth_1h$fd),
  municipio = "Villamartín de Don Sancho",
  year = 2024)%>%
  mutate(Sex="Male")
#
# Women 2003
depth_gr1m03 <- wom_gr1_agrupado %>%
  filter(year == 2003, municipio == "Villamartín de Don Sancho") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
x <- depth_gr1m03$Edad_central
y <- depth_gr1m03$Personas
fdParobj <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

funct_smooth_1m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_1m03 <- data.frame(
  Edad_central = edad_fina_grp1,
  Personas = eval.fd(edad_fina_grp1, funct_smooth_1m03$fd),
  municipio = "Villamartín de Don Sancho",
  year = 2003) %>%
  mutate(Sex="Female")
#
#Men 2003
#
depth_gr1h03 <- men_gr1_agrupado %>%
  filter(year == 2003, municipio == "Villamartín de Don Sancho") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
# Smooth
x <- depth_gr1h03$Edad_central
y <- depth_gr1h03$Personas
fdParobj <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

funct_smooth_1h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_1h03 <- data.frame(
  Edad_central = edad_fina_grp1,
  Personas = eval.fd(edad_fina_grp1, funct_smooth_1h03$fd),
  municipio = "Villamartín de Don Sancho",
  year = 2003) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_depth_1 <- bind_rows(smooth_depth_1h,smooth_depth_1m,smooth_depth_1h03,smooth_depth_1m03)
#
# Group 2
#
# Women 2024
depth_gr2m <- wom_gr2_agrupado %>%
  filter(year == 2024, municipio == "Valderrey") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
x <- depth_gr2m$Edad_central
y <- depth_gr2m$Personas
fdParobj <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

funct_smooth_2m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_2m <- data.frame(
  Edad_central = edad_fina_grp2,
  Personas = eval.fd(edad_fina_grp2, funct_smooth_2m$fd),
  municipio = "Valderrey",
  year = 2024)%>%
  mutate(Sex="Female")
#
#Men 2024
#
depth_gr2h <- men_gr2_agrupado %>%
  filter(year == 2024, municipio == "Valderrey") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
# Smooth
x <- depth_gr2h$Edad_central
y <- depth_gr2h$Personas
fdParobj <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

funct_smooth_2h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_2h <- data.frame(
  Edad_central = edad_fina_grp2,
  Personas = eval.fd(edad_fina_grp2, funct_smooth_2h$fd),
  municipio = "Valderrey",
  year = 2024)%>%
  mutate(Sex="Male")
#
# Women 2003
depth_gr2m03 <- wom_gr2_agrupado %>%
  filter(year == 2003, municipio == "Valderrey") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
x <- depth_gr2m03$Edad_central
y <- depth_gr2m03$Personas
fdParobj <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

funct_smooth_2m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_2m03 <- data.frame(
  Edad_central = edad_fina_grp2,
  Personas = eval.fd(edad_fina_grp2, funct_smooth_2m03$fd),
  municipio = "Valderrey",
  year = 2003) %>%
  mutate(Sex="Female")
#
#Men 2003
#
depth_gr2h03 <- men_gr2_agrupado %>%
  filter(year == 2003, municipio == "Valderrey") %>%
  filter(!is.na(Edad_central) & !is.na(Personas)) %>%
  arrange(Edad_central)
#
# Smooth
x <- depth_gr2h03$Edad_central
y <- depth_gr2h03$Personas
fdParobj <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

funct_smooth_2h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_2h03 <- data.frame(
  Edad_central = edad_fina_grp2,
  Personas = eval.fd(edad_fina_grp2, funct_smooth_2h03$fd),
  municipio = "Valderrey",
  year = 2003) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_depth_2 <- bind_rows(smooth_depth_2h,smooth_depth_2m,smooth_depth_2h03,smooth_depth_2m03)
#
# Group 3
#
# Women 2024
depth_gr3m <- wom_gr3 %>%
  filter(year == 2024, municipio == "Congosto") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr3m$Edad
y <- depth_gr3m$Personas
fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

funct_smooth_3m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_3h <- data.frame(
  Edad = edad_fina_grp3,
  Personas = pmax(eval.fd(edad_fina_grp3, funct_smooth_3h$fd), 0),
  municipio = "Congosto",
  year = 2024
) %>%
  mutate(Sex="Male")
#
#Men 2024
#
depth_gr3h <- men_gr3 %>%
  filter(year == 2024, municipio == "Congosto") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr3h$Edad
y <- depth_gr3h$Personas
fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

funct_smooth_3h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_3h <- data.frame(
  Edad = edad_fina_grp3,
  Personas = pmax(eval.fd(edad_fina_grp3, funct_smooth_3h$fd), 0),
  municipio = "Congosto",
  year = 2024
) %>%
  mutate(Sex="Male")
#
# Women 2003
depth_gr3m03 <- wom_gr3 %>%
  filter(year == 2003, municipio == "Congosto") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr3m03$Edad
y <- depth_gr3m03$Personas
fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

funct_smooth_3m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_3m03 <- data.frame(
  Edad = edad_fina_grp3,
  Personas = pmax(eval.fd(edad_fina_grp3, funct_smooth_3m03$fd), 0),
  municipio = "Congosto",
  year = 2003
) %>%
  mutate(Sex="Female")
#
#Men 2003
#
depth_gr3h03 <- men_gr3 %>%
  filter(year == 2003, municipio == "Congosto") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr3h03$Edad
y <- depth_gr3h03$Personas
fdParobj <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

funct_smooth_3h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_3h03 <- data.frame(
  Edad = edad_fina_grp3,
  Personas = pmax(eval.fd(edad_fina_grp3, funct_smooth_3h03$fd), 0),
  municipio = "Congosto",
  year = 2003
) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_depth_3 <- bind_rows(smooth_depth_3h,smooth_depth_3m,smooth_depth_3h03,smooth_depth_3m03)
#
# Group 4
#
# Women 2024
depth_gr4m <- wom_gr4 %>%
  filter(year == 2024, municipio == "Valencia de Don Juan") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr4m$Edad
y <- depth_gr4m$Personas
fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

funct_smooth_4m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_4m <- data.frame(
  Edad = edad_fina_grp4,
  Personas = eval.fd(edad_fina_grp4, funct_smooth_4m$fd),
  municipio = "Valencia de Don Juan",
  year = 2024)%>%
  mutate(Sex="Female")
#
#Men 2024
#
depth_gr4h <- men_gr4 %>%
  filter(year == 2024, municipio == "Valencia de Don Juan") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr4h$Edad
y <- depth_gr4h$Personas
fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

funct_smooth_4h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_4h <- data.frame(
  Edad = edad_fina_grp4,
  Personas = eval.fd(edad_fina_grp4, funct_smooth_4h$fd),
  municipio = "Valencia de Don Juan",
  year = 2024)%>%
  mutate(Sex="Male")
#
# Women 2003
depth_gr4m03 <- wom_gr4 %>%
  filter(year == 2003, municipio == "Valencia de Don Juan") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr4m03$Edad
y <- depth_gr4m03$Personas
fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

funct_smooth_4m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_4m03 <- data.frame(
  Edad = edad_fina_grp4,
  Personas = eval.fd(edad_fina_grp4, funct_smooth_4m03$fd),
  municipio = "Valencia de Don Juan",
  year = 2003) %>%
  mutate(Sex="Female")
#
#Men 2003
#
depth_gr4h03 <- men_gr4 %>%
  filter(year == 2003, municipio == "Valencia de Don Juan") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr4h03$Edad
y <- depth_gr4h03$Personas
fdParobj <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

funct_smooth_4h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_4h03 <- data.frame(
  Edad = edad_fina_grp4,
  Personas = eval.fd(edad_fina_grp4, funct_smooth_4h03$fd),
  municipio = "Valencia de Don Juan",
  year = 2003) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_depth_4 <- bind_rows(smooth_depth_4h,smooth_depth_4m,smooth_depth_4h03,smooth_depth_4m03)
#
# Group 5
#
# Women 2024
depth_gr5m <- wom_gr5 %>%
  filter(year == 2024, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr5m$Edad
y <- depth_gr5m$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5m <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5m$fd),
  municipio = "León",
  year = 2024)%>%
  mutate(Sex="Female")
#
#Men 2024
#
depth_gr5h <- men_gr5 %>%
  filter(year == 2024, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr5h$Edad
y <- depth_gr5h$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

funct_smooth_5h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5h <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, funct_smooth_5h$fd),
  municipio = "León",
  year = 2024)%>%
  mutate(Sex="Male")
#
# Women 2003
depth_gr5m03 <- wom_gr5 %>%
  filter(year == 2003, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- depth_gr5m03$Edad
y <- depth_gr5m03$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5m03 <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5m03$fd),
  municipio = "León",
  year = 2003) %>%
  mutate(Sex="Female")
#
#Men 2003
#
depth_gr5h03 <- men_gr5 %>%
  filter(year == 2003, municipio == "León") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- depth_gr5h03$Edad
y <- depth_gr5h03$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

funct_smooth_5h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_depth_5h03 <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, funct_smooth_5h03$fd),
  municipio = "León",
  year = 2003) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_depth_5 <- bind_rows(smooth_depth_5h,smooth_depth_5m,smooth_depth_5h03,smooth_depth_5m03)
#
########## Each deepest municipality pyramids ##########

# Preparate men and women with 2003 and 2024
sancho_pyram_h03 <- smooth_depth_1 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
sancho_pyram_m03 <- smooth_depth_1 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
sancho_pyram_h24 <- smooth_depth_1 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
sancho_pyram_m24 <- smooth_depth_1 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
# Joint datasets
sancho_pyram <- bind_rows(sancho_pyram_h03, sancho_pyram_m03, sancho_pyram_h24, sancho_pyram_m24)
#
# Plot
ggplot(sancho_pyram, aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(sancho_pyram$Personas), na.rm = TRUE),
                      max(abs(sancho_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in V. de D. Sancho (group 1)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
##########
#
rey_pyram_h03 <- smooth_depth_2 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
rey_pyram_m03 <- smooth_depth_2 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
rey_pyram_h24 <- smooth_depth_2 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
rey_pyram_m24 <- smooth_depth_2 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
# Joint datasets
rey_pyram <- bind_rows(rey_pyram_h03, rey_pyram_m03, rey_pyram_h24, rey_pyram_m24)
#
ggplot(rey_pyram, aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(rey_pyram$Personas), na.rm = TRUE),
                      max(abs(rey_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in Valderrey (group 2)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
##########
#   
cong_pyram_h03 <- smooth_depth_3 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
cong_pyram_m03 <- smooth_depth_3 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
cong_pyram_h24 <- smooth_depth_3 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
cong_pyram_m24 <- smooth_depth_3 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
cong_pyram <- bind_rows(cong_pyram_h03, cong_pyram_m03, cong_pyram_h24, cong_pyram_m24)
#
ggplot(cong_pyram, aes(x = Edad, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(cong_pyram$Personas), na.rm = TRUE),
                      max(abs(cong_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in Congosto (group 3)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
##########
#
vddj_pyram_h03 <- smooth_depth_4 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
vddj_pyram_m03 <- smooth_depth_4 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
vddj_pyram_h24 <- smooth_depth_4 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
vddj_pyram_m24 <- smooth_depth_4 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
vddj_pyram <- bind_rows(vddj_pyram_h03, vddj_pyram_m03, vddj_pyram_h24, vddj_pyram_m24)
#
ggplot(vddj_pyram, aes(x = Edad, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(vddj_pyram$Personas), na.rm = TRUE),
                      max(abs(vddj_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in Valencia de D. Juan (group 4)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
######
#
le_pyram_h03 <- smooth_depth_5 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
le_pyram_m03 <- smooth_depth_5 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
le_pyram_h24 <- smooth_depth_5 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
le_pyram_m24 <- smooth_depth_5 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
le_pyram <- bind_rows(le_pyram_h03, le_pyram_m03, le_pyram_h24, le_pyram_m24)
#
ggplot(le_pyram, aes(x = Edad, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(le_pyram$Personas), na.rm = TRUE),
                      max(abs(le_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in León (group 5)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
######
#
#
# Same population pyramids with discrete points
#
# Discrete points
#
sancho_discr_h03 <- men_gr1_agrupado %>%
  filter(municipio=="Villamartín de Don Sancho") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
sancho_discr_h24 <- men_gr1_agrupado %>%
  filter(municipio=="Villamartín de Don Sancho") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
sancho_discr_m03 <- wom_gr1_agrupado %>%
  filter(municipio=="Villamartín de Don Sancho") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
sancho_discr_m24 <- wom_gr1_agrupado %>%
  filter(municipio=="Villamartín de Don Sancho") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
sancho_discr <- bind_rows(sancho_discr_h03,sancho_discr_h24,sancho_discr_m24,sancho_discr_m03)
sancho_discr <- sancho_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +
  geom_line(data = sancho_pyram, 
            aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  geom_point(data = sancho_discr, 
             aes(x = Edad_central, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(sancho_pyram$Personas, sancho_discr$Personas)), na.rm = TRUE),
                      max(abs(c(sancho_pyram$Personas, sancho_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Smoothed population pyramid + raw data - V. de D. Sancho (2003 vs 2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw data)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#
rey_discr_h03 <- men_gr2_agrupado %>%
  filter(municipio=="Valderrey") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
rey_discr_h24 <- men_gr2_agrupado %>%
  filter(municipio=="Valderrey") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
rey_discr_m03 <- wom_gr2_agrupado %>%
  filter(municipio=="Valderrey") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
rey_discr_m24 <- wom_gr2_agrupado %>%
  filter(municipio=="Valderrey") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
rey_discr <- bind_rows(rey_discr_h03,rey_discr_h24,rey_discr_m24,rey_discr_m03)
rey_discr <- rey_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +

  geom_line(data = rey_pyram, 
            aes(x = Edad_central, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +

  geom_point(data = rey_discr, 
             aes(x = Edad_central, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(rey_pyram$Personas, rey_discr$Personas)), na.rm = TRUE),
                      max(abs(c(rey_pyram$Personas, rey_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Population pyramid + raw data - Valderrey (2003 vs 2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#
cong_discr_h03 <- men_gr3 %>%
  filter(municipio=="Congosto") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
cong_discr_h24 <- men_gr3 %>%
  filter(municipio=="Congosto") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
cong_discr_m03 <- wom_gr3 %>%
  filter(municipio=="Congosto") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
cong_discr_m24 <- wom_gr3 %>%
  filter(municipio=="Congosto") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
cong_discr <- bind_rows(cong_discr_h03,cong_discr_h24,cong_discr_m24,cong_discr_m03)
cong_discr <- cong_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +

  geom_line(data = cong_pyram, 
            aes(x = Edad, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +

  geom_point(data = cong_discr, 
             aes(x = Edad, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(cong_pyram$Personas, cong_discr$Personas)), na.rm = TRUE),
                      max(abs(c(cong_pyram$Personas, cong_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Population pyramid + raw data - Congosto (2003-2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#
vddj_discr_h03 <- men_gr4 %>%
  filter(municipio=="Valencia de Don Juan") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
vddj_discr_h24 <- men_gr4 %>%
  filter(municipio=="Valencia de Don Juan") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
vddj_discr_m03 <- wom_gr4 %>%
  filter(municipio=="Valencia de Don Juan") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
vddj_discr_m24 <- wom_gr4  %>%
  filter(municipio=="Valencia de Don Juan") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
vddj_discr <- bind_rows(vddj_discr_h03,vddj_discr_h24,vddj_discr_m24,vddj_discr_m03)
vddj_discr <- vddj_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +

  geom_line(data = vddj_pyram, 
            aes(x = Edad, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +

  geom_point(data = vddj_discr, 
             aes(x = Edad, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(vddj_pyram$Personas, vddj_discr$Personas)), na.rm = TRUE),
                      max(abs(c(vddj_pyram$Personas, vddj_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Population pyramid + raw data - V. D. Juan (2003-2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#
le_discr_h03 <- men_gr5 %>%
  filter(municipio=="León") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
le_discr_h24 <- men_gr5 %>%
  filter(municipio=="León") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
le_discr_m03 <- wom_gr5 %>%
  filter(municipio=="León") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
le_discr_m24 <- wom_gr5  %>%
  filter(municipio=="León") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
le_discr <- bind_rows(le_discr_h03,le_discr_h24,le_discr_m24,le_discr_m03)
le_discr <- le_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +
  geom_line(data = le_pyram, 
            aes(x = Edad, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  geom_point(data = le_discr, 
             aes(x = Edad, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(le_pyram$Personas, le_discr$Personas)), na.rm = TRUE),
                      max(abs(c(le_pyram$Personas, le_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Population pyramid + raw data - León (2003-2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
#
########## Ponferrada #####
#
# Women 2024
ponfe_gr5m <- wom_gr5 %>%
  filter(year == 2024, municipio == "Ponferrada") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- ponfe_gr5m$Edad
y <- ponfe_gr5m$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5m <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_ponfe_5m <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5m$fd),
  municipio = "Ponferrada",
  year = 2024)%>%
  mutate(Sex="Female")
#
#Men 2024
#
ponfe_gr5h <- men_gr5 %>%
  filter(year == 2024, municipio == "Ponferrada") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- ponfe_gr5h$Edad
y <- ponfe_gr5h$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5h <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_ponfe_5h <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5h$fd),
  municipio = "Ponferrada",
  year = 2024)%>%
  mutate(Sex="Male")
#
# Women 2003
ponfe_gr5m03 <- wom_gr5 %>%
  filter(year == 2003, municipio == "Ponferrada") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
x <- ponfe_gr5m03$Edad
y <- ponfe_gr5m03$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5m03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_ponfe_5m03 <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5m03$fd),
  municipio = "Ponferrada",
  year = 2003) %>%
  mutate(Sex="Female")
#
#Men 2003
#
ponfe_gr5h03 <- men_gr5 %>%
  filter(year == 2003, municipio == "Ponferrada") %>%
  filter(!is.na(Edad) & !is.na(Personas)) %>%
  arrange(Edad)
#
# Smooth
x <- ponfe_gr5h03$Edad
y <- ponfe_gr5h03$Personas
fdParobj <- fdPar(basis5, Lfdobj = int2Lfd(2), lambda = lambda5)

ponfe.funct_smooth_5h03 <- tryCatch({
  smooth.basis(x, y, fdParobj)
}, error = function(e) return(NULL))

smooth_ponfe_5h03 <- data.frame(
  Edad = edad_fina_grp5,
  Personas = eval.fd(edad_fina_grp5, ponfe.funct_smooth_5h03$fd),
  municipio = "Ponferrada",
  year = 2003) %>%
  mutate(Sex="Male")
#
# Joint 2003 and 2024
#
smooth_ponfe_5 <- bind_rows(smooth_ponfe_5h,smooth_ponfe_5m,smooth_ponfe_5h03,smooth_ponfe_5m03)
#
#
# Men and women for 2003 and 2024
ponfe_pyram_h03 <- smooth_ponfe_5 %>%
  filter(year==2003) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")# Pop pyramid men (-)
#
ponfe_pyram_m03 <- smooth_ponfe_5 %>%
  filter(year==2003) %>%
  filter(Sex == "Female")
#
ponfe_pyram_h24 <- smooth_ponfe_5 %>%
  filter(year==2024) %>%
  mutate(Personas = -Personas)%>%
  filter(Sex=="Male")
#
ponfe_pyram_m24 <- smooth_ponfe_5 %>%
  filter(year==2024) %>%
  filter(Sex == "Female")
#
# Join datasets
ponfe_pyram <- bind_rows(ponfe_pyram_h03, ponfe_pyram_m03, ponfe_pyram_h24, ponfe_pyram_m24)
#
ggplot(ponfe_pyram, aes(x = Edad, y = Personas, color = Sex, linetype = factor(year))) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(ponfe_pyram$Personas), na.rm = TRUE),
                      max(abs(ponfe_pyram$Personas), na.rm = TRUE))),
    name = "Number of people"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  labs(
    title = "Smoothed population pyramid in Ponferrada (group 5)",
    x = "Age",
    color = "Sex",
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
# Discrete poitns
#
ponfe_discr_h03 <- men_gr5 %>%
  filter(municipio=="Ponferrada") %>%
  filter(year==2003) %>%
  mutate(Sex="Male")
ponfe_discr_h24 <- men_gr5 %>%
  filter(municipio=="Ponferrada") %>%
  filter(year==2024) %>%
  mutate(Sex="Male")
ponfe_discr_m03 <- wom_gr5 %>%
  filter(municipio=="Ponferrada") %>%
  filter(year==2003) %>%
  mutate(Sex="Female")
ponfe_discr_m24 <- wom_gr5  %>%
  filter(municipio=="Ponferrada") %>%
  filter(year==2024) %>%
  mutate(Sex="Female")
ponfe_discr <- bind_rows(ponfe_discr_h03,ponfe_discr_h24,ponfe_discr_m24,ponfe_discr_m03)
ponfe_discr <- ponfe_discr %>%
  mutate(Personas = ifelse(Sex == "Male", -Personas, Personas))

ggplot() +

  geom_line(data = ponfe_pyram, 
            aes(x = Edad, y = Personas, color = Sex, linetype = factor(year)),
            size = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +

  geom_point(data = ponfe_discr, 
             aes(x = Edad, y = Personas, color = Sex, shape = factor(year)),
             size = 1, alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-max(abs(c(ponfe_pyram$Personas, ponfe_discr$Personas)), na.rm = TRUE),
                      max(abs(c(ponfe_pyram$Personas, ponfe_discr$Personas)), na.rm = TRUE))),
    name = "Número de personas"
  ) +
  scale_color_manual(values = c("Male" = "cyan4", "Female" = "#CD2626")) +
  scale_linetype_manual(values = c("2003" = "solid", "2024" = "dashed")) +
  scale_shape_manual(values = c("2003" = 16, "2024" = 17)) +  
  labs(
    title = "Population pyramid + raw data - Ponferrada (2003 vs 2024)",
    x = "Age",
    color = "Sex",
    linetype = "Year (smooth)",
    shape = "Year (raw)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
#
#
###################### Pyram evolution for deepest munic #######################
#
#
# Parameters
lambda.leon.city <- 1e-02
n_basis <- 18  # ajusta según necesidad
age_range <- range(leon.city$Edad, na.rm = TRUE)
basis.city.leon <- create.bspline.basis(rangeval = age_range, nbasis = n_basis)
fdParobj.leon.city <- fdPar(basis.city.leon, Lfdobj = int2Lfd(2), lambda = lambda.leon.city)

# Create grid same age
edad_grid <- seq(age_range[1], age_range[2], length.out = 101)

# List to keep smoothed results
curvas_suavizadas <- list()

# Loop per year 
for (año in unique(leon.city$year)) {
  datos_año <- leon.city %>% filter(year == año)
  
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.leon.city
  )$fd
  
  personas_suavizadas <- eval.fd(edad_grid, smooth_fd)
  
  df_suavizado <- data.frame(
    Edad = edad_grid,
    Personas = personas_suavizadas,
    year = año
  )
  
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

# Join everything in a single dataframe
df_todo <- bind_rows(curvas_suavizadas)

# Plot
ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") 
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in León (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
#
# Parameters
# Obtain ages unique and range for B-splines
edad_fina_grp3 
age_range_grp3 
n_basis3 
basis3 
lambda3   
fd_obj_3
fdParobj.gr3 <- fdPar(basis3, Lfdobj = int2Lfd(2), lambda = lambda3)

curvas_suavizadas <- list()

for (año in unique(tot_gr3$year)) {
  datos_año <- tot_gr3 %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr3
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp3, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp3,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Congosto, G3 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
#
# 
edad_fina_grp4 
age_range_grp4 
n_basis4 
basis4 
lambda4 
fd_obj_4
fdParobj.gr4 <- fdPar(basis4, Lfdobj = int2Lfd(2), lambda = lambda4)

curvas_suavizadas <- list()

for (año in unique(tot_gr4$year)) {
  datos_año <- tot_gr4 %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr4
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp4, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp4,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Valendia de D. Juan, G4 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
#
# 
edad_fina_grp1 
age_range_grp1 
n_basis1 
basis1 
lambda1   
fd_obj_1
fdParobj.gr1 <- fdPar(basis1, Lfdobj = int2Lfd(2), lambda = lambda1)

curvas_suavizadas <- list()

for (año in unique(tot_gr1_agrupado$year)) {
  datos_año <- tot_gr1_agrupado %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad_central,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr1
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp1, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp1,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Villamartín de D. Sancho, G1 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
#
# 
edad_fina_grp2 
age_range_grp2 
n_basis2 
basis2 
lambda2  
fd_obj_2
fdParobj.gr2 <- fdPar(basis2, Lfdobj = int2Lfd(2), lambda = lambda2)

curvas_suavizadas <- list()

for (año in unique(tot_gr2_agrupado$year)) {
  datos_año <- tot_gr2_agrupado %>% filter(year == año)
  smooth_fd <- smooth.basis(
    argvals = datos_año$Edad_central,
    y = datos_año$Personas,
    fdParobj = fdParobj.gr2
  )$fd
  personas_suavizadas <- eval.fd(edad_fina_grp2, smooth_fd)
  df_suavizado <- data.frame(
    Edad = edad_fina_grp2,
    Personas = personas_suavizadas,
    year = año
  )
  curvas_suavizadas[[as.character(año)]] <- df_suavizado
}

df_todo <- bind_rows(curvas_suavizadas)

ggplot(df_todo, aes(x = Edad, y = Personas, color = year, group = year)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Año") +  # o cualquier otra paleta
  theme_minimal() +
  labs(
    title = "Evolution of the distribution of population by age in Valderrey, G2 (2003–2024)",
    x = "Age", y = "Number of people"
  )
#
#
############################## Outliers ########################################
#
#G1
#
ms1 <- msplot(dts = suave_df1_matriz, return_mvdir = TRUE, plot = TRUE)
#
ggplot(long_df1, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal()
#
# G2
#
ms2 <- msplot(dts = suave_df2_matriz, return_mvdir = TRUE, plot = TRUE)
#
ggplot(long_df2, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal()
#
# G3
ms3 <- msplot(dts = suave_df3_matriz, return_mvdir = TRUE, plot = TRUE)
#
ggplot(long_df3, aes(x = edad, y = personas, group = id)) +
  geom_line(aes(color = tipo), size = 0.6, alpha = 0.8) +
  scale_color_manual(values = c("normal" = "gray70", "outlier" = "red")) +
  labs(title = "Smoothed population curves with detected functional outliers",
       x = "Age", y = "Population",
       color = "Type") +
  theme_minimal() 



