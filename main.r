library(readr)
# Ouverture des fichiers CSV

validation_1_trim_2025 = read.csv("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv", sep = ";")
validation_2_trim_2025 = read.csv("data/validations-reseau-ferre-nombre-validations-par-jour-2eme-trimestre.csv", sep = ";")


View(head(validation_1_trim_2025))
View(head(validation_2_trim_2025))


#Ouverture des fichier txt
#2018
validation_1_trim_2018 = read.delim("data/data-rf-2018/2018_S1_NB_FER.txt")
validation_2_trim_2018 = read.delim("data/data-rf-2018/2018_S2_NB_FER.txt")

#2019
validation_1_trim_2019 = read.delim("data/data-rf-2019/2019_S1_NB_FER.txt")
validation_2_trim_2019 = read.delim("data/data-rf-2019/2019_S2_NB_FER.txt")

#2020
validation_1_trim_2020 = read.delim("data/data-rf-2020/2020_S1_NB_FER.txt")
validation_2_trim_2020 = read.delim("data/data-rf-2020/2020_S2_NB_FER.txt")

#2021
validation_1_trim_2021 = read.delim("data/data-rf-2021/2021_S1_NB_FER.txt")
validation_2_trim_2021 = read.delim("data/data-rf-2021/2021_S2_NB_FER.txt")

#2022
validation_1_trim_2022 = read.delim("data/data-rf-2022/2022_S1_NB_FER.txt")
validation_2_trim_2022 = read.delim("data/data-rf-2022/2022_S2_NB_FER.txt")

#2023
validation_1_trim_2023 = read.delim("data/data-rf-2023/2023_S1_NB_FER.txt")
validation_2_trim_2023 = read.delim("data/data-rf-2023/2023_S2_NB_FER.txt")
# validation_2_trim_2023 = read_delim("data/data-rf-2023/2023_S2_NB_FER.txt", delim = "\t")

#2024
validation_1_trim_2024 = read.delim("data/data-rf-2024/2024_S1_NB_FER.txt")
validation_2_trim_2024 = read.delim("data/data-rf-2024/2024_S2_NB_FER.txt")


