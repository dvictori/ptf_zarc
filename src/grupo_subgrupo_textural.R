# Funções para classificar amostras nos grupos / subgrupos texturais do SIBCS
# Daniel de Castro Victoria
# 20 ago 2020

# classifica o grupo textural
# de acordo com SIBCS 2018
# são 5 grupos (p. 52)
grupo_textural <- function(sand, silt, clay) {
  classes <- factor(c('Arenosa', 'Média', 'Siltosa', 'Argilosa', 'Muito argilosa', 'NA'),
                    levels = c('Arenosa', 'Média', 'Siltosa', 'Argilosa', 'Muito argilosa', 'NA'))
  
  check <- trunc(sand + silt + clay)
  
  ids <- ifelse(check > 100 | check < 95, 9,
                ifelse(clay > 60, 5,
                       ifelse(clay > 35, 4, 
                              ifelse(clay < 35 & sand < 15, 3,
                                     ifelse(sand - clay > 70, 1, 2)))))
  return(classes[ids])
}

# classifica o subgrupo textural
# de acordo com SIBCS 2018
# são 8 grupos (p. 373)
# regras para algumas classes vem do Soil Survey Manual,  (p.108)
subgrupo_textural <- function(sand, silt, clay) {
  classes <- factor(c('Muito arenosa', 'Arenosa-média', 'Média-arenosa', 'Média-argilosa',
                      'Média-siltosa', 'Siltosa', 'Argilosa', 'Muito argilosa', NA),
                    levels = c('Muito arenosa', 'Arenosa-média', 'Média-arenosa', 'Média-argilosa',
                               'Média-siltosa', 'Siltosa', 'Argilosa', 'Muito argilosa', NA))
  
  check <- trunc(sand + silt + clay)
  
  ids <- ifelse(check > 100 | check < 95, 9,
                ifelse(clay > 60, 8,
                       ifelse(clay > 35, 7, 
                              ifelse(clay < 35 & sand < 15, 6,
                                     ifelse(sand > 85 & silt + 1.5*clay < 15, 1,
                                            ifelse(sand - clay > 70, 2,
                                                   ifelse(sand > 45 & silt < 28 & clay >= 20 & clay <= 35, 4, 
                                                          ifelse(sand >= 52, 3, 5))))))))
  return(classes[ids])
}
