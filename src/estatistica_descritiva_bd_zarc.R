# Estatísticas básicas do conjunto de dados principal

library(tidyverse)
library(Ternary)
source('src/grupo_subgrupo_textural.R')

# Carregandos dados da base PTF
dados <- read.csv2("dados/bd_zarc.csv")
dados$AD <- dados$AD * 10

# Conferindo se a soma dos dados de granulometria dão 100 (100%)
soma <- dados$AT + dados$SIL + dados$ARG
max(soma) # valor maximo da serie de somas
min(soma) # valor minimo da serie de somas

dados$grupo <- grupo_textural(dados$AT, dados$SIL, dados$ARG)
dados$subgrupo <- subgrupo_textural(dados$AT, dados$SIL, dados$ARG)

summary(dados)

ggplot(dados) +
  geom_density(aes(x = AT)) +
  labs(x = 'Areia Total [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_at.png')

ggplot(dados) +
  geom_density(aes(x = SIL)) +
  labs(x = 'Silte [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_sil.png')

ggplot(dados) +
  geom_density(aes(x = ARG)) +
  labs(x = 'Argila [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_arg.png')

ggplot(dados) +
  geom_density(aes(x = AD)) +
  labs(x = 'Água disponível - AD - [mm cm⁻¹]', y = 'FDP Kernel') +
  geom_vline(aes(xintercept = median(AD))) +
  geom_vline(aes(xintercept = quantile(AD, 0.25)), lty = 'dashed') +
  geom_vline(aes(xintercept = quantile(AD, 0.75)), lty = 'dashed')
ggsave('figs/distribuicao_AD.png')

dados_long <- dados %>%
  pivot_longer(c(AT, SIL, ARG),
               names_to = 'fração') %>%
  mutate(fração = factor(fração, levels = c('AT', 'SIL', 'ARG'),
                         labels = c('Areia Total', 'Silte', 'Argila')))

ggplot(dados_long) +
  geom_density(aes(x = value, fill = fração), alpha = 0.4) +
  labs(x = 'porcento', y = 'FDP Kernel', fill = 'Granulometria')
ggsave('figs/distribuicao_fracoes_over.png')

ggplot(dados_long) +
  geom_density(aes(x = value)) +
  facet_wrap(~fração) +
  labs(x = 'Porcento', y = 'FDP Kernel')
ggsave('figs/distribuicao_fracoes_grade.png')

ggplot(dados_long) +
  geom_boxplot(aes(x = fração, y = value)) +
  labs(x = 'Frações texturais', y = 'Porcento')
ggsave('figs/boxplot_fracoes.png')

resumo <- dados %>%
  group_by(subgrupo) %>%
  summarise(mediana = round(median(AD),2),
            obs = n())

ggplot(dados) +
  geom_boxplot(aes(x = subgrupo, y = AD)) +
  geom_text(data = resumo,
            aes(x = subgrupo, y = mediana + 0.08, label = mediana)) +
  geom_text(data = resumo,
            aes(x = subgrupo, y = 2.6, label = paste0('(', obs, ')'))) +
  labs(x = 'Subgrupo textural', y = 'AD [mm cm⁻¹]')
ggsave('figs/boxplot_AD_subgrupo_textural.png')

#### Triangulo com as amostras ####
# salvando configurações padrão do gráfico
# para voltar ao normal depois
orig.par <- par(no.readonly = TRUE)

# Selecionando uma paleta de cores aos valores de AD em 5 classes

paleta <- colorRampPalette(c('red',  'green', 'blue'))
quebras_amostra <- cut(dados$AD,
                       breaks = seq(min(dados$AD),
                                    max(dados$AD),
                                    len = 6),
                       include.lowest = TRUE)

cores <- paleta(6)[quebras_amostra]

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

TernaryPlot(main = 'Gráfico ternário dos dados de AD',
            alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')
AddToTernary(points, dados[c('ARG', 'SIL', 'AT')],
             cex = 0.5, pch = 19, col = cores)

legend('topright',
       title = 'AD [mm cm⁻¹]',
       legend = levels(quebras_amostra),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = paleta(6))

dev.copy(png, 'figs/amostras_ternario.png')
dev.off()

# voltando configurações da área do gráfico
par(orig.par)

#### Descritiva dos resultados do modelo M2 ####

dados_modelo <- read.csv2('resultados/Preditos_triangulo_AD_m2.csv')

dados_modelo$grupo <- grupo_textural(dados_modelo$AT, dados_modelo$SIL, dados_modelo$ARG)
dados_modelo$subgrupo <- subgrupo_textural(dados_modelo$AT, dados_modelo$SIL, dados_modelo$ARG)

ggplot(dados_modelo) +
  geom_density(aes(x = ADm2)) +
  labs(x = 'Água disponível calculada- ADcal - [mm cm⁻¹]', y = 'FDP Kernel') +
  geom_vline(aes(xintercept = median(ADm2))) +
  geom_vline(aes(xintercept = quantile(ADm2, 0.25)), lty = 'dashed') +
  geom_vline(aes(xintercept = quantile(ADm2, 0.75)), lty = 'dashed')

ggsave('figs/distribuicao_AD_modelo2.png')

resumo_modelo <- dados_modelo %>%
  group_by(subgrupo) %>%
  summarise(mediana = round(median(ADm2),2),
            obs = n())

ggplot(dados_modelo) +
  geom_boxplot(aes(x = subgrupo, y = ADm2)) +
  geom_text(data = resumo_modelo,
            aes(x = subgrupo, y = mediana + 0.08, label = mediana)) +
  geom_text(data = resumo_modelo,
            aes(x = subgrupo, y = 3, label = paste0('(', obs, ')'))) +
  labs(x = 'Subgrupo textural', y = 'ADcal [mm cm⁻¹]')

ggsave('figs/boxplot_AD_subgrupo_textural_modelo2.png')
