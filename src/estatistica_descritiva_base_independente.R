# Estatísticas básicas do conjunto de dados independente

library(tidyverse)
library(Ternary)
source('src/grupo_subgrupo_textural.R')

# Carregandos dados indepentendes
dados <- read_csv2('dados/bd_independente.csv') %>%
  rename(id = X1) %>%
  mutate(AD = AD * 10) %>%
  filter(AT + SIL + ARG > 96,
         AT + SIL + ARG <= 100)

dados$grupo <- grupo_textural(dados$AT, dados$SIL, dados$ARG)
dados$subgrupo <- subgrupo_textural(dados$AT, dados$SIL, dados$ARG)

summary(dados)

ggplot(dados) +
  geom_density(aes(x = AT)) +
  labs(x = 'Areia Total [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_at_independente.png')

ggplot(dados) +
  geom_density(aes(x = SIL)) +
  labs(x = 'Slte [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_sil_independente.png')

ggplot(dados) +
  geom_density(aes(x = ARG)) +
  labs(x = 'Argila [%]', y = 'FDP Kernel')
ggsave('figs/distribuicao_arg_independente.png')

ggplot(dados) +
  geom_density(aes(x = AD)) +
  labs(x = 'Água disponível - AD - [mm cm⁻¹]', y = 'FDP Kernel') +
  geom_vline(aes(xintercept = median(AD))) +
  geom_vline(aes(xintercept = quantile(AD, 0.25)), lty = 'dashed') +
  geom_vline(aes(xintercept = quantile(AD, 0.75)), lty = 'dashed') +
ggsave('figs/distribuicao_AD_independente.png')

dados_long <- dados %>%
  pivot_longer(c(AT, SIL, ARG),
               names_to = 'fração') %>%
  mutate(fração = factor(fração, levels = c('AT', 'SIL', 'ARG'),
                         labels = c('Areia Total', 'Silte', 'Argila')))

ggplot(dados_long) +
  geom_density(aes(x = value, fill = fração), alpha = 0.4) +
  labs(x = 'porcento', y = 'FDP Kernel', fill = 'Granulometria')
ggsave('figs/distribuicao_fracoes_over_independente.png')

ggplot(dados_long) +
  geom_density(aes(x = value, col = base)) +
  facet_wrap(~fração) +
  labs(x = 'Porcento', y = 'FDP Kernel')
ggsave('figs/distribuicao_fracoes_grade_independente.png')

ggplot(dados_long) +
  geom_boxplot(aes(x = fração, y = value)) +
  labs(x = 'Frações texturais', y = 'Porcento')
ggsave('figs/boxplot_fracoes_independente.png')

resumo <- dados %>%
  group_by(subgrupo) %>%
  summarise(mediana = round(median(AD),2),
            obs = n())

ggplot(dados) +
  geom_boxplot(aes(x = subgrupo, y = AD)) +
  geom_text(data = resumo,
            aes(x = subgrupo, y = mediana + 0.08, label = mediana)) +
  geom_text(data = resumo,
            aes(x = subgrupo, y = 3.1, label = paste0('(', obs, ')'))) +
  labs(x = 'Subgrupopamento textural', y = 'AD [mm cm⁻¹]')
ggsave('figs/boxplot_AD_subgrupo_textural_independente.png')

#### Triangulo ####
# salvando configurações padrão do gráfico
# para voltar ao normal depois
orig.par <- par(no.readonly = TRUE)

# Selecionando uma paleta de cores aos valores de AD em 5 classes

paleta <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
quebras_amostra <- cut(dados$AD,
                       breaks = seq(min(dados$AD),
                                    max(dados$AD),
                                    len = 6),
                       include.lowest = TRUE)

cores <- paleta[quebras_amostra]

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

TernaryPlot(main = 'Gráfico ternário dos dados de AD independentes',
            alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')
AddToTernary(points, dados[c('ARG', 'SIL', 'AT')],
             cex = 0.6, pch = 21, bg = cores, col = 'grey35')

legend('topright',
       title = 'AD [mm cm⁻¹]',
       legend = levels(quebras_amostra),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = paleta, col = 'grey25')

dev.copy(png, 'figs/amostras_ternario_independente.png')
dev.off()

# voltando configurações da área do gráfico
par(orig.par)
