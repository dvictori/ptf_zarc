library(Ternary)

dados <- read.csv2("../dados/bd_zarc.csv")

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

paleta <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
quebras_amostra <- cut(dados$AD * 10,
                       breaks = seq(min(dados$AD) * 10,
                                    max(dados$AD) * 10,
                                    len = 6),
                       include.lowest = TRUE)

cores <- paleta[quebras_amostra]

# salvar em alta resolução
png('figs/amostras_ternario_restricoes_base.png', width = 6,
    height = 6, units = 'in', res = 300)

TernaryPlot(alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')

AddToTernary(points, dados[c('ARG', 'SIL', 'AT')],
             cex = 0.5, pch = 21, bg = cores, col = 'grey35')

base_filtrada <- matrix(c(
  06, 42, 52,
  06, 16, 78,
  12, 10, 78,
  81, 10, 09,
  81, 16, 03,
  55, 42, 03
), ncol = 3, byrow = TRUE)

TernaryPolygon(base_filtrada, border = 'black', lty = 'dashed', lwd = 1)

legend('topright',
       title = 'AD [mm cm⁻¹]',
       legend = levels(quebras_amostra),
       cex=0.6, bty='n', pch=21,
       pt.bg = paleta, col = 'grey35')

# dev.copy(png, '../figs/amostras_ternario_restricoes_base.png')
dev.off()
