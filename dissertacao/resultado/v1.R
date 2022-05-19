####################################
# RESULTADOS MESTRADO - MAR<U+FFFD>O 2022 #
####################################

## baixando pacotes ##
library(lme4) ## para fazer os modelos mistos
library(car) ## para o Anova
library(DHARMa) ## para analise de residuos
library(mixlm) ## tambem permite analisar os modelos
library(MuMIn) ## para calcular o R2
library(ggplot2)
library(glmulti)
library(devtools)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(emmeans)
library(multcomp)
library(multcompView)


# CHAMANDO A TABELA #
data1 <- read.delim2("dados.txt", stringsAsFactors = TRUE)
# data1 <- read.delim("C:/Users/Renata/Google Drive/Mestrado RENATA/R/dados_mestrado_renata_completo.txt", stringsAsFactors=TRUE,dec = ',')
View(data1)
summary(data1)
dim(data1) # dimensiona minha tabela (linhas / colunas)

#    _____________________________________________________________________________________________________
#   |PERGUNTA: AS LAGOAS S<U+FFFD>O DIFERENTES EM QUANTIDADE (DOC) E QUALIDADE? (A440, SUVA, A 254, S275 E SR)  |
#   -----------------------------------------------------------------------------------------------------

# anova

# DOC

str(data1)
carb <- glm(doc ~ lake, data = data1)
summary(carb)
simulateResiduals(carb, n = 500, plot = T)
car::Anova(carb)

EMM = emmeans(carb, ~lake, type = "response")
EMM
CLD <- as.data.frame(cld(EMM, Letters = letters))
pairs(EMM)

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
emmeans(warp.lm, ~ wool | tension, drop = TRUE)


# SUVA
suv <- glm(suva ~ lake, data = data1)
summary(suv)
simulateResiduals(suv, n = 500, plot = T)
car::Anova(suv)

EMM <- emmeans(suv, ~lake, type = "response")
EMM
CLD <- as.data.frame(cld(EMM, Letters = letters))
CLD
pairs(EMM)


# S275
as275 <- glm(s275 ~ lake, data = data1)
summary(as275)
simulateResiduals(as275, n = 500, plot = T)
car::Anova(as275)

EMM <- emmeans(as275, ~lake, type = "response")
EMM
CLD <- as.data.frame(cld(EMM, Letters = letters))
CLD
pairs(EMM)


# A254
aa254 <- glm(a254 ~ lake, data = data1)
summary(aa254)
simulateResiduals(aa254, n = 500, plot = T)
car::Anova(aa254)

EMM <- emmeans(aa254, ~lake, type = "response")
EMM
CLD <- as.data.frame(cld(EMM, Letters = letters))
CLD
pairs(EMM)


# SR
asr <- glm(sr ~ lake, data = data1)
summary(asr)
simulateResiduals(asr, n = 500, plot = T)
car::Anova(asr)

EMM <- emmeans(asr, ~lake, type = "response")
EMM
CLD <- as.data.frame(cld(EMM, Letters = letters))
CLD
pairs(EMM)

#     -------------------------------------------------------------------------------------------------------
#    | PERGUNTA: QUAIS S<U+FFFD>O AS CARACTER<U+FFFD>STICAS QUE MAIS INFLUENCIAM A QUANTIDADE DE CARBONO NESSES LAGOS ?   |
#    --------------------------------------------------------------------------------------------------------

#  |PCA|

# PACOTES
library(FactoMineR)
library(factoextra)
library(vegan)
library(nlme)
library(AICcmodavg)
library(ggplot2)
library(mice)


data1 <- read.delim("C:/Users/Renata/Google Drive/Mestrado RENATA/R/dados_mestrado_renata_completo.txt", stringsAsFactors = TRUE, dec = ",")
data1
# mydata <- na.omit(tabela_pca) # retira as linhas onde eu n<U+FFFD>o tenho dados
View(data1)
summary(data1)

mydata.PCA <- complete(mice(data1))
names(data1)
# gerando a PCA com as colunas de interesse

names(mydata.PCA)
colnames(mydata.PCA) <- c(
    "Lago", "Estação", "Ano", "Status", "Entorno", "LA", "DA", "Vol", "DA/VOL", "DR", "DP", "Inclinação", "L", "Prof. Med", "Prof. Max", "%NF", "%AA", "Pluviosidade",
    "Temp", "secchi", "Zmix", "KdPAR", "Zeu", "pH", "Chl-a", "TSS", "TN", "TP", "DOC", "cdom", "a254", "a440", "SR", "SUVA254", "s275-295", "s350", "BV"
)

# dataPCA<-mydata.PCA[,c(37,21,19,18,25,22,15,16,17,27,10,12,29,31,33:35)]

# PAR<U+FFFD>METROS A SER UTILIZADOS: volume (8), DA/vOL (9), DR (10), DP (11), inclina<U+FFFD><U+FFFD>o(12), prof. m<U+FFFD>d (14), %nf(16)
# pluviosidade(18), temp(19), kd(22), chl-a(25),zmix(21),TN(27), DOC(29), A254(31), SR(33),SUVA(34), S275 (35), bv(37)

dataPCA <- mydata.PCA[, c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 21, 27, 29, 31, 33, 34, 35, 37)]

# pca <- PCA(mydata.PCA[,c(8,10,11,12,14,16,18,19,22,25,29,31:35)], graph = F)
pca <- prcomp(dataPCA, scale = T) # mudo aqui para dataPCA ou data.PCA
summary(pca)

# extrair % de explica<U+FFFD><U+FFFD>o das componentes
eig.value <- get_eigenvalue(pca)
eig.value

fviz_eig(pca, addlabels = T, ylim = c(0, 90)) ## 43,3%

# gr<U+FFFD>fico

auto <- autoplot(pca, data = data1, loadings = TRUE, loadings.colour = "black", loadings.label.repel = T, frame = TRUE, frame.type = "norm", layers = "species", scale = 2, loading.label.color = "black")

theme <- theme(panel.border = element_rect(fill = NA), strip.background = element_blank(), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), axis.ticks = element_line(colour = "black"), plot.margin = unit(c(1, 1, 1, 1), "line")) + theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.3, "cm"), legend.background = element_rect(color = "black", linetype = "solid")) + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
pic4 <- auto + theme

pic4

# _________________________________________________________________________________________________________________

#    ____________________________________
#   | Escolhendo as melhores vari<U+FFFD>veis  |
#   ------------------------------------

names(data1)
## escolhendo as vari<U+FFFD>veis de acordo com o AIC##

# names(TABELA)[X] => no colchete, colocar a vari<U+FFFD>vel resposta
# names(TABELA)[c(11,17,14,20)] => quais as provaveis vari<U+FFFD>veis. pode colocar quantas quiser
# intercept = TRUE, level = 1, minsize = 1, maxsize =4 ( AT<U+FFFD> QUANTAS COMBINA<U+FFFD><U+FFFD>ES PARA O TAMANHO DO MODELO)
# METHOD H= RODA / D = TESTA

# -------------------------------------
# DOC ~ vari<U+FFFD>veis de interesse :  10-da.la, 12-slope, 18-rainfall, 19-temp,14- dmean,
# 11-dp, 8-vol, 22-kd, 16-nf
# ------------------------------------
glmulti(names(data1)[29],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)

glmulti(names(data1)[29],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 27, 37)], intercept = TRUE, level = 1, minsize = 1, maxsize = 4,
    method = "h", crit = "aic", confsetsize = 10
)

# -------------------------------------
# SUVA ~ vari<U+FFFD>veis de interesse :  10-da.la, 12-slope, 18-rainfall, 19-temp,14- dmean,
# 11-dp, 8-vol, 22-kd, 16-nf
# ------------------------------------

glmulti(names(data1)[34],
    data = data1,
    names(data1)[c(8, 10, 11, 12, 14, 16, 18, 19, 22, 25)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)

glmulti(names(data1)[34],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 27, 37)], intercept = TRUE, level = 1, minsize = 1, maxsize = 4,
    method = "h", crit = "aic", confsetsize = 10
)

# -------------------------------------
# S275 ~ vari<U+FFFD>veis de interesse :  10-da.la, 12-slope, 18-rainfall, 19-temp,14- dmean,
# 11-dp, 8-vol, 22-kd, 16-nf
# ------------------------------------

glmulti(names(data1)[35],
    data = data1,
    names(data1)[c(8, 10, 11, 12, 14, 16, 18, 19, 22, 25)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)

glmulti(names(data1)[35],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 28, 37)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)
# -------------------------------------
# A254 ~ vari<U+FFFD>veis de interesse :  10-da.la, 12-slope, 18-rainfall, 19-temp,14- dmean,
# 11-dp, 8-vol, 22-kd, 16-nf
# ------------------------------------

glmulti(names(data1)[31],
    data = data1,
    names(data1)[c(8, 10, 11, 12, 14, 16, 18, 19, 22, 25)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)

glmulti(names(data1)[31],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 28, 37)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)
# -------------------------------------
# SR ~ vari<U+FFFD>veis de interesse :  10-da.la, 12-slope, 18-rainfall, 19-temp,14- dmean,
# 11-dp, 8-vol, 22-kd, 16-nf
# ------------------------------------

glmulti(names(data1)[33],
    data = data1,
    names(data1)[c(8, 10, 11, 12, 14, 16, 18, 19, 22, 25)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)

glmulti(names(data1)[33],
    data = data1,
    names(data1)[c(8, 9, 10, 11, 12, 14, 16, 18, 19, 22, 25, 28, 37)], intercept = TRUE, level = 1, minsize = 1, maxsize = 5,
    method = "h", crit = "aic", confsetsize = 10
)
# ______________________________________________________________________________________________________________________________________

#  __________
# | MODELOS |
# -----------

#  ===> DOC   ( doc~dp+slope+dmean+nf+chl.a)
names(data1)
cor(data1[, c(11, 12, 14, 16, 24, 25)])
MG.COD <- lmer(doc ~ dp + slope + dmean + nf + chl.a + (1 | lake), data = data1)
summary(MG.COD)
simulateResiduals(MG.COD, n = 500, plot = T)
car::Anova(MG.COD)

MG.COD <- lmer(doc ~ da.vol + slope + rainfall + temp + (1 | lake), data = data1)
summary(MG.COD)
simulateResiduals(MG.COD, n = 500, plot = T)
car::Anova(MG.COD)


#  ===> SUVA   ( suva~da.la+ dp+slope+dmean+chl.a)
names(data1)
cor(data1[, c(10, 11, 12, 14, 24, 25)])
MG.SUVA <- lmer(suva254 ~ dr + dp + slope + dmean + chl.a + (1 | lake), data = data1)
summary(MG.SUVA)
simulateResiduals(MG.SUVA, n = 500, plot = T)
car::Anova(MG.SUVA)

MG.SUVA <- lmer(suva254 ~ vol + dp + rainfall + kd + (1 | lake), data = data1)
summary(MG.SUVA)
simulateResiduals(MG.SUVA, n = 500, plot = T)
car::Anova(MG.SUVA)


#  ===> S275   ( s275~dp+chl.a+kd)
names(data1)
cor(data1[, c(11, 25, 22)])
MG.S275 <- lmer(s275.295 ~ dp + chl.a + kd + (1 | lake), data = data1)
summary(MG.S275)
simulateResiduals(MG.S275, n = 500, plot = T)
car::Anova(MG.S275)

dnames(data1)
cor(data1[, c(11, 25, 22)])
MG.S275 <- lmer(s275.295 ~ rainfall + temp + kd + bv + (1 | lake), data = data1)
summary(MG.S275)
simulateResiduals(MG.S275, n = 500, plot = T)
car::Anova(MG.S275)


#  ===> A245   ( a254~vol+dp+ slope+kd)
names(data1)
cor(data1[, c(8, 11, 12, 22)]) # NA NO VOLUME, POR ISSO N<U+FFFD>O APARECE
MG.A254 <- lmer(a254 ~ vol + dp + slope + kd + (1 | lake), data = data1)
summary(MG.A254)
simulateResiduals(MG.A254, n = 500, plot = T) # ACHOU TEND<U+FFFD>NCIA
car::Anova(MG.A254)

#  ===> SR  (sr~kd+chl.a)
names(data1)
cor(data1[, c(24, 25)])
MG.SR <- lmer(sr ~ kd + chl.a + (1 | lake), data = data1)
summary(MG.SR)
simulateResiduals(MG.SR, n = 500, plot = T)
car::Anova(MG.SR)

MG.SR <- lmer(sr ~ kd + chl.a + bv + (1 | lake), data = data1)
summary(MG.SR)
simulateResiduals(MG.SR, n = 500, plot = T)
car::Anova(MG.SR)