load('df_bairros.RData')


summary(df_bairros)


# ---------------------- Procedimento N-1 Dummies

df_bairros <- dummy_columns(.data = df_bairros,
                                    select_columns = "tipo",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)

df_bairros_dummies <- dummy_columns(.data = df_bairros,
                                   select_columns = "bairro",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

#Retirando espaçoes entre os nomes das colunas
names(df_bairros_dummies) <- sub(" ", "_", names(df_bairros_dummies))
names(df_bairros_dummies) <- sub(" ", "_", names(df_bairros_dummies))


summary(df_bairros_dummies)

# Vamos separar a base em treinamento e teste #
set.seed(1234)
bool_treino <- stats::runif(dim(df_bairros_dummies)[1])>.25

treino <- df_bairros_dummies[bool_treino,]
teste  <- df_bairros_dummies[!bool_treino,]


# ---------------------- Estimação da regressão linear múltipla
modelo_linear <- lm(formula = precos ~ .,
                    data = treino)

# ---------------------- Verificar se tem modelo
summary(modelo_linear)
#Como p-value do teste F é menor que 0,05 (2.2e-16), há modelo 
#Várias variáveis explicativas não passaram no teste t (p-value menor 0,05 )

# ---------------------- Procedimento stepwise
step_modelo_linear <- step (modelo_linear, k = 3.841459)
summary(step_modelo_linear)

# ---------------------- Teste de Shapiro-Francia
# teste de verificação da aderência dos resíduos à normalidade (pacote nortest)
sf.test(step_modelo_linear$residuals)
#Como p-value do teste de Shapiro-Francia é menor que 0,05 (9.674e-07), é necessário fazer a transformação de box-cox pois não há normalidade

# ---------------------- Teste de Breusch-Pagan 
#teste de diagnóstico de de diagnóstico de heterocedasticidade (pacote olsrr)
ols_test_breusch_pagan(step_modelo_linear)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!
#O valor de p-value é menor que 0,05 (3.338757e-08 ), ou seja, há indicios que não há omissão de uma variavel relevante

#Adicionando fitted values e resíduos do modelo 
#no dataset 'df_bairros_dummies'
#Criando dataset de resultados na bsae de teste
df_resultados <- teste[, c("precos","precos")]
df_resultados <- df_resultados[,1, drop=F]

step_modelo_linear$coefficients

df_resultados$fitted_step <- predict(step_modelo_linear, teste)

#Gerando os fitted values dos modelos
#df_resultados$fitted_step <- step_modelo_linear$fitted.values
#df_resultados$residuos_step <- step_modelo_linear$residuals


#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
#df_resultados %>%
#  ggplot() +
#  geom_point(aes(x = fitted_step, y = residuos_step),
#             color = "#55C667FF", size = 3) +
#  labs(x = "Fitted Values do Modelo Stepwise",
#       y = "Resíduos do Modelo Stepwise") +
#  theme_bw()

# ---------------------- Para calcular o lambda da transformação Box-Cox
lambda_BC <- powerTransform(treino$precos) #função powerTransform do pacote car
lambda_BC

# ---------------------- Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
treino$bc_precos <- (((treino$precos ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

# ---------------------- Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_bc <- lm(bc_precos ~ .-precos , 
                data = treino)
summary(modelo_bc)
#p-value: < 2.2E-16 ... há modelo

# ---------------------- Procedimento stepwise
step_modelo_bc <- step (modelo_bc, k = 3.841459)
summary(step_modelo_bc)

# ---------------------- Teste de Shapiro-Francia
sf.test(step_modelo_bc$residuals)
#Agora passa no teste de Shapiro-Francia pois o valor de p-value é maior que 0,05 (0.1149)

# ---------------------- Teste de Breusch-Pagan 
ols_test_breusch_pagan(step_modelo_bc)
#O valor de p-value é maior que 0,05 (0.7291486), ou seja, há indicios que há omissão de uma variable relevante


#Adicionando fitted values e resíduos do modelo 'step_modelo_bc' no dataset
df_resultados$fitted_step_bc <- (((predict(step_modelo_bc, teste)*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#df_bairros_dummies$fitted_step_bc <- step_modelo_bc$fitted.values
#df_resultados$residuos_step_bc <- step_modelo_bc$residuals

# ---------------------- Apagando coluna com os valores preditos por boxcox
treino$bc_precos <- NULL

#Gráfico 
#Ajustes dos modelos: valores previstos (fitted values) X valores reais
ggplotly(
  df_resultados %>%
    ggplot() +
    geom_smooth(aes(x = precos, y = fitted_step, color = "Linear - Stepwise"),
                method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
    geom_point(aes(x = precos, y = fitted_step),
               color = "#440154FF", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = precos, y = fitted_step_bc, color = "Box-Cox - Stepwise"),
                method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
    geom_point(aes(x = precos, y = fitted_step_bc),
               color = "#287D8EFF", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = precos, y = precos), method = "lm", 
                color = "grey30", size = 1.05,
                linetype = "longdash") +
    scale_color_manual("Modelos:", 
                       values = c("#287D8EFF", "#440154FF")) +
    labs(x = "Preços", y = "Fitted Values") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")
)



#####################################
pacotes <- c(
  'tidyverse',  # Pacote básico de datawrangling
  'rpart',      # Biblioteca de árvores
  'rpart.plot', # Conjunto com Rpart, plota a parvore
  'gtools',     # funções auxiliares como quantcut,
  'Rmisc',      # carrega a função sumarySE para a descritiva
  'scales',     # importa paletas de cores
  'viridis',    # Escalas 'viridis' para o ggplot2
  'caret',       # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost',
  'rsample',
  'ranger',
  'caret',
  'forecast',
  'h2o'
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Construindo a árvore simples
system.time(
tree <- rpart(precos ~ . - precos, 
              data=treino,
              control=rpart.control(maxdepth = 30, cp=0))
)


# Valores preditos para uma arvore simples
df_resultados$tree_fitted = predict(tree, teste)
#df_resultados$tree_residuos_fitted = df_resultados$precos - df_resultados$tree_fitted

df_resultados %>%
  ggplot() +
  geom_smooth(aes(x = precos, y = fitted_step_bc, color = "2 - Box-Cox - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= fitted_step, color = "1 - Linear - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= tree_fitted, color = "3 - Árvore Simples"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y = precos), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid","springgreen3","chocolate1","red","orange")) +
  labs(x = "Preços", y = "Fitted Values") +
  theme_bw()

# Construindo a random forest convencional

set.seed(123)
rf_simples <- randomForest(
  formula = precos ~ .,
  data    = treino
)

rf_simples

plot(rf_simples)
# número de árvores com o menor MSE (mean squared error)
which.min(rf_simples$mse)

# RMSE (root mean squared error) do random forest com menor MSE 
sqrt(rf_simples$mse[which.min(rf_simples$mse)])


# nome das variaveis de entrada (p)
features <- setdiff(names(treino), "precos")
length(features)

# passo opcional: tentar encontrar o valor de mtry otimo para a árvore
set.seed(1234)
modelo_2 <- tuneRF(
  x          = treino[features],
  y          = treino$precos,
  ntreeTry   = 500, 
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # não mostar o progresso em tempo real 
)
# comparar com o valor padrão do pacote (p/3)
length(features)/3
#Valor bem distindo

# Escolhendo pacote para o processo iterativo
# Tempo decorrido no randomForest 
system.time(
  tempo_randomForest <- randomForest(
    formula = precos ~ ., 
    data    = df_bairros_dummies, 
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)
#Tempo decorrido de 0,51s pra varrer todo dataset

# Tempo decorrido no ranger speed
system.time(
  tempo__ranger <- ranger(
    formula   = precos ~ ., 
    data      = df_bairros_dummies, 
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)
#Tempo decorrido de 0,04s pra varrer todo dataset


# Criando dataframe com todas as possíveis combinações dos parametros
hyper_grid <- expand.grid(
  mtry       = seq(2, 7, by = 1),
  sampe_size = seq(.5, .9, by = 0.02),
  node_size  = seq(2, 5, by = 1),
  OOB_RMSE   = 0
)


# número de combinações obtidos
nrow(hyper_grid)
## [1] 4836


system.time(
for(i in 1:nrow(hyper_grid)) {
  model <- ranger(
    formula         = precos ~ ., 
    data            = treino, 
    num.trees       = 500, 
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  # Adicionando OOB no dataframe das combinações possíveis
  #treino$temp <- predict(model, treino)$predictions
  #hyper_grid$RMSE[i] <- accuracy(treino$precos,treino$temp)[2]
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}
)
#treino$temp <- NULL

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(5)

#hyper_grid %>% 
#  dplyr::arrange(RMSE) %>%
#  head(5)

tuning_values <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]
tuning_values

#OOB RMSE vária entre 216.6448 e 2450.9158
min(hyper_grid$OOB_RMSE)
max(hyper_grid$OOB_RMSE)

OOB_RMSE <- vector(mode = "numeric", length = 100)

set.seed(1234)
for(i in seq_along(OOB_RMSE)) {
  optimal_ranger <- ranger(
    formula         = precos ~ ., 
    data            = treino, 
    num.trees       = 500,
    mtry            = tuning_values$mtry,
    min.node.size   = tuning_values$node_size,
    sample.fraction = tuning_values$sampe_size,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}


hist(OOB_RMSE, breaks = 20)

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  xlab("Variável") + 
  ylab("Importância da Variável") +
  geom_col() +
  coord_flip() +
  ggtitle("")


set.seed(1234)
optimal_rf <- randomForest(
  formula = precos ~ .,
  data    = treino,
  num.trees = 500,
  mtry = tuning_values$mtry,
  min.node.size = tuning_values$node_size,
  sample.fraction = tuning_values$sampe_size    
)

min(optimal_rf$mse)
sqrt(min(optimal_rf$mse))

min(rf_simples$mse)
sqrt(min(rf_simples$mse))

# Predições do random forest

#df_resultados$rf_simples_fitted <- predict(rf_simples, df_bairros_dummies)
#df_resultados$ranger_fitted <- predict(optimal_ranger, df_bairros_dummies)$predictions
#df_resultados$rf_fitted <- predict(optimal_rf, df_bairros_dummies)

df_resultados$rf_simples_fitted <- predict(rf_simples, teste)
df_resultados$ranger_fitted <- predict(optimal_ranger, teste)$predictions
df_resultados$rf_fitted <- predict(optimal_rf, teste)


#Plotando
df_resultados %>%
  ggplot() +
  geom_smooth(aes(x = precos, y = fitted_step_bc, color = "2 - Box-Cox - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= fitted_step, color = "1 - Linear - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= tree_fitted, color = "3 - Árvore Simples"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= rf_simples_fitted, color = "4 - RF Simples"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= rf_fitted, color = "5 - RF Otimizado"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y = precos), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid","springgreen3","chocolate1","red","blue")) +
  labs(x = "Preços", y = "Valores Preditos") +
  theme_bw()



accuracy(df_resultados$precos,df_resultados$tree_fitted)
accuracy(df_resultados$precos,df_resultados$fitted_step)
accuracy(df_resultados$precos,df_resultados$fitted_step_bc)

accuracy(df_resultados$precos,df_resultados$rf_simples_fitted)
accuracy(df_resultados$precos,df_resultados$rf_fitted)
accuracy(df_resultados$precos,df_resultados$ranger_fitted)

#############################################################################################################
#NAO USADO NO TCC
#Estudo sobre o desbalanceamento dos dados
aggregate(data.frame(contagem = df_bairros$bairro), list(bairro = df_bairros$bairro), length)

#Preco medio da acomodação por 
aggregate(data.frame(media = df_bairros$precos), list(bairro = df_bairros$bairro), FUN=mean) 


#Exploração visual do preco médio
df_bairros %>%
  group_by(bairro) %>%
  mutate(preco_medio = mean(precos, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = bairro, y = precos),color = "orange", alpha = 0.5, size = 2) +
  geom_line(aes(x = bairro, y = preco_medio, 
                group = 1, color = "Preço Médio por Bairro"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Bairro",
       y = "Preço") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (precos) por bairro
ggplotly(
  ggplot(df_bairros, aes(x = precos)) +
    geom_density(aes(color = bairro, fill = bairro), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Step-up strategy
################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM2                        #
################################################################################
set.seed(1234)
bool_treino <- stats::runif(dim(df_bairros)[1])>.25

treino_2 <- df_bairros[bool_treino,]
teste_2  <- df_bairros[!bool_treino,]

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = precos ~ 1, #1 = não varia em função de ngm (não tem variável X no modelo nulo)
                        random = ~ 1 | bairro, #1 = apenas efeito aleatorio de intercepto,v0j, (após o pipe declara o nível) no nível bairro
                        data = treino_2,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)
#Variância de efeitos aleatórios de intercepto Var(v0j) é estatisticamente diferente de 0, ou seja,
#consideração de modelo multinível é superior a qualquer modelo no cenário OLS


#ICC Intraclass correlation
icc_bairro <- 27545.17/(27545.17+110188.93)
icc_bairro #19,98% da variação dos preços é devido ao efeito bairro

################################################################################
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = precos ~ 1, 
                      data = treino_2)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())



################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################

#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = precos ~ superhost + avaliacoes + hospedes + banheiros + quartos + camas + comentarios,
                             # precos varia de acordo com as variáveis após o "~"
                             random = ~ 1 | bairro,
                             #1 = apenas efeito aleatorio de intercepto,v0j, (após o pipe declara o nível) no nível bairro
                             data = treino_2,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#fixed = precos ~ superhost + avaliacoes + hospedes + banheiros + quartos + camas + comentarios,
#random = ~ superhost + avaliacoes + hospedes + banheiros + quartos + camas + comentarios | bairro,
#superhost + banheiros + quartos

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = precos ~ superhost + avaliacoes + hospedes + banheiros + quartos + camas + comentarios,
                                    random = ~ 1 | bairro, #errado colocar superhost no lugar de 1
                                    data = treino_2,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Gerando os fitted values dos modelos
df_resultados$hlm2_nulo_fitted <- predict(modelo_nulo_hlm2, teste_2)
df_resultados$hlm2_intercept_fitted <- predict(modelo_intercept_hlm2, teste_2)
df_resultados$hlm2_intercept_inclin_fitted <- predict(modelo_intercept_inclin_hlm2, teste_2)


#Plotagem
df_resultados %>%
  ggplot() +
  geom_smooth(aes(x = precos, y = fitted_step_bc, color = "2 - Box-Cox - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= fitted_step, color = "1 - Linear - Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= hlm2_nulo_fitted, color = "3 - HLM2 - Nulo"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= hlm2_intercept_fitted, color = "4 - HLM2 - Interceptos Aleatórios"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y= hlm2_intercept_inclin_fitted, color = "5 - HLM2 - Inclinações/Interceptos Aleatórios"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = precos, y = precos), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid","springgreen3","chocolate1","red")) +
  labs(x = "Preços", y = "Fitted Values") +
  theme_bw()

accuracy(df_resultados$precos,df_resultados$fitted_step)
accuracy(df_resultados$precos,df_resultados$fitted_step_bc)

accuracy(df_resultados$precos,df_resultados$rf_simples_fitted)
accuracy(df_resultados$precos,df_resultados$rf_fitted)
accuracy(df_resultados$precos,df_resultados$ranger_fitted)

accuracy(df_resultados$precos,df_resultados$hlm2_nulo_fitted)
accuracy(df_resultados$precos,df_resultados$hlm2_intercept_fitted)
accuracy(df_resultados$precos,df_resultados$hlm2_intercept_inclin_fitted)

