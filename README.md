# Genetic-correlation-between-environments
Genetic correlation between environments
#########################
######################### Genetic correlation between environments ------------


library(asreml)

# Exemplo: Modelo com interação gen x ambiente (random) para yield
mod_corr <- asreml(fixed = yield ~ env,
                   random = ~ fa(env, 1):gen,
                   data = data,
                   na.action = na.method(y = "include"))

# Obter a matriz de variâncias e covariâncias genéticas (G-matrix)
G_matrix <- predict(mod_corr, classify = "gen:env")$pvals

# Opcional: Converter para matriz de correlação genética
# Você precisa extrair as covariâncias primeiro
# A partir do G-matrix, você calcula a correlação:
# cor_matrix <- cov2cor(G_matrix)


head(G_matrix)


library(tidyr)
library(dplyr)

# Transformar para formato wide: genótipos nas linhas e ambientes nas colunas
G_wide <- G_matrix %>%
  select(gen, env, predicted.value) %>%
  pivot_wider(names_from = env, values_from = predicted.value)

# Visualizar o formato da tabela
head(G_wide)



# Remove a coluna 'gen' para fazer a correlação só com os valores
G_wide_mat <- as.matrix(G_wide[,-1])  # Exclui a coluna 'gen'

# Calcula a correlação genética entre ambientes
cor_genetic_env <- cor(G_wide_mat, use = "pairwise.complete.obs")

# Visualiza a matriz de correlação genética
print(cor_genetic_env)

library(corrplot)

# Criar um gráfico de correlação entre ambientes
corrplot(cor_genetic_env,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         col = colorRampPalette(c("#FF3300", "white", "#26D950"))(200))

################

# Pacotes necessários
library(reshape2)
library(dplyr)
library(corrplot)

# Passo 1: Exemplo - carregar ou usar sua matriz de correlação genética entre ambientes
# (Aqui você já tem cor_genetic_env)

# Visualizar a matriz para conferir
print(cor_genetic_env)

# Passo 2: Converter matriz para formato longo
cor_long <- melt(cor_genetic_env, varnames = c("Env1", "Env2"), value.name = "Correlation")

# Passo 3: Remover pares duplicados e a diagonal
cor_long <- cor_long %>%
  mutate(
    Env1 = as.character(Env1),
    Env2 = as.character(Env2)
  ) %>%
  filter(Env1 != Env2) %>%                             # Remove a diagonal (correlação de 1 com ele mesmo)
  distinct(pair1 = pmin(Env1, Env2),                  # Mantém apenas um par
           pair2 = pmax(Env1, Env2),
           .keep_all = TRUE)

# Passo 4: Contagem de interações simples e complexas
n_total <- nrow(cor_long)
n_simple <- sum(cor_long$Correlation > 0)
n_complex <- sum(cor_long$Correlation < 0)

# Passo 5: Proporções (%)
prop_simple <- (n_simple / n_total) * 100
prop_complex <- (n_complex / n_total) * 100

# Passo 6: Exibir resultados
cat("Proporção de interação simples:", round(prop_simple, 2), "%\n")
cat("Proporção de interação complexa:", round(prop_complex, 2), "%\n")

# Passo 7: Visualização opcional - Mapa de correlação
corrplot(cor_genetic_env,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Genetic Correlation between Environments",
         mar = c(0,0,2,0))

# Passo 8: Visualização opcional - Pizza plot para proporções
library(ggplot2)

# Criar data frame com as proporções
prop_df <- data.frame(
  Interaction = c("Simple Interaction", "Complex Interaction"),
  Percentage = c(prop_simple, prop_complex)
)

# Plotar pizza
ggplot(prop_df, aes(x = "", y = Percentage, fill = Interaction)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Simple Interaction" = "darkgreen", "Complex Interaction" = "red")) +
  theme_void() +
  labs(fill = "Interaction Type",
       title = "Proportion of Simple and Complex Interaction (%)")
