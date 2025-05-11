# Carregar bibliotecas possíveis
#biblioteca (WDI)
#biblioteca(ggplot2)

library(WDI)
library(tidyverse)

# Coletando os dados
#dadospib <- WDI(país = 'Todos',
#                indicador = 'NY.GDP.MKTP.CD')

basepib <- WDI(country = 'all',
               indicator = 'NY.GDP.MKTP.CD')

# Adicionando uma coluna de cor para o Brasil
#dadospib$cor <- ifelse(dadospib$iso2c == 'BR', 'Brasil', 'Outros')

library(dplyr)

basepib <- basepib %>%
  mutate(cor = ifelse(iso2c == "BR", "red", "gray"))

# Criar o gráfico
#grafpainel <- ggplot(dados = dadospib,
#                     aes(x = ano, y = NY.GDP.MKTP.CD, cor = cor)) +
#  geom_point(alpha = 0.6, size = 1.5) + # Adicionando pontos com transparência e tamanho personalizado
#  scale_color_manual(values ​​= c('Brasil' = 'red', 'Outros' = 'darkgrey')) + # Definir como núcleos
#  labs(title = "Produto Interno Bruto (PIB) dos Países ao Longo do Tempo",
#       x = "Ano",
#       y = "PIB (em USD)",
#       color = "País") + # Adicionar título e rótulos aos eixos
#  theme_minimal() + # Usar tema minimalista para um visual mais moderno
#  tema(
#    plot.title = element_text(hjust = 0.5, tamanho = 16, face = "negrito"),
#    axis.title.x = element_text(tamanho = 12),
#    axis.title.y = element_text(tamanho = 12),
#    legend.position = "top", # Colocando a legenda no topo
#    legenda.título = elemento_texto(tamanho = 12),
#    legend.text = element_text(tamanho = 10)
#  )

library(ggplot2)

# Baixar dados do PIB
basepib <- WDI(country = 'all', indicator = 'NY.GDP.MKTP.CD')

# Criar coluna de cor: Brasil em vermelho, outros em cinza
basepib <- basepib %>%
  mutate(cor = ifelse(iso2c == "BR", "Brasil", "Outros"))

# Gráfico
grafpainel <- ggplot(data = basepib, 
                     aes(x = year, y = NY.GDP.MKTP.CD, color = cor)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c('Brasil' = 'red', 'Outros' = 'darkgrey')) +
  labs(
    title = "Produto Interno Bruto (PIB) dos Países ao Longo do Tempo",
    x = "Ano",
    y = "PIB (em USD)",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Exibir gráfico
print(grafpainel)

##### NÃO EXECUTAR ESTA PARTE
### PROMPT SEM CHAT

# Usando a biblioteca WDI, coletamos estes dados (em painel):
  
  # DADOS EM PAINEL
#  dadospib <- WDI(país = 'todos',
#                  indicador = 'NY.GDP.MKTP.CD')

# Estruturou-se este gráfico simples:
  
#  grafpainel <- ggplot(dados = dadospib,
#                       mapeamento = aes(y = NY.GDP.MKTP.CD,
#                                        x = ano)) +
#  geom_point()

# Como fazer um gráfico mais moderno, incluindo:
  
# 1) Adicione um título ao gráfico
# 2) Renomeie o eixo e como PIB
# 3) Renomeie o eixo x como Ano
# 4) Utilize núcleos modernos
# 5) Coloque os dados do Brasil em vermelho

# Criar coluna de cor: Brasil ou Outros
basepib <- basepib %>%
  mutate(cor = ifelse(iso2c == "BR", "Brasil", "Outros"))

# Separar Brasil dos Outros
brasil <- basepib %>% filter(iso2c == "BR")
outros <- basepib %>% filter(iso2c != "BR")

# Gráfico com linha para o Brasil e pontos para os outros
grafpainel <- ggplot() +
  # Outros países: pontos
  geom_point(data = outros, aes(x = year, y = NY.GDP.MKTP.CD),
             color = "#A8A8A8", alpha = 0.5, size = 1.5) +
  # Brasil: linha forte
  geom_line(data = brasil, aes(x = year, y = NY.GDP.MKTP.CD),
            color = "#E63946", size = 1.2) +
  # Labels e tema
  labs(
    title = "Produto Interno Bruto (PIB) - Brasil em Destaque",
    x = "Ano",
    y = "PIB (em USD)",
    color = "País"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none", # Sem legenda porque o destaque visual já é suficiente
    panel.grid.minor = element_blank()
  )

# Exibir gráfico
print(grafpainel)





# Criar coluna de grupo (quem é destaque)
basepib <- basepib %>%
  mutate(grupo = case_when(
    iso2c == "BR" ~ "Brasil",
    iso2c == "US" ~ "Maiores",
    iso2c == "CN" ~ "Maiores",
    iso2c == "JP" ~ "Maiores",
    iso2c == "DE" ~ "Médios",
    iso2c == "FR" ~ "Médios",
    iso2c == "GB" ~ "Médios",
    iso2c == "MX" ~ "Menores",
    iso2c == "ZA" ~ "Menores",
    TRUE ~ "Outros"
  ))

# Separar os dados
destaques <- basepib %>% filter(grupo != "Outros")
outros <- basepib %>% filter(grupo == "Outros")

# Cores para os grupos
cores_grupo <- c(
  "Brasil" = "#E63946",
  "Maiores" = "#2A9D8F",
  "Médios" = "#F4A261",
  "Menores" = "#333333"
)

# Gráfico
grafpainel <- ggplot() +
  # Outros países: pontos discretos
  geom_point(data = outros, aes(x = year, y = NY.GDP.MKTP.CD),
             color = "#A8A8A8", alpha = 0.5, size = 1.5) +
  # Países destaque: linhas
  geom_line(data = destaques, aes(x = year, y = NY.GDP.MKTP.CD, color = grupo),
            size = 1.2) +
  # Cores manuais
  scale_color_manual(values = cores_grupo) +
  labs(
    title = "Produto Interno Bruto (PIB) - Brasil e Outros Países em Destaque",
    x = "Ano",
    y = "PIB (em USD)",
    color = "Grupo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Exibir gráfico
print(grafpainel)





# Remover entradas que não são países (os 7 primeiros)
basepib <- basepib %>% 
  filter(!iso2c %in% c("1A", "S3", "S4", "EU", "OE", "V1", "V2"))

# Criar coluna de país destaque
basepib <- basepib %>%
  mutate(pais = case_when(
    iso2c == "BR" ~ "Brasil",
    iso2c == "US" ~ "Estados Unidos",
    iso2c == "CN" ~ "China",
    iso2c == "JP" ~ "Japão",
    iso2c == "DE" ~ "Alemanha",
    iso2c == "FR" ~ "França",
    iso2c == "GB" ~ "Reino Unido",
    iso2c == "MX" ~ "México",
    iso2c == "ZA" ~ "África do Sul",
    TRUE ~ "Outros"
  ))

# Separar os dados
destaques <- basepib %>% filter(pais != "Outros")
outros <- basepib %>% filter(pais == "Outros")

# Cores específicas para cada país
cores_paises <- c(
  "Brasil" = "#E63946",
  "Estados Unidos" = "#006400", # Verde escuro
  "China" = "#228B22",           # Verde médio
  "Japão" = "#66CDAA",           # Verde claro
  "Alemanha" = "#FFD700",        # Amarelo forte
  "França" = "#FFEA00",          # Amarelo médio
  "Reino Unido" = "#FFF176",     # Amarelo claro
  "México" = "#003366",          # Azul escuro
  "África do Sul" = "#4682B4"    # Azul mais claro
)

# Gráfico
grafpainel <- ggplot() +
  # Outros países: pontos discretos
  geom_point(data = outros, aes(x = year, y = NY.GDP.MKTP.CD),
             color = "#A8A8A8", alpha = 0.5, size = 1.5) +
  # Países destaque: linhas com cores específicas
  geom_line(data = destaques, aes(x = year, y = NY.GDP.MKTP.CD, color = pais),
            size = 1.2) +
  # Aplicar cores manuais
  scale_color_manual(values = cores_paises) +
  labs(
    title = "Produto Interno Bruto (PIB) - Brasil e Países em Destaque",
    x = "Ano",
    y = "PIB (em USD)",
    color = "País"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Exibir gráfico
print(grafpainel)





# Lista atualizada de países (sem Espanha)
paises_iniciais <- c("BR", "US", "CN", "JP", "DE", "ZA", "IN")

# Filtrar apenas os países desejados
basepib <- basepib %>% 
  filter(iso2c %in% paises_iniciais)

# Criar coluna de país destaque
basepib <- basepib %>%
  mutate(pais = case_when(
    iso2c == "BR" ~ "Brasil",
    iso2c == "US" ~ "Estados Unidos",
    iso2c == "CN" ~ "China",
    iso2c == "JP" ~ "Japão",
    iso2c == "DE" ~ "Alemanha",
    iso2c == "ZA" ~ "África do Sul",
    iso2c == "IN" ~ "Índia",
    TRUE ~ "Outros"
  ))

# Separar dados
destaques <- basepib %>% filter(pais != "Outros")

# Atualizar cores (sem Espanha)
cores_paises <- c(
  "Brasil" = "#000000",          # Preto
  "Estados Unidos" = "#4169E1",  # Azul Royal
  "China" = "#DC143C",           # Carmesim
  "Japão" = "#8A2BE2",           # Azul Violeta
  "Alemanha" = "#32CD32",        # Verde Lima
  "África do Sul" = "#00CED1",   # Turquesa
  "Índia" = "#FFD700"            # Amarelo Ouro
)

# Últimos pontos para adicionar sigla
ultimos_pontos <- destaques %>%
  group_by(pais) %>%
  filter(year == max(year)) %>%
  ungroup()

# Encontrar máximo do PIB
max_pib <- max(destaques$NY.GDP.MKTP.CD, na.rm = TRUE)

# Criar sequências de valores para o eixo Y
breaks_y <- seq(0, max_pib, by = 2e12) # A cada 2 trilhões

# Definir anos de 5 em 5 no eixo X
breaks_x <- seq(min(destaques$year), max(destaques$year), by = 5)

# Gráfico
grafpainel <- ggplot() +
  geom_line(data = destaques, aes(x = year, y = NY.GDP.MKTP.CD, color = pais),
            size = 1.2) +
  geom_text(data = ultimos_pontos,
            aes(x = year, y = NY.GDP.MKTP.CD, label = iso2c),
            color = "black",
            hjust = 0,
            nudge_x = 0.5,
            size = 4,
            fontface = "bold") +
  scale_color_manual(values = cores_paises) +
  scale_y_continuous(
    breaks = breaks_y,
    labels = function(x) paste0("US$ ", formatC(x / 1e9, format = "f", digits = 0), " Bi")
  ) +
  scale_x_continuous(breaks = breaks_x) +
  labs(
    title = "Produto Interno Bruto (PIB) - Países Selecionados",
    x = "Ano",
    y = "PIB (Bilhões de USD)",
    color = "País"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(x = max(destaques$year) + 3)

# Exibir gráfico
print(grafpainel)