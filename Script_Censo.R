# Instalação dos pacotes que vão ser utilizados.
#install.packages("readr", dependencies = TRUE)
#install.packages("dplyr")
#install.packages("naniar")

#pacotes utilizados
library(readr)
library(naniar)
library(dplyr)
library(writexl)
library(purrr)
library(tidyr)

# Usando o pacote readr para importar os dados.
library(readr)

dados = read_csv2(file = "microdados_ed_basica_2024.csv" , 
                  locale = locale(encoding = "ISO-8859-1"))


# Usando o pacote dplyr para as manipulações dos dados.
library(dplyr)  

dados = dados |> filter(SG_UF == "RJ") # Usando para filtrar as escolas somente do RJ.

#Selecionando as colunas relevantes
dados = dados |> select(NO_MUNICIPIO,CO_MUNICIPIO,TP_DEPENDENCIA, IN_AGUA_POTAVEL,IN_TRATAMENTO_LIXO_INEXISTENTE,IN_BANHEIRO_PNE, IN_SALA_ATENDIMENTO_ESPECIAL, IN_ACESSIBILIDADE_RAMPAS, IN_ACESSIBILIDADE_ELEVADOR,IN_ACESSIBILIDADE_VAO_LIVRE,IN_ACESSIBILIDADE_PISOS_TATEIS, IN_ACESSIBILIDADE_SINAL_SONORO,IN_ACESSIBILIDADE_SINAL_TATIL,QT_SALAS_UTILIZA_CLIMATIZADAS,QT_SALAS_UTILIZADAS,QT_SALAS_UTILIZADAS_ACESSIVEIS,IN_INTERNET,QT_PROF_PSICOLOGO,QT_PROF_NUTRICIONISTA) 

#Transformando os valores em fatores
factors = c("CO_MUNICIPIO","TP_DEPENDENCIA", "IN_AGUA_POTAVEL","IN_TRATAMENTO_LIXO_INEXISTENTE","IN_BANHEIRO_PNE", "IN_SALA_ATENDIMENTO_ESPECIAL", "IN_ACESSIBILIDADE_RAMPAS", "IN_ACESSIBILIDADE_ELEVADOR","IN_ACESSIBILIDADE_VAO_LIVRE","IN_ACESSIBILIDADE_PISOS_TATEIS", "IN_ACESSIBILIDADE_SINAL_SONORO","IN_ACESSIBILIDADE_SINAL_TATIL","IN_INTERNET")

dados[factors] = lapply(dados[factors], factor)


# Manipulação da coluna:TP_DEPENDENCIA. Pois vamos usar essa coluna para todos os indicadores.
dados = dados |> mutate(CO_MUNICIPIO = factor(CO_MUNICIPIO)) |> 
  mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~ "Federal",
    TP_DEPENDENCIA == 2 ~ "Estadual",
    TP_DEPENDENCIA == 3 ~ "Municipal",
    TP_DEPENDENCIA == 4 ~ "Privada"
  ))

### 1. Infraestrutura
#### 1.1 Proporção de Escolas com Fornecimento de Agua Potável

# Manipulação da coluna: IN_AGUA_POTAVEL.
agua_potavel = dados |> mutate(IN_AGUA_POTAVEL = case_when(
  IN_AGUA_POTAVEL == 1 ~ "Sim",
  IN_AGUA_POTAVEL == 0 ~ "Não"
))

# Selecionando apenas as colunas importantes para o uso.
agua_potavel = agua_potavel |> select(CO_MUNICIPIO, NO_MUNICIPIO,
                                      TP_DEPENDENCIA,IN_AGUA_POTAVEL)


# Carregando o pacote para análise dos dados faltantes
library(naniar)

# Resumo dos valores faltantes
miss_var_summary(data = agua_potavel)

# Indicador de proporção
agua_potavel = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_AGUA_POTAVEL
  ) |>
  mutate( #transforma as opçoes 0 e 1 em nao e sim
    IN_AGUA_POTAVEL = case_when(
      IN_AGUA_POTAVEL == 1 ~ "Sim",
      IN_AGUA_POTAVEL == 0 ~ "Não",
      TRUE ~ NA_character_
    )
  ) |>#calcula o total geral
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |> #vai agrupar os valores e usar a formula para cada tipo de dependencia
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    agua_potavel_porc = round((sum(IN_AGUA_POTAVEL == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows( #adiciona uma linha pro total geral
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_AGUA_POTAVEL
      ) |>
      mutate(
        IN_AGUA_POTAVEL = case_when(
          IN_AGUA_POTAVEL == 1 ~ "Sim",
          IN_AGUA_POTAVEL == 0 ~ "Não",
          TRUE ~ NA_character_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise( #vai agrupar e depois somar os valores e usar a formula para o total de escolas 
        total_escolas_por_dependencia = n(),
        agua_potavel_porc = round((sum(IN_AGUA_POTAVEL == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |> #vai juntar os resultados numa tabela so
  pivot_wider( #transforma  a coluna tp_dependencia em formato wide (linha)
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, agua_potavel_porc),
    values_fill = list(total_escolas_por_dependencia = 0, agua_potavel_porc = 0)
  )

############# A partir daqui farei tudo de forma mais resumida, para os indicadores

#### 1.2 Proporção de tratamento de resíduos

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
trat_res = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_TRATAMENTO_LIXO_INEXISTENTE
  ) |>
  mutate(
    IN_TRATAMENTO_LIXO_INEXISTENTE = case_when(
      IN_TRATAMENTO_LIXO_INEXISTENTE == 1 ~ "Não",
      IN_TRATAMENTO_LIXO_INEXISTENTE == 0 ~ "Sim",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    trat_res_porc = round((sum(IN_TRATAMENTO_LIXO_INEXISTENTE == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_TRATAMENTO_LIXO_INEXISTENTE
      ) |>
      mutate(
        IN_TRATAMENTO_LIXO_INEXISTENTE = case_when(
          IN_TRATAMENTO_LIXO_INEXISTENTE == 1 ~ "Não",
          IN_TRATAMENTO_LIXO_INEXISTENTE == 0 ~ "Sim",
          TRUE ~ NA_character_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        trat_res_porc = round((sum(IN_TRATAMENTO_LIXO_INEXISTENTE == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, trat_res_porc),
    values_fill = list(total_escolas_por_dependencia = 0, trat_res_porc = 0)
  )

### 2. Acessibilidade 
#### 2.1 Proporção de escolas com presença de banheiros acessíveis


# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
banh_acess = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_BANHEIRO_PNE
  ) |>
  mutate(
    IN_BANHEIRO_PNE = case_when(
      IN_BANHEIRO_PNE == 1 ~ "Sim",
      IN_BANHEIRO_PNE == 0 ~ "Não",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    banh_acess_porc = round((sum(IN_BANHEIRO_PNE == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_BANHEIRO_PNE
      ) |>
      mutate(
        IN_BANHEIRO_PNE = case_when(
          IN_BANHEIRO_PNE == 1 ~ "Sim",
          IN_BANHEIRO_PNE == 0 ~ "Não",
          TRUE ~ NA_character_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        banh_acess_porc = round((sum(IN_BANHEIRO_PNE == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, banh_acess_porc),
    values_fill = list(total_escolas_por_dependencia = 0, banh_acess_porc = 0)
  )


#### 2.2 Proporção da presença de sala para Atendimento Educacional Especializado (AEE)

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
salas_aae = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_SALA_ATENDIMENTO_ESPECIAL
  ) |>
  mutate(
    IN_SALA_ATENDIMENTO_ESPECIAL = case_when(
      IN_SALA_ATENDIMENTO_ESPECIAL == 1 ~ "Sim",
      IN_SALA_ATENDIMENTO_ESPECIAL == 0 ~ "Não",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    salas_aae_porc = round((sum(IN_SALA_ATENDIMENTO_ESPECIAL == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_SALA_ATENDIMENTO_ESPECIAL
      ) |>
      mutate(
        IN_SALA_ATENDIMENTO_ESPECIAL = case_when(
          IN_SALA_ATENDIMENTO_ESPECIAL == 1 ~ "Sim",
          IN_SALA_ATENDIMENTO_ESPECIAL == 0 ~ "Não",
          TRUE ~ NA_character_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        salas_aae_porc = round((sum(IN_SALA_ATENDIMENTO_ESPECIAL == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, salas_aae_porc),
    values_fill = list(total_escolas_por_dependencia = 0, salas_aae_porc = 0)
  )
#### 2.3 Proporção de salas climatizadas por escola

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
salas_clim = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    QT_SALAS_UTILIZADAS, QT_SALAS_UTILIZA_CLIMATIZADAS
  ) |>
  mutate(
    prop_salas = case_when(
      QT_SALAS_UTILIZADAS > 0 ~ QT_SALAS_UTILIZA_CLIMATIZADAS / QT_SALAS_UTILIZADAS,
      TRUE ~ NA_real_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    salas_clim_porc = round(mean(prop_salas, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        QT_SALAS_UTILIZADAS, QT_SALAS_UTILIZA_CLIMATIZADAS
      ) |>
      mutate(
        prop_salas = case_when(
          QT_SALAS_UTILIZADAS > 0 ~ QT_SALAS_UTILIZA_CLIMATIZADAS / QT_SALAS_UTILIZADAS,
          TRUE ~ NA_real_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        salas_clim_porc = round(mean(prop_salas, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, salas_clim_porc),
    values_fill = list(total_escolas_por_dependencia = 0, salas_clim_porc = 0)
  )

#### 2.4 Proporção de salas acessíveis por escola

salas_acess = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    QT_SALAS_UTILIZADAS_ACESSIVEIS,
    QT_SALAS_UTILIZADAS
  ) |>
  mutate(
    prop_salas_acess = case_when(
      QT_SALAS_UTILIZADAS > 0 ~ QT_SALAS_UTILIZADAS_ACESSIVEIS / QT_SALAS_UTILIZADAS,
      TRUE ~ NA_real_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    prop_media_salas_acess = round(mean(prop_salas_acess, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        QT_SALAS_UTILIZADAS_ACESSIVEIS,
        QT_SALAS_UTILIZADAS
      ) |>
      mutate(
        prop_salas_acess = case_when(
          QT_SALAS_UTILIZADAS > 0 ~ QT_SALAS_UTILIZADAS_ACESSIVEIS / QT_SALAS_UTILIZADAS,
          TRUE ~ NA_real_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        prop_media_salas_acess = round(mean(prop_salas_acess, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, prop_media_salas_acess),
    values_fill = list(total_escolas_por_dependencia = 0, prop_media_salas_acess = 0)
  )

#### 2.5 Proporção de acessibilidade para cadeirantes

acess_cadeirantes = dados |>
  mutate(
    ACESS_CADEIRANTES = case_when(
      IN_ACESSIBILIDADE_VAO_LIVRE == 1 &
        (IN_ACESSIBILIDADE_RAMPAS == 1 | IN_ACESSIBILIDADE_ELEVADOR == 1) ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    acess_cadeirantes_porc = round(mean(ACESS_CADEIRANTES, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      mutate(
        ACESS_CADEIRANTES = case_when(
          IN_ACESSIBILIDADE_VAO_LIVRE == 1 &
            (IN_ACESSIBILIDADE_RAMPAS == 1 | IN_ACESSIBILIDADE_ELEVADOR == 1) ~ 1,
          TRUE ~ 0
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise( 
        total_escolas_por_dependencia = n(), 
        acess_cadeirantes_porc = round(mean(ACESS_CADEIRANTES, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) |>
      mutate( 
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, acess_cadeirantes_porc),
    values_fill = list(total_escolas_por_dependencia = 0, acess_cadeirantes_porc = 0)
  )

#### 2.6 Proporção sinalização para deficientes visuais

library(dplyr)
library(tidyr)

sinalizacao_visual = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_ACESSIBILIDADE_PISOS_TATEIS,
    IN_ACESSIBILIDADE_SINAL_TATIL,
    IN_ACESSIBILIDADE_SINAL_SONORO
  ) |>
  mutate(
    SINALIZACAO_VISUAL = case_when(
      IN_ACESSIBILIDADE_PISOS_TATEIS == 1 |
        IN_ACESSIBILIDADE_SINAL_TATIL == 1 |
        IN_ACESSIBILIDADE_SINAL_SONORO == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    sinalizacao_visual_porc = round(mean(SINALIZACAO_VISUAL, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_ACESSIBILIDADE_PISOS_TATEIS,
        IN_ACESSIBILIDADE_SINAL_TATIL,
        IN_ACESSIBILIDADE_SINAL_SONORO
      ) |>
      mutate(
        SINALIZACAO_VISUAL = case_when(
          IN_ACESSIBILIDADE_PISOS_TATEIS == 1 |
            IN_ACESSIBILIDADE_SINAL_TATIL == 1 |
            IN_ACESSIBILIDADE_SINAL_SONORO == 1 ~ 1,
          TRUE ~ 0
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        sinalizacao_visual_porc = round(mean(SINALIZACAO_VISUAL, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, sinalizacao_visual_porc),
    values_fill = list(total_escolas_por_dependencia = 0, sinalizacao_visual_porc = 0)
  )

### 3. Equipamentos
#### 3.1 Proporção de escolas com acesso à internet

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
acess_inter = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    IN_INTERNET
  ) |>
  mutate(
    IN_INTERNET = case_when(
      IN_INTERNET == 1 ~ "Sim",
      IN_INTERNET == 0 ~ "Não",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    acess_inter_porc = round((sum(IN_INTERNET == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        IN_INTERNET
      ) |>
      mutate(
        IN_INTERNET = case_when(
          IN_INTERNET == 1 ~ "Sim",
          IN_INTERNET == 0 ~ "Não",
          TRUE ~ NA_character_
        )
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        acess_inter_porc = round((sum(IN_INTERNET == "Sim", na.rm = TRUE) / total_escolas_por_dependencia) * 100, 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, acess_inter_porc),
    values_fill = list(total_escolas_por_dependencia = 0, acess_inter_porc = 0)
  )


### 4. Recursos Humanos
#### 4.1 Média de psicologos por escola

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
media_psic = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    QT_PROF_PSICOLOGO
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    media_psicologo = round(mean(QT_PROF_PSICOLOGO, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        QT_PROF_PSICOLOGO
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        media_psicologo = round(mean(QT_PROF_PSICOLOGO, na.rm = TRUE), 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, media_psicologo),
    values_fill = list(total_escolas_por_dependencia = 0, media_psicologo = 0)
  )

#### 4.2 Média de nutricionistas por escola

# Selecionando as colunas,manipulação dos dados e seus agrupamentos.
media_nutri = dados |>
  select(
    CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA,
    QT_PROF_NUTRICIONISTA
  ) |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
  mutate(total_escolas_municipio_geral = n()) |>
  ungroup() |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA, total_escolas_municipio_geral) |>
  summarise(
    total_escolas_por_dependencia = n(),
    media_nutri = round(mean(QT_PROF_NUTRICIONISTA, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  bind_rows(
    dados |>
      select(
        CO_MUNICIPIO, NO_MUNICIPIO,
        QT_PROF_NUTRICIONISTA
      ) |>
      group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
      summarise(
        total_escolas_por_dependencia = n(),
        media_nutri = round(mean(QT_PROF_NUTRICIONISTA, na.rm = TRUE), 2),
        .groups = "drop"
      ) |>
      mutate(
        TP_DEPENDENCIA = "Geral",
        total_escolas_municipio_geral = total_escolas_por_dependencia
      )
  ) |>
  arrange(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = c(total_escolas_por_dependencia, media_nutri),
    values_fill = list(total_escolas_por_dependencia = 0, media_nutri = 0)
  )

teste =select(dados,QT_PROF_PSICOLOGO,NO_MUNICIPIO) |> filter(NO_MUNICIPIO == "Barra Mansa")




########################## Para finalizar, vamos fazer a junção das tabelas.

# Vamos usar esse pacote.
library(purrr)

tabelas <- list(
  acess_cadeirantes,
  acess_inter,
  agua_potavel,
  banh_acess,
  media_nutri,
  media_psic,
  salas_aae,
  salas_acess,
  salas_clim,
  sinalizacao_visual,
  trat_res
)

tabelas <- purrr::map(tabelas, ~ dplyr::select(.x, -total_escolas_municipio_geral))

chaves_juncao <- c(
  "CO_MUNICIPIO",
  "NO_MUNICIPIO",
  "total_escolas_por_dependencia_Estadual",
  "total_escolas_por_dependencia_Federal",
  "total_escolas_por_dependencia_Municipal",
  "total_escolas_por_dependencia_Privada",
  "total_escolas_por_dependencia_Geral"
)

base_final <- purrr::reduce(tabelas, ~ dplyr::inner_join(.x, .y, by = chaves_juncao))

#exportar para o excel
library(writexl)

write_xlsx(base_final, "base_final.xlsx")




