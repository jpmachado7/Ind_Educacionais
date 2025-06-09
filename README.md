#  Indicadores Educacionais — Censo Escolar 2024

Este repositório apresenta um projeto de construção de indicadores educacionais com base nos dados do **Censo Escolar 2024**, com foco no estado do **Rio de Janeiro**.

---

##  Visão geral

Este projeto foi desenvolvido a partir de uma base de dados extensa do Censo Escolar 2024. O grupo realizou uma **seleção criteriosa de variáveis** consideradas relevantes para análise educacional, com base em discussões internas e objetivos previamente definidos.

O trabalho envolveu duas partes principais:

1. **Documental** — as variáveis escolhidas foram organizadas em um documento explicativo (`Indicadores_Censo.pdf`), onde estão indicadas através de **fórmulas matemáticas formais** que definem cada indicador construído.

2. **Computacional** — as fórmulas definidas no documento foram implementadas em um script em **linguagem R**, aplicado sobre os **microdados originais do INEP**. O resultado desse processamento foi uma planilha final (`base_final.xlsx`) com os indicadores já agregados por município para o estado do Rio de Janeiro.


---

##  Estrutura do repositório

- `Script_Censo.R` — script completo em R com todo o processo de limpeza e geração dos indicadores  
- `base_final.xlsx` — planilha final contendo os dados agregados por município  
- `Indicadores_Censo.pdf` — documento com explicações formais sobre a metodologia adotada  
- `microdados_ed_basica_2024.csv` — base de microdados do INEP (versão original, não modificada)  
- Arquivos auxiliares gerados pelo RStudio (`.Rproj`, `.RData`, etc.)

---

##  Fonte dos dados

Este projeto utiliza como insumo os **Microdados da Educação Básica 2024**, disponibilizados pelo **INEP (Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira)**. Trata-se de uma base nacional, pública e detalhada, com informações de todas as escolas brasileiras.

O repositório **não armazena diretamente o arquivo original completo**, devido ao seu tamanho. O link oficial para download está disponível abaixo:

 [Baixar microdados do Censo Escolar 2024 (INEP)](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip)

Após o download, utilize o script `Script_Censo.R` para reproduzir os tratamentos e gerar a planilha `base_final.xlsx` com os dados resumidos do estado do Rio de Janeiro.

---

##  Ferramentas utilizadas

Este projeto foi inteiramente desenvolvido em **R**, com apoio dos seguintes pacotes:

- tidyverse  
- dplyr  
- tidyr  
- readr  
- purrr  
- writexl  
- naniar  

Além disso, a documentação formal foi feita no **Overleaf (LaTeX)**.

---

##  Participantes

Este trabalho foi realizado pelo seguinte grupo de estudantes do **3º período da graduação em Estatística da Universidade Federal Fluminense (UFF)**:

- Beatriz Franco de Freitas  
- Davi Lucas de Jesus Caetano  
- Felipe Maia Rodrigues de Miranda  
- João Pedro Santos Machado  
- Kiane Sassaki Menezes  
- Paolla Pinheiro Pacheco  

---