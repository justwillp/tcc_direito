########### TCC em Direito (REIS, 2023) #################
########### 1 - Pacotes necessários     #################
install.packages(pacman)
pacman::p_load(magrittr,tidyverse,lubridate,data.table)

########### 2 - Temas que serão usados para gerar os graficos #########
tema_sem_legenda1 <- theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        plot.title = element_text(hjust = .5, 
                                  size = 12*1.8, 
                                  face = "bold"), 
        axis.title = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.text = element_text(size = 20), 
        axis.text.x = element_text(face = "bold"),
        text = element_text(family = "Garamond"),
        plot.caption = element_text(size = 16))

tema_com_legenda <- theme_bw() + 
  theme(legend.position = "top", 
        legend.text = element_text(size= 15),
        legend.title = element_blank(),
        axis.title.x = element_blank(), 
        plot.title = element_text(hjust = .5, 
                                  size = 12*1.8, 
                                  face = "bold"), 
        axis.title = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.text = element_text(size = 20), 
        axis.text.x = element_text(face = "bold"),
        text = element_text(family = "Garamond"),
        plot.caption = element_text(size = 16))

tema_sem_legenda2 <- theme_bw() + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 12*1.5),
        legend.text = element_text(size= 15),
        legend.title = element_blank(),
        axis.title.x = element_blank(), 
        plot.title = element_text(hjust = .5, 
                                  size = 12*1.8, 
                                  face = "bold"), 
        axis.title = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.text = element_text(size = 20), 
        axis.text.x = element_text(face = "bold"),
        text = element_text(family = "Garamond"),
        plot.caption = element_text(size = 16))

################# 4 - Importando tabela ######################
tabela <- fread("tabela_base.csv", sep = "\t") %>% 
  as_tibble() %>% select(-1) %>% mutate(disponibilizacao = ymd(disponibilizacao))

################# 5 - Definindo marcadores ###################
marcadores_pix <- c("PIX", "Pagamento instantâneo",
                   "pagamento instantaneo")

marcadores_ted <- c("^TED$", "Transferência eletrônica disponível",
                   "Transferencia eletronica disponivel")

marcadores_doc <- c("^DOC$", "Documento de ordem de crédito",
                   "Documento de ordem de credito")

marcadores_fraude <- c("Fraude","Fraudulento","Fraudou")

marcadores_golpe <- c("golpe","golpista")

marcadores_estelionato <- c("estelionato","Art. 171")

marcadores_extorsao <- c("extorsão", "extorsao", "violência",
                        "ameaç",  "violencia", "Art. 158")

marcadores_sequestro <- c("sequestro", "Art. 159")

marcadores_lgpd <- c("LGPD", "Vazamento", "Dados pessoais", "Chave pix")

marcadores_credito <- c("Cartão de crédito", "Cartao de credito", "de credito",
                       "de crédito", "no crédito", "no credito")

marcadores_debito <- c("Cartão de débito", "Cartao de debito", "de debito",
                      "de debito", "no debito")

marcadores_cheque <- c("Cheque")

marcadores_resp_banco <- c("Súmula nº 479", "Sumula 479", "Súmula n 479", "Sumula n 479", "Art 14", 
                          "Art. 14", "Culpa concorrente")

marcadores_resp_autor <- c("§3º, II", "§3º", "Exclusiva da vítima", "exclusiva da vitima", "exclusiva d", "Culpa concorrente")


marcadores_provimento <- c("Defiro o",
                          "Julgo procedente",
                          "Acolho a",
                          "Concedo a",
                          "Julgo favoravelmente",
                          "Dou provimento",
                          "Provido",
                          "Determino o atendimento",
                          "Concedo o",
                          "Acato o",
                          "reformada",
                          "Condeno o réu")

marcadores_nao_provimento <- c("Indefiro",
                               "Não provido",
                               "Improcede",
                               "Não acolho",
                               "Não vislumbro",
                               "Julgo improcedente",
                               "Julgo improcedentes",
                               "Não há razão para acolher",
                               "Não se verifica a existência do direito",
                               "Ausência de provas suficientes",
                               "Não é possível conceder",
                               "mantida",
                               "Condeno o autor")

marcadores_legitimidade <- c("legitimidade")

marcadores_parcial <- c("em parte", "parcialmente")

################# 6 - Filtrando os julgamentos mais relevantes ####################
mais_relevantes <- tabela %>% 
  mutate(IPIX = case_when(
    julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_pix, collapse = "|"))) ~ 1,
    TRUE ~ 0),
    IDOC = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_doc, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ITED = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_ted, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IFRAUDE = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_fraude, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IGOLPE = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_golpe, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IESTELIONATO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_estelionato, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IEXTORSAO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_extorsao, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ISEQUESTRO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_sequestro, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ILGPD = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_lgpd, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ICREDITO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_credito, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IDEBITO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_debito, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ICHEQUE = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_cheque, collapse = "|"))) ~ 1,
      TRUE ~ 0), 
    IRESP_BANCO =  case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_resp_banco, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IRESP_USUARIO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_resp_autor, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IPROVIMENTO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_provimento, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    INPROVIMENTO = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_nao_provimento, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    IPROVPARCIAL = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_parcial, collapse = "|"))) ~ 1,
      TRUE ~ 0),
    ILEGITIMIDADE = case_when(
      julgado %>%  str_detect(paste0("(?i)",paste0(marcadores_legitimidade, collapse = "|"))) ~ 1,
      TRUE ~ 0)) %>% 
  select(1,disponibilizacao,IPIX:ILEGITIMIDADE) %>% 
  mutate(Soma = rowSums(.[,3:14])) %>% 
  arrange(Soma %>% desc())

classes_interesse <- c("Procedimento Comum Cível",
                       "Procedimento do Juizado Especial Cível")

filtrado_relevantes <- mais_relevantes %>%
  inner_join(tabela, by = c("processo", "disponibilizacao")) %>%
  filter(classe %in% classes_interesse, Soma >= 1) %>%
  distinct()

filtrado_relevantes


################# 7 - Quem está sendo demandado? ####################
Requeridos_base <- filtrado_relevantes %>%
  select(processo,disponibilizacao,julgado) %>%
  mutate(Requerido = str_extract(julgado,"(?<=Requerido:|Réu)(.*?)((?i)\\sS/A|\\sS.A.|\\sSA\\s|\\sSA\\.|
|\\sS/A.\\sLtda.)") %>%
           str_remove_all(",") %>%  str_squish() %>% str_to_sentence())

Requeridos <- Requeridos_base %>% select(processo,disponibilizacao,julgado,Requerido) %>%
  mutate(Requerido_Padronizado = case_when(
    Requerido %>% str_detect("(?i)BMG") ~ "Banco BMG",
    Requerido %>% str_detect("(?i)\\sPAN\\s") ~ "Banco PAN",
    Requerido %>% str_detect("(?i)Bradesco") ~ "Banco Bradesco",
    Requerido %>% str_detect("(?i)Santander") ~ "Banco Santander",
    Requerido %>% str_detect("(?i)Cetelem") ~ "Banco Cetelem",
    Requerido %>% str_detect("(?i)Mercantil do Brasil") ~ "Banco Mercantil do Brasil",
    Requerido %>% str_detect("(?i)Banco do brasil") ~ "Banco do Brasil",
    Requerido %>% str_detect("(?i)c6|c 6") ~ "Banco C6",
    Requerido %>% str_detect("(?i)Itau|Itaú") ~ "Banco Itaú",
    Requerido %>% str_detect("(?i)Nu pagamentos|Nubank") ~ "NuBank",
    Requerido %>% str_detect("(?i)ficsa") ~ "Banco Ficsa",
    Requerido %>% str_detect("(?i)panamericano") ~ "Banco panamericano",
    Requerido %>% str_detect("(?i)\\sinter\\s") ~ "Banco Inter",
    Requerido %>% str_detect("(?i)safra") ~ "Banco Safra",
    Requerido %>% str_detect("(?i)Aymoré|Aymore") ~ "Aymoré",
    Requerido %>% str_detect("(?i)daycoval") ~ "Daycoval",
    Requerido %>% str_detect("(?i)Claro") ~ "Claro",
    Requerido %>% str_detect("(?i)Banco original") ~ "Original",
    Requerido %>% str_detect("(?i)Picpay|pic pay") ~ "Picpay",
    Requerido %>% str_detect("(?i)pagseguro|pagsseguro") ~ "PagSeguro",
    Requerido %>% str_detect("(?i)Decolar") ~ "Decolar",
    Requerido %>% str_detect("(?i)Tim celular|Tim s a|Tim s/a|Tim s.a") ~ "Tim",
    Requerido %>% str_detect("Oi móvel") ~ "Oi",
    Requerido %>% str_detect("(?i)Bonsucesso") ~ "Banco Bonsucesso",
    Requerido %>% str_detect("(?i)Bv financeira|B.v financeira|votorantim|B.v. financeira|votorantim") ~ "Banco Votorantim",
    Requerido %>% str_detect("(?i)Cielo") ~ "Cielo",
    Requerido %>% str_detect("(?i)Mastercard") ~ "Mastercard",
    Requerido %>% str_detect("(?i)VISA") ~ "VISA",
    Requerido %>% str_detect("(?i)Telefônica brasil|Telefonica brasil|Tefônica brasil|vivo") ~ "VIVO",
    Requerido %>% str_detect("(?i)Agibank") ~ "Agibank",
    Requerido %>% str_detect("(?i)Cruzeiro do sul") ~ "Banco Cruzeiro do Sul",
    Requerido %>% str_detect("(?i)Portoseg|porto seguro") ~ "Porto Seguro",
    Requerido %>% str_detect("(?i)Redecard") ~ "Redecard",
    Requerido %>% str_detect("(?i)Sicredi") ~ "Banco Sicredi",
    Requerido %>% str_detect("(?i)Intermedium") ~ "Banco Intermedium",
    Requerido %>% str_detect("(?i)Losango") ~ "Banco Losango",
    Requerido %>% str_detect("(?i)rci brasil") ~ "Banco RCI Brasil",
    Requerido %>% str_detect("(?i)sifra") ~ "Sifra",
    Requerido %>% str_detect("(?i)Olé cons") ~ "Banco Olé",
    Requerido %>% str_detect("(?i)BTG") ~ "Banco BTG",
    Requerido %>% str_detect("(?i)Crefisa") ~ "Crefisa"
  )) %>% select(1,2,5)

Requeridos %>% 
  count(Requerido_Padronizado) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))

################# 8 - Assuntos ####################
assuntos_count <- filtrado_relevantes %>% 
  count(assunto, sort = T) 

assuntos_count %>% 
  head(6) %>% 
  rbind(c("Outros", assuntos_count %>% 
            tail(nrow(.)-6) %>% 
            summarise(n = sum(n)) %>% 
            select(n) %>% as.numeric())) %>% 
  mutate(n = as.numeric(n), 
         Perc = n/sum(n)) 


################# 9 - Entrada geral dos gráficos ####################
entrada <- filtrado_relevantes %>% 
  mutate(IExtorsaoCSequestro = IEXTORSAO*ISEQUESTRO) %>% 
  select(-Soma) %>% 
  select(1,disponibilizacao, IExtorsaoCSequestro,contains("I")) %>%
  gather(Relevancia, Indicador,3:21) %>% 
  group_by(Ano_Mes = paste0(month(disponibilizacao), "-",year(disponibilizacao)) %>% 
             my(),Relevancia) %>% 
  arrange(Ano_Mes) %>% 
  summarise(Incidencia = sum(Indicador)) %>% 
  ungroup() %>% 
  group_by(Relevancia) %>% 
  arrange(Relevancia,Ano_Mes) %>% 
  mutate(Cumulado = 
           cumsum(Incidencia)/(first(Incidencia))) %>% 
  mutate(Cumulado = 
           cumsum(Incidencia)) %>% 
  mutate(Dummy_PIX = ifelse(Ano_Mes <= "2020-11" %>% ym(),0,1))

################# 9.1 - Entrada dos gráficos para crimes patrimoniais ####################
vg_patrimoniais_entry <- entrada %>% 
  filter(Relevancia %>% str_detect("IESTELIONATO|IExtorsaoCSequestro|IFRAUDE|IGOLPE")) %>% 
  mutate(Relevancia = case_when(
    Relevancia == "IESTELIONATO" ~ "Estelionato",
    Relevancia == "IExtorsaoCSequestro" ~ "Extorsão mediante\nsequestro",
    Relevancia == "IFRAUDE" ~ "Fraude",
    Relevancia == "IGOLPE" ~ "Golpe",
    TRUE ~ Relevancia)) 

vg_patrimoniais <- vg_patrimoniais_entry %>% 
  ggplot(aes(Ano_Mes,Incidencia, color = Relevancia)) + 
  geom_vline(xintercept = "16/11/2020" %>% dmy())  +
  geom_line(size = 4.8) +
  facet_wrap(~ Relevancia, scales = "free_y") +
  geom_text(inherit.aes = F, data = vg_patrimoniais_entry %>% 
              filter(Relevancia %>% str_detect("mediante")),
            aes(x = ymd("2020/08/10"), 
                y = max(Incidencia)*.8,
                label = "PIX", group = Relevancia),
            size = 5) +
  geom_text(inherit.aes = F, data = vg_patrimoniais_entry %>% 
              filter(Relevancia %>% str_detect("Fraude")),
            aes(x = ymd("2020/08/10"), 
                y = max(Incidencia)*.8, 
                label = "PIX", group = Relevancia),
            size = 5) +
  geom_text(inherit.aes = F, data = vg_patrimoniais_entry %>% 
              filter(Relevancia %>% str_detect("Estelionato")),
            aes(x = ymd("2020/08/10"), 
                y = max(Incidencia)*.8, 
                label = "PIX", group = Relevancia),
            size = 5) +
  geom_text(inherit.aes = F, data = vg_patrimoniais_entry %>% 
              filter(Relevancia %>% str_detect("^Golpe$")),
            aes(x = ymd("2020/08/10"), 
                y = max(Incidencia)*.8, 
                label = "PIX", group = Relevancia), 
            size = 5) +
  geom_smooth(method = "loess", se = F, 
              lty = "dashed", color = "black") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 20,
                              face = "bold"), 
    axis.title = element_blank(), 
    axis.text = element_text(size = 20)
  )

vg_patrimoniais
ggsave("./visao_geral_patrimoniais.png", 
       vg_patrimoniais, 
       width = 30, 
       height = 20, 
       units = "cm")


filtrado_relevantes %>% 
  filter(ITED == 1) %>% 
  select(ementa) %>% 
  sample_n(5) %>% 
  as.matrix() %>% 
  paste0(collapse = "\n\n\n") %>% 
  cat()


vg_meios_Pagamento_entry %>% 
  group_by(Ano = year(Ano_Mes), Relevancia) %>% 
  summarise(Incidencia = sum(Incidencia)) %>% 
  arrange(Relevancia) %>% 
  spread(2,3)


vg_patrimoniais_entry %>% 
  group_by(Ano = year(Ano_Mes), Relevancia) %>% 
  summarise(Incidencia = sum(Incidencia)) %>% 
  arrange(Relevancia) %>% 
  spread(2,3)

vg_patrimoniais_entry %>% 
  group_by(Ano = year(Ano_Mes), Relevancia) %>% 
  summarise(Incidencia = sum(Incidencia)) %>% ungroup() %>% group_by(Relevancia) %>% 
  arrange(Relevancia) %>% mutate(Var = (Incidencia - lag(Incidencia))/lag(Incidencia)) 
spread(2,3)

vg_meios_Pagamento_entry %>% 
  group_by(Ano = year(Ano_Mes), Relevancia) %>% 
  summarise(Incidencia = sum(Incidencia)) %>% ungroup() %>% group_by(Relevancia) %>% 
  arrange(Relevancia) %>% mutate(Var = (Incidencia - lag(Incidencia))/lag(Incidencia)) %>% 
  print(n = nrow(.))


vg_meios_Pagamento_entry <- entrada %>% 
  filter(Relevancia %>% str_detect("(?i)pix|credito|debito|cheque|doc|ted")) %>% 
  mutate(Relevancia = case_when(
    Relevancia == "ICHEQUE" ~ "Cheque",
    Relevancia == "ICREDITO" ~ "Crédito",
    Relevancia == "IDEBITO" ~ "Débito",
    Relevancia == "IDOC" ~ "DOC",
    Relevancia == "IPIX" ~ "PIX",
    Relevancia == "ITED" ~ "TED",
    TRUE ~ Relevancia)) 


ggplot(aes(Ano_Mes,Cumulado, color = Relevancia, 
           group = Dummy_PIX)) + 
  geom_line(size = 2) +
  geom_vline(xintercept = "16/11/2020" %>% dmy(), alpha = .5)  +
  facet_wrap(~ Relevancia, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15,
                              face = "bold"), 
    axis.title = element_blank(), 
    axis.text = element_text(size = 20)
  )


vg_meios_Pagamento
ggsave("./visao_geral_mpagamentos.png", 
       vg_meios_Pagamento, 
       width = 30, 
       height = 20, 
       units = "cm")


########################### Montando a tabela do zero ############################
# caso eu não tenha instalado preciso rodar:
#install.packages("remotes")
#remotes::install_github("jjesusfilho/tjsp")

#exp <- "(TRANSFERÊNCIA ELETRÔNICA OU TRANSFERENCIA ELETRONICA) E (Ilicito OU Ilícito OU Fraude)"

# Escolher uma pasta para receber os resultados
#pasta <- "./input/primeiro_grau"
# Vou criar essa pasta no meu computador para salvar os resultados:
#usethis::use_directory(pasta)
#tjsp::tjsp_baixar_cjpg(livre = exp,
#                       inicio = "01/01/2018",
#                       fim = "31/12/2018",
#                       diretorio = pasta)
#tjsp::tjsp_baixar_cjpg(livre = exp,
#                       inicio = "01/01/2019",
#                       fim = "31/12/2019",
#                       diretorio = pasta)

#tjsp::tjsp_baixar_cjpg(livre = exp,
#                       inicio = "01/01/2020",
#                       fim = "31/12/2020",
#                       diretorio = pasta)

#tjsp::tjsp_baixar_cjpg(livre = exp,
#                       inicio = "01/01/2021",
#                       fim = "31/12/2021",
#                       diretorio = pasta)

#tjsp::tjsp_baixar_cjpg(livre = exp,
#                       inicio = "01/01/2022",
#                       fim = "31/12/2022",
#                       diretorio = pasta)

#tabela <- tjsp::ler_cjpg(diretorio = pasta)
