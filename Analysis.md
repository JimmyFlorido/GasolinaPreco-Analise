---
title: "GasPrice"
author: "Lira"
date: "25/01/2020"
output: html_document
---

**ESSA ANÁLISE REFERE-SE AO MUNICÍPIO DE SÃO PAULO/SP, EM QUE ESTÃO VÁRIOS POSTOS INSTALADOS NA CIDADE. OS DADOS ABORDAM OS PREÇOS DE GASOLINA NO PERÍODO DE LEVANTAMENTO DE NOVEMBRO DE 2019. Mais informações podem ser achadas aqui http://www.anp.gov.br/preco/ **

Os pacotes necessários para o exercício

```{r PACKAGE}

library(tidyverse)
library(skimr)
library(leaflet)
library(ggmap)
library(factoextra)

#Uma função que pode ser útil
'%!in%' <- function(x,y)!('%in%'(x,y))

```

Carregue os dados necessários para a análise

```{r DATASET}

dados <- read.csv("LevantamentoPreçoSampa.csv") #dados dos postos de combustíveis da cidade de São Paulo

bairros <- read.csv("SampaBairros.csv") #Dados com os bairros e distritos de São Paulo, categorizados entre as zonas da cidade.

pop <- read.csv2("Populaçãototal20200117.csv",
                       header = FALSE,
                       encoding = "latin1",
                       dec = ".",
                       stringsAsFactors = FALSE)

```

Visão preliminar dos dados - enxergar o que precisa ser ajustado

```{r OVERLOOK}

skim(dados)

```

Ajustar os dados - conserte principalmente as informações de bairro: há duplicados com acento & sem acento, e de quebra, há bairros com nome equivocado. 

```{r DATA FIX, include=FALSE}

postos <- dados %>% 
  mutate_at(vars(-MODELIDADE.DE.COMPRA, -FORNECEDOR..B.BRANCA., -NOTA.FISCAL), list(~as.character(.))) %>% 
  mutate_at(vars(PREÇO.VENDA, PREÇO.COMPRA), list(~as.numeric(.))) %>%
  mutate_at(vars(DATA.COLETA, DATA.RECUSA), list(~as.Date(as.character(.), 
                                                          format = "%d/%m/%Y"))) %>% 
  mutate(BAIRRO = iconv(BAIRRO, 
                        from="utf-8", to = "ASCII//TRANSLIT"),
         BAIRRO = str_to_lower(BAIRRO), 
         BAIRRO = gsub("raza", "rasa", BAIRRO),
         BAIRRO = gsub("alvin", "alvim", BAIRRO),
         BAIRRO = gsub("belezinho", "belenzinho", BAIRRO),
         BAIRRO = gsub("do limao", "limao", BAIRRO),
         BAIRRO = gsub("bairro limao", "limao", BAIRRO),
         BAIRRO = gsub("bairro limoeiro", "limoeiro", BAIRRO),
         BAIRRO = gsub("jardim angela \\(zona sul\\)", "jardim angela", BAIRRO),
         BAIRRO = gsub("jardim da gloria", "jardim gloria", BAIRRO),
         BAIRRO = gsub("jardim das rosas - pinheiros", "jardim das rosas", BAIRRO),
         BAIRRO = gsub("jardim iv centenario", "jardim quarto centenario", BAIRRO),
         BAIRRO = gsub("vila moinho velho/vila vera", "vila moinho velho", BAIRRO),
         BAIRRO = gsub("vila manchester", "vila nova manchester", BAIRRO),
         BAIRRO = gsub("guaianazes", "guaianases", BAIRRO),
         BAIRRO = gsub("alto dos pinheiros", "alto de pinheiros", BAIRRO),
         BAIRRO = gsub("vila nov a conceicao", "vila nova conceicao", BAIRRO),
         BAIRRO = gsub("vila merces", "vila das merces", BAIRRO),
         BAIRRO = gsub("vila vera/sacoma", "vila vera", BAIRRO),
         BAIRRO = gsub("vila firmino pinto", "vila firmiano pinto", BAIRRO),
         BAIRRO = gsub("s miguel paulista", "sao miguel paulista", BAIRRO),
         BAIRRO = gsub("sto amaro", "santo amaro", BAIRRO),
         BAIRRO = gsub("parque paineiras", "parque das paineiras", BAIRRO),
         BAIRRO = gsub("nossa sra do o", "nossa senhora do o", BAIRRO),
         BAIRRO = gsub("j s cristovao", "jardim nove de julho", BAIRRO),
         BAIRRO = gsub("hygienopolis", "higienopolis", BAIRRO),
         BAIRRO = gsub("nova parelheiros", "parelheiros", BAIRRO),
         BAIRRO = gsub("colonia paulista", "parada colonia paulista", BAIRRO),
         BAIRRO = gsub("freguesia do o", "nossa senhora do o", BAIRRO),
         BAIRRO = gsub("casa verde - santana", "casa verde", BAIRRO),
         BAIRRO = gsub("cicy lapa 1", "bela alianca", BAIRRO),
         BAIRRO = gsub("freguesia do o", "nossa senhora do o", BAIRRO),
         BAIRRO = gsub("cid ae carvalho", "cidade a.e.carvalho", BAIRRO),
         BAIRRO = gsub(c("^jd","^jd\\."), "jardim", BAIRRO),
         BAIRRO = gsub("jardim\\.", "jardim", BAIRRO),
         BAIRRO = gsub(c("pq", "pq\\."), "parque", BAIRRO),
         BAIRRO = gsub("parque\\.", "parque", BAIRRO),
         BAIRRO = gsub("prq", "parque", BAIRRO),
         BAIRRO = gsub("^vl", "vila", BAIRRO),
         BAIRRO = gsub("^v\\.", "vila", BAIRRO),
         BAIRRO = gsub("vila\\.", "vila", BAIRRO))

#Essa base serve para avaliar os preços de São Paulo Capital
preco <- postos %>% 
  filter(NOTA.FISCAL == "Sim") %>% 
  distinct(ENDEREÇO, .keep_all = TRUE) %>% 
  select(-DATA.RECUSA, -NOTA.FISCAL)

```

Fazer a descritiva básica

```{r DESCRIPTIVE}

skim(preco)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/skim1.png "descriptive1")


Saber quantos postos há em São Paulo Capital, e claro: quantos donos (CNPJs). Descubra qual é a média de postos por dono

```{r OWNERS}

preco %>% 
  group_by(Dono = RAZÃO.SOCIAL) %>% 
  summarise(Postos= n_distinct(ENDEREÇO),
            Bandeiras = n_distinct(BANDEIRA),
            Bairro = n_distinct(BAIRRO)) %>% 
  as.data.frame() %>% 
  skim()

```
Praticamente 1 posto por CNPJ, apesar do registro de CNPJs que detém 7 postos. 

Verificar quantos postos e bandeiras há por bairro

```{r NEIGHBORHOODS}

preco %>% 
  group_by(Bairro = BAIRRO) %>% 
  summarise(Postos= n_distinct(ENDEREÇO),
            Bandeiras = n_distinct(BANDEIRA)) %>% 
  as.data.frame() %>% 
  skim()

```
Há 2.51 postos por bairro, e 1.64 Bandeiras por bairro. 

Afirma-se que ninguém sonegou informações para a pesquisa - todos os postos responderam ela, mesmo em períodos diferentes. 

Verificar qual é a bandeira de posto mais popular em São Paulo, e qual que tem a gasolina mais cara?

```{r FLAG RANK}

preco %>% 
  group_by(Marca = BANDEIRA) %>% 
  summarise(Postos = n()) %>% 
  as.data.frame() %>% 
  arrange(desc(Postos))

preco %>% 
  group_by(Marca = BANDEIRA) %>% 
  summarise(Gasolina = mean(PREÇO.VENDA)) %>% 
  as.data.frame() %>% 
  arrange(desc(Gasolina))

```
Marca | Preço (R$/l)
------------ | -------------
Ipiranga |	4.284852			
Petrobrás |	4.258189			
Raízen |	4.201177			
Zema |	4.139000			
Alesat |	4.040357			
Branca |	3.974141

As bandeiras "branca" e "Ipiranga" são, respectivamente, a primeira e segunda bandeiras mais populares em São Paulo Capital. 
Enquanto as bandeiras com gasolina mais cara são "Ipiranga" e "Petrobrás." Salienta-se que a bandeira "branca" pratica os menores preços

Analisar para descobrir qual é o bairro em São Paulo que tem a gasolina mais cara.

```{r NEIGHBORHOOD RANK}

preco %>% 
  group_by(Bairro = BAIRRO) %>% 
  summarise(Gasolina = mean(PREÇO.VENDA)) %>% 
  as.data.frame() %>% 
  arrange(desc(Gasolina))

```
Bairro | Preço (R$/l)
------------ | -------------
jardim cabore |	5.799000			
higienopolis |	4.999000			
parque colonial |	4.999000			
chacara itaim |	4.990000			
vila progredior |	4.990000			
jardim america |	4.932333			
jardim paulista |	4.899000			
liberdade |	4.799000			
indianopolis |	4.699000			
jardim sao sebastiao |	4.699000	

Conseguir endereços mais precisos para fazer pesquisa de coordenadas (latitude e longitude) no Google Maps por meio do "ggmap".

```{r COORDINATES, eval=FALSE, include=FALSE}

#Primeiro ative o API do Google Maps

register_google(key = "AIzaSyDraLrdm7nUtAOXC7xqA2aL-vqi4QzsY0o")

#Verifique se a chave está ativa
ggmap::has_google_key()

postos2 <- postos %>% 
  mutate(Lugar = str_to_lower(paste(ENDEREÇO, BAIRRO, sep = " - ")),
         CidadeUF = str_to_lower(paste("São Paulo", "sp", sep = " - ")),
         Lugar = paste(Lugar, CidadeUF, "brazil", sep = ", ")) %>% #Colocar endereço nesse formato: "praça joão pessoa, 3 - centro, itapecerica da serra - sp, 06850-035, brazil"
  mutate_geocode(Lugar, 
                 output = "latlona", 
                 source = "google") #Pesquisar as coordenadas com base na variável "Lugar"

write.csv(postos2, "SampaGasPreço.csv", 
          row.names = FALSE) #Salvar a base com a info de coordenadas para evitar repetir a pesquisa novamente

```

Uma vez com essas coordenadas, usar esse arquivo para plotar um mapa em que mostre onde está mais caro o preço da gasolina. 

```{r PLOT PRICE LOCATION}

preco2 <- postos2 %>% 
  filter(NOTA.FISCAL == "Sim",
         PREÇO.VENDA < 4.75)

pal1 <- colorNumeric(palette = rev(RColorBrewer::brewer.pal(10,"RdYlBu")), 
                     domain = preco2$PREÇO.VENDA)

preco2 %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   color = ~pal1(PREÇO.VENDA),
                   radius = 10,
                   opacity = 1,
                   popup = ~Lugar,
                   label = ~PREÇO.VENDA) %>% 
  addLegend("bottomright", 
            pal = pal1, 
            values = ~PREÇO.VENDA,
            title = "Preço da Gasolina",
            labFormat = labelFormat(prefix = "R$"),
            opacity = 1)

```
Além disso, mostre num mapa como estão distribuídos por marca os postos. 

```{r WHITE LOCATION}

preco2 <- postos2 %>% 
  filter(NOTA.FISCAL == "Sim",
         PREÇO.VENDA < 4.75) %>% 
  mutate(BANDEIRA = as.factor(BANDEIRA))

pal2 <- colorFactor("Set1", preco2$BANDEIRA)

preco2 %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   color = ~pal2(preco2$BANDEIRA),
                   radius = 7.5,
                   opacity = 1,
                   popup = ~Lugar,
                   label = ~PREÇO.VENDA) %>% 
  addLegend("bottomright", 
            pal = pal2, 
            values = ~BANDEIRA,
            title = "Bandeira de Posto",
            opacity = 1)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/SampaGas.png "Mapa1")
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/SampaGas2.png "Mapa2")

Ao comparar os dois mapas, consegue-se traçar certos padrões: na zona leste de São Paulo, há mais postos baratos, e também há mais postos de bandeira "Branca". É uma correlação que vale a pena ser testada. 

Apesar dessas aferições, os mapas não respondem a principal pergunta: **lugares com maior competição entre os postos, têm menores preços de gasolina**

Para responder isso, será feita uma regressão que agrupará os postos em torno de bairros (Consolação, Bela Vista, Moema, etc), a fim de entender se um bairro com mais postos, tende a ter um combustível mais barato. 

```{r NEIGHBORHOOD DATASET FIX}

bairros2 <- bairros %>% 
  add_row(
    Bairro = c("limao", "casa verde", "nossa senhora do o", "limoeiro", "guaianases", "jaragua", "sao miguel paulista", "itaquera", "itaim paulista", "pirituba", "pompeia", "sao miguel", "vila curuca", "ermelino matarazzo", "parque do estado", "jardim tremembe", "vila nova curuca", "vila curuca velha", "cidade nitro operario", "vila nancy", "vila americana", "jardim nove de julho", "piqueri", "moinho velho", "itaberaba"), 
    Distrito = c("CASA VERDE", "CASA VERDE", "FREGUESIA DO O", "SAO LUCAS", "GUAIANASES", "JARAGUA", "SAO MIGUEL", "ITAQUERA", "ITAIM PAULISTA", "PIRITUBA", "LAPA", "SAO MIGUEL", "VILA CURUCA", "ERMELINO MATARAZZO", "IPIRANGA", "TREMEMBE", "VILA CURUCA", "JARDIM HELENA", "SAO MIGUEL", "GUAIANASES", "SAO MIGUEL", "ITAQUERA", "FREGUESIA DO O", "PIRITUBA", "PIRITUBA"), 
    Divisão = c(NA, NA, NA, "ZONA LESTE 1", "ZONA LESTE 1", NA, "ZONA LESTE 1", "ZONA LESTE 1", "ZONA LESTE 1", NA, NA,"ZONA LESTE 1", "ZONA LESTE 1", "ZONA LESTE 1", "ZONA SUL 1", NA, "ZONA LESTE 1", NA, "ZONA LESTE 1", "ZONA LESTE 1", "ZONA LESTE 1", "ZONA LESTE 1", NA, NA, NA), 
    Zona = c("NORTE", "NORTE", "NORTE", "LESTE", "LESTE", "NORTE", "LESTE", "LESTE", "LESTE", "NORTE", "OESTE", "LESTE", "LESTE", "LESTE", "SUL", "NORTE", "LESTE", "NORTE", "LESTE", "LESTE", "LESTE", "LESTE", "NORTE", "NORTE", "NORTE")
  ) %>% 
  arrange(Bairro, Distrito)

bairros2 <- bairros2 %>% 
  mutate_at(vars(-Zona), list(~as.character(.))) %>% 
  mutate(Bairro = str_to_lower(Bairro),
         Bairro = iconv(Bairro, 
                        from="utf-8", to = "ASCII//TRANSLIT"),
         Bairro = gsub("jardim progredior", "vila progredior", Bairro),
         Distrito = str_to_lower(Distrito),
         Distrito = iconv(Distrito, 
                          from="utf-8", to = "ASCII//TRANSLIT"),
         Distrito = gsub("itaim-bibi", "itaim bibi", Distrito),
         Divisão = ifelse(Divisão == "", NA, Divisão)) %>% 
  distinct(Bairro, .keep_all = TRUE)

```

Junte a informação de preços com a zona ou distrito pertencente aos bairros, e faça a tabela a ser usada nas modelagens, além de um panorama dela através do "skim". 

```{r EVIDENCE TABLE}

postos2[219, 3] <- "tremembe"

tabela <- postos2 %>% 
  filter(NOTA.FISCAL == "Sim") %>% 
  mutate(Branca = ifelse(BANDEIRA == "BRANCA", 
                         1, 0)) %>% 
  group_by(Bairro = BAIRRO) %>% 
  summarise(Postos = n_distinct(ENDEREÇO),
            Donos = n_distinct(RAZÃO.SOCIAL),
            PreçoMédia = mean(PREÇO.VENDA),
            Bandeiras = n_distinct(BANDEIRA),
            Branca = sum(Branca)
  ) %>% 
  as.data.frame() %>% 
  mutate(Outlier = ifelse(PreçoMédia > 4.75, 1, 0)) %>% 
  left_join(bairros2, by = "Bairro") %>% 
  select(-Divisão)
  
skim(tabela)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/skim2.png "descriptive2")

Verifique a influência sobre o nível de preço

```{r EVIDENCE MODEL1}

reg1 <- lm(PreçoMédia ~ Outlier + Postos + I(Branca/Postos), 
           data = tabela)

summary(reg1)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/reg1.png "regression1")

Acrescenta-se que pode ser incluída a variável (dummy) das zonas da cidade na regressão - se tal bairro é da zona norte, sul, leste ou oeste, mas isso é acessório: não ajuda o modelo a responder o mais importante.

# O ponto que essa regressão simples revela é que a quantidade de postos não é relevante para explicar o nível de preços entre a amostra de bairros. No entanto, a quantidade de postos de marca branca

No entanto, também está claro que os bairros, como unidades de cluster (agrupações), não são o suficiente para testar a nossa hipótese, especialmente por conta de uma premissa que não é verdadeira: os motoristas não buscam e pesquisam combustível dentro de um bairro, e sim, dentro de uma área que envolve vários bairros. É necessário criar uma nova clusterização (agrupamento) para testar o modelo novamente. 

Uma abordagem adequada para criar clusters: o algoritmo k-means. 

Mas para criar os clusters precisamos de uma referência baseada nas informações de bairros e distritos, conforme este exemplo: o distrito do Grajaú tem vários bairros (Bororé, Parque Cocaia, Jardim Gaivota, etc), incluindo o bairro Grajaú, então o posto de combustível desse distrito é que servirá de referência para formar o cluster; se houver mais de 1 posto nesse bairro, é feito uma média com as coordenadas (encontrar o meio termo). **Na prática, são montados grupos baseados em distritos e suas coordenadas médias.**

```{r CLUSTER REFERENCE}

reference <- bairros2 %>% 
  mutate(
         Núcleo = ifelse(str_to_lower(Bairro) == str_to_lower(Distrito), 1, 0)
         ) %>% 
  filter(Núcleo == 1) %>% 
  select(Bairro, Núcleo) %>% 
  left_join(pop2, by = c("Bairro" = "DISTRITO"))

reference <- postos2 %>% 
  distinct(ENDEREÇO, .keep_all = TRUE) %>% 
  right_join(reference, by = c("BAIRRO" = "Bairro")) %>% 
  group_by(BAIRRO, População = POPULAÇÃO) %>%
  summarise(lon = mean(lon, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE)) %>%
  as.data.frame() %>% 
  filter(!is.nan(lon))

```

Ajustar a base de dados de população por distrito, caso seja necessário usar essa informação. 

```{r POPULATION DATASET FIX}

pop2 <- pop[-1,]

pop2 <- pop2 %>% 
  select(V2, V3, V4) %>% 
  rename("DISTRITO" = V2, "ANO" = V3, "POPULAÇÃO" = V4) %>% 
  mutate(DISTRITO = gsub("\\(Distrito\\)", "", DISTRITO),
         DISTRITO = str_to_lower(DISTRITO),
         DISTRITO = iconv(DISTRITO, 
                        from="utf-8", to = "ASCII//TRANSLIT"),
         ANO = as.numeric(ANO),
         POPULAÇÃO = gsub("\\.", "", POPULAÇÃO),
         POPULAÇÃO = as.numeric(POPULAÇÃO))

```

Observa-se que há outras formas de construir clusters sem viés ao aliar o k-means com o "teste de silhueta" - afirmar qual é o número ótimo de clusters dentro do algoritmo. 

No entanto, o número que esta técnica fornece é 6 (resposta providenciada pelo pacote "factoextra"), o que impossibilita rodar uma regressão com 6 observações, e assim, responder a nossa pergunta. 

Usar a informação de distrito é uma forma de clusterizar, sem ser discricionário (usar um método conforme a conveniência, oq ue pode induzir a viés). 

Ao usar a informação de referência, é fornecido 54 pontos para formar os clusters. 

```{r CREATING CLUSTERS}

k <- reference %>% 
  select(-BAIRRO, -População)

x <- postos2 %>% 
  distinct(ENDEREÇO, .keep_all = TRUE) %>% 
  select(lon, lat)

clustering <- kmeans(x, k) 

ggcluster <- clustering[["centers"]] %>% 
  as.data.frame() %>% 
  mutate(Tipo = "Cluster")

cluster <- clustering[["cluster"]] %>% 
  as.data.frame()

clustering

```
Nota-se que a clusterização está razoável: agrupou mais de 97% dos postos listados. 

Observe, por meio do gráfico de dispersão, como ficou a clusterização: ela está coerente, e realmente distribuída entre vários postos. Dentro de cada cluster desses, há número suficiente de postos de gasolina para os consumidores pesquisarem os preços e trazerem uma premissa mais realista para a regressão. 

```{r SEEING CLUSTERS}

theme_set(theme_minimal())

postos2 %>% 
  select(lon, lat) %>% 
  mutate(Tipo = "Posto") %>% 
  bind_rows(ggcluster) %>% 
  ggplot() +
  geom_point(aes(x = lon, y = lat, colour = Tipo), 
             size = 1.5) +
  scale_color_manual(values = c("#ff0000","#e9ab00")) +
  labs(x = "Longitude", 
       y = "Latitude") +
  theme(legend.position = "top")

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/ClusterVisualization.png "54clusters")

Adicionar as informações de cluster e montar grupos com número de postos e preço médio praticado no lugar. 

```{r ADDING CLUSTER INFO}

postos3 <- postos2 %>% 
  distinct(ENDEREÇO, .keep_all = TRUE) %>% 
  add_column(cluster = cluster$.)

tabela2 <- postos3 %>% 
  filter(NOTA.FISCAL == "Sim",
         PREÇO.VENDA < 4.75) %>%
  mutate(Branca = ifelse(BANDEIRA == "BRANCA", 
                         1, 0)) %>% 
  group_by(Cluster = cluster) %>% 
  summarise(Postos = n_distinct(ENDEREÇO),
            Donos = n_distinct(RAZÃO.SOCIAL),
            PreçoMédia = mean(PREÇO.VENDA),
            Bandeiras = n_distinct(BANDEIRA),
            Branca = sum(Branca)
  ) %>% 
  as.data.frame() %>% 
  add_column(População = reference$População) %>% 
  mutate(Cluster = as.character(Cluster)
         )

skim(tabela2)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/skim3.png "descriptive3")

Visualize, mais uma vez, as distribuições de preço médio de gasolina e número de postos

```{r PRICE DISTIRBUTION2}

theme_set(theme_minimal())

tabela2 %>%
  ggplot() +
  aes(x = PreçoMédia) %>%
  geom_histogram(
                 fill = "lawngreen") +
  labs(x = "Preço (R$)",
       y = "Frequência")

tabela2 %>% 
  ggplot() +
  aes(x = Postos) %>% 
  geom_freqpoly(colour = "green", size = 1.5) +
  labs(x = "Quantidade de Postos",
       y = "Frequência")

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/dist1.png "prices")
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/dist2.png "stations")

Rodar o modelo, de novo

```{r CLUSTER EVIDENCE}

clustereg1 <- lm(PreçoMédia ~ Postos + I(Branca/Postos), 
                 data = tabela2)

summary(clustereg1)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/reg2.png "regression2")

```{r TESTING REGRESSION}

#Package with tests for different problems that occur to OLS regressions
library(lmtest)

#Verify the underspecification modeling - Ramsey's RESET Test
resettest(clustereg1, 
          power = 2, 
          type = "regressor")

#Testing heterocesdasticity - White Test (using Breusch-Pagan formula)
bptest(clustereg1, 
       varformula = ~ Postos + I(Branca/Postos) + I(Postos^2) + I((Branca/Postos)^2) + I(Postos*(Branca/Postos)), 
       data = tabela2, 
       studentize = FALSE)

#Testting autocorrelation - Durbin-Watson index
dwtest(clustereg1)

```
![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/tests.png "regression_tests")

A baixa significância da estatística no teste RESET mostra que não há problema de não-linearidade do modelo (má especificação).
Não há presença de heterocedasticidade (a hipótese nula afirma a ausência de heterocedasticidade).
E não há problema de autocorrelação - o DW está próximo de 2.

---
Os resutados da regressão, com a nova agrupação (baseada nos distritos), demonstram que quanto maior a quantidade de postos de combustíveis, menor é o preço médio da gasolina praticada na área. No entanto, é preciso enfatizar que o efeito disso é baixo (a regressão explica somente 14% do nível de preços entre as diferentes áreas). 
---
