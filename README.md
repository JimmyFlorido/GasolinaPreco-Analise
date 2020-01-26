# Análise do preço de gasolina entre os bairros de São Paulo: a competição importa?

Este simples exercício com os dados de postos de combustíveis instalados no município de São Paulo, pertencentes à ANP 
(Agência Nacional do Petróleo, Gás Natural e Biocombustíveis), busca entender se, de fato, uma região que tem mais postos, 
tem gasolina com preço menor que as demais regiões, ou seja, verificar se a competição estimula menores preços de gasolina 
numa das maiores capitais do país. 
Os dados abrangem o período de novembro de 2019, e podem ser achados aqui http://www.anp.gov.br/preco/ 

**A análise aqui foi feita por meio do R.** 

Entendeu-se que há regiões que realmente praticam menores preços de gasolina, e que possuem mais postos que as demais, 
além de haver grandes "outliers" - postos que cobram mais de R$ 5 pelo litro da gasolina! O mapa abaixo mostra um pouco disso: 

![alt text](https://github.com/JimmyFlorido/GasolinaPreco-Analise/blob/Patch-2/Images/SampaGas.png "PostosPrecos")

Mas o mapa não traz clareza sobre o efeito da competição, e por isso, foi feita uma regressão OLS (Ordinay Least Square) para 
entender se quanto maior a quantidade de postos numa região, menor é o preço da gasolina pŕaticada na região. 

A proxy para representar "região" no contexto de regressão foi, a princípio, os bairros de São Paulo, mas entendeu-se que 
o consumidor (ainda mais locomovendo-se no carro) não fica confinado aos bairros para pesquisar e avaliar os preços: a procura 
por combustível pode envolver vários bairros. 
 
Por isso, construi-se uma outra proxy de "região" usando o algoritmo k-means, ou seja, usou-se um algoritmo de clusterização 
para criar grupos de postos de combustíveis que não obedecem a lógica de bairro, mas sim, de distrito. 

O resultado mostra que há uma relação invera entre o número de postos de combustível com o preço médio praticado numa região.
Mas tal relação não é forte o suficiente: a infuência da quantidade de postos sobre o preço é de 10%, tanto que o aumento de 
10% no número de postos, reduz o preço da gasolina da região em 0.12% . 

A riqueza desse exercício não limita-se a rodar uma regressão simples, mas de usar um algoritmo de clusterização para criar uma base de dados adaptada para o propósito desse exercício. Acredita-se que esse exercício não é conclusivo, pois é necessário adicionar uma variável para captar demanda: houve tentativas com projeção de população de distritos, mas pensa-se em usar do dados de tráfego de veículos (Waze?) para captar a demanda nos postos de combustível. 
