#importando uma base nativa do R
help("mtcars") 
View(mtcars) 

#criando as variáveis MPG e WT
MPG <- mtcars$mpg  # Consumo de combustível
WT <- mtcars$wt    # Peso do veículo

#ver as primeiras entradas
head(MPG)
head(WT)

#criando os graficos
plot(WT, MPG, main = "Relação entre Peso e Consumo de Combustível",
     xlab = "Peso do veículo (1000 libras)", ylab = "Consumo (Milhas por Galão)")

#ajustando o modelo de regressão linear
modelo <- lm(MPG ~ WT)


#adicionando a linha de regressão no gráfico
abline(modelo)

# Utilizando ggplot para melhorar o gráfico
library(ggplot2)

ggplot(mapping = aes(WT, MPG)) +  
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relação entre Peso e Consumo de Combustível", x = "Peso (1000 libras)", y = "Consumo (MPG)")

#remover o delimitador cinza e adicionar a linha da média
retas <- ggplot(mapping = aes(WT, MPG)) +  
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") +  
  geom_hline(yintercept = mean(MPG)) +
  labs(title = "Relação entre Peso e Consumo com Linha de Média", x = "Peso (1000 libras)", y = "Consumo (MPG)")
retas


#medir distâncias dos pontos para a média
retas + geom_segment(aes(x=WT, y=MPG, xend = WT, yend = mean(MPG)), color = "red")

#medir distância entre Regressão e valor real

# Calcular o R²
sqt = sum( (mean(MPG) - MPG)^2)
sqres = sum((predict(modelo) - MPG)^2)
r2 = (sqt - sqres) / sqt
r2  # Exibe o valor de R²

#prever o consumo de combustível para novos valores de peso de veículos
novos_pesos = data.frame(WT = c(3, 4, 5))  # Pesos de 3000, 4000 e 5000 libras
previsoes_consumo = predict(modelo, novos_pesos)
previsoes_consumo















