##################
#C�lculos para VaR com Monte Carlo
#Autores:
#Cristiano Manh�es de Oliveira
#Ewerton Sanches Moraes
##################




##################
#Bibliotecas necess�rias
##################

#precisa instalar moments para curtose e vari�ncia
install.packages("moments")
library(moments)

#precisa instalar ISwR para correla��o
install.packages("ISwR")
library(ISwR)





##################
#Estat�stica Descritiva
##################


#ler arquivo
m = read.table(file=file.choose(),header = TRUE,sep = ",")
#mAmbev = as.matrix(m)



#calculando retorno
for(i in nrow(m)-1){
	retorno[i] = (m[i,2] - m[i+1,2])/ m[i+1,2]
}
#retorno[700] = 0


#imprimindo o retorno
for(i in nrow(m)-1){
    print (retorno[i])
}



#m�dia
media = mean(retorno)

#Erro padr�o
erro = sd(retorno)/sqrt(length(retorno))

#mediana
mediana = median(retorno)

#moda
moda = subset(table(retorno),table(retorno) == max(table(retorno)))

#Desvio padr�o
desvio = sd(retorno)

#Variancia da amostra
variancia = var(retorno)

#curtose
curtose = kurtosis(retorno)

#Assimetria
assimetria = skewness(retorno)

#Intervalo
intervalo = max(retorno) - min(retorno)

#M�nimo
minimo = min(retorno)

#M�ximo
maximo = max(retorno)

#Soma
soma = sum(retorno)

#Contagem
contagem = length(retorno)

#Jarque-Bara
#jarqueBara = jarque.test(retorno)
jarqueBara = (contagem/6)*((assimetria^2)+((1/4)*((curtose-3)^2)))

#VaR 1%
var1 = (media-(-2.3263*desvio))*100

#VaR 5%
var5 = (media-(-1.6449*desvio))*100

#VaR 10%
var10 = (media-(-1.2816*desvio))*100

#Salvando o resultado em um arquivo
Smedia = paste("     M�dia: ",media)
Serro = paste("      Erro: ",erro)
Smediana = paste("   Mediana: ",mediana)
Smoda = paste("      Moda: ",moda)
Sdesvio = paste("    Desvio: ",desvio)
Svariancia = paste(" Vari�ncia: ",variancia)
Scurtose = paste("   Curtose: ",curtose)
Sassimetria = paste("Assimetria: ",assimetria)
Sintervalo = paste(" Intervalo: ",intervalo)
Sminimo = paste("    M�nimo: ",minimo)
Smaximo = paste("    M�ximo: ",maximo)
Ssoma = paste("      Soma: ",soma)
Scont = paste("  Contagem: ",contagem)
SjarqueBara = paste("JarqueBara: ",jarqueBara)
Svar1 = paste(" VaR de 1%: ",var1)
Svar5 = paste(" VaR de 5%: ",var5)
Svar10 = paste(" VaR de 10%: ",var10)

fileConn<-file(file.choose())
writeLines(c(Smedia,Serro,Smediana,Smoda,Sdesvio,Svariancia,Scurtose,Sassimetria,Sintervalo,Sminimo,Smaximo,Ssoma,Scont,SjarqueBara,Svar1,Svar5,Svar10), fileConn)
close(fileConn)



##################
#PLOTANDO Histograma
##################

#Histograma:
hist(retorno)






##################
#PARA CALCULAR O RETORNO EM VETORES
##################


retornoAMBEV = 0
retornoBRADESCO = 0
retornoPETROBRAS = 0
retornoVALE = 0

#ler arquivo

m = read.table(file=file.choose(),header = TRUE,sep = ",")


#calculando retorno AMBEV
for(i in 1:nrow(m)-1){
	retornoAMBEV[i] = (m[i,2] - m[i+1,2])/ m[i+1,2]
}


#ler arquivo
m = read.table(file=file.choose(),header = TRUE,sep = ",")




#calculando retorno BRADESCO
for(i in 1:nrow(m)-1){
	retornoBRADESCO[i] = (m[i,2] - m[i+1,2])/ m[i+1,2]
}


#ler arquivo
m = read.table(file=file.choose(),header = TRUE,sep = ",")




#calculando retorno PETROBRAS
for(i in 1:nrow(m)-1){
	retornoPETROBRAS[i] = (m[i,2] - m[i+1,2])/ m[i+1,2]
}



#ler arquivo
m = read.table(file=file.choose(),header = TRUE,sep = ",")




#calculando retorno VALE
for(i in 1:nrow(m)-1){
	retornoVALE[i] = (m[i,2] - m[i+1,2])/ m[i+1,2]
}



##################
#CALCULANDO A CORRELA��O
##################

cor(retornoAMBEV,retornoAMBEV)
cor(retornoAMBEV,retornoBRADESCO)
cor(retornoAMBEV,retornoPETROBRAS)
cor(retornoAMBEV,retornoVALE)






##################
#OPERA��ES COM MATRIZES
##################

#iniciando uma matriz
A = matrix(c(1:10),2,2,1)
A

#matriz transposta
B = t(A)

#multiplica��o de matrizes
C = A*B
C

#multiplica��o por um
D = A*2
D

#Criando um vetor
v = 0
v[0] = 1
v[1] = 2

#multiplicando matriz por vetor
E = D*v
E


##################
#GERANDO N�MEROS ALEAT�RIOS
##################
#Gera numeros aleatorios de distribuicao uniforme
x = runif(10000)



# Gera numeros aleatarios de distribuicao normal
y = rnorm(10000)
y
hist(y)

#gera numeros aleatorios de distribuicao binominal
d<-rbinom(1000, 100, 0.5)
d
hist(d)
