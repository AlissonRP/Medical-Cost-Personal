library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries)
library('tidyverse')

# teste de Jarque-Bera
options(digits=3)
theme_set(theme_minimal())
knitr::opts_chunk$set(echo=FALSE,message=F,warning=F,fig.pos = 'H',fig.align = 'center',fig.width=7.8, fig.height=4.85)
doParallel::registerDoParallel(cores=2)
scale_fill_discrete = \(...) scale_fill_brewer(... , palette="Set2")
df=read_csv("https://raw.githubusercontent.com/AlissonRP/DATA_SETS/master/insurance.csv")
##https://www.kaggle.com/mirichoi0218/insurance# link do df




df=df %>% 
  mutate(smoker=ifelse(smoker=="yes",1,0),sex=ifelse(sex=='female',1,0)) %>% 
  select(-region,-bmi) 
  
attach(df)

fit=bind_cols(y=y2,df) %>%
  select(-charges) %>% 
  lm(y~.,.)
  


result=boxcox(lm(charges~age+children+smoker),lambda=seq(0,1,by=.1))
mylambda = result$x[which.max(result$y)]
mylambda
y2 = (charges^mylambda-1)/mylambda
hist(y2)

df %>% 
  ggplot(aes(charges)) + 
  geom_density()


## Testa [S0]
## Teste RESET de especificacao
## H0: O modelo esta corretamente especificado
resettest(fit)  # sem problemas até aqui



## Testa [S1]
## Teste t para a média dos errros
## H0: média dos erros eh igual a zero
t.test(resid(fit),mu=0,alternative="two.sided") #sem problemas



## Testa [s2]
## Teste de Bressch-Pagan (Koenker) de Heteroscedasticidade
## H0: erros sao homoscedasticos
bptest(fit, studentize = TRUE)





## Testa [S3]
## Teste de Durbin-Watson de autocorrelacao
## H0: : Nao hah autocorrelacao 
dwtest(fit) #ok
vif(ft2)


## Testa [S4]
## Usa Fatores de Inflacao de Variancia para detectar multicolinearidade
## Regra de bolso: vif > 10 indica multicolinearidade. vif=1 seria o ideal.



## Testa [S5]
## Teste Jarque-Bera de Normalidade
## H0: Os erros possuem distribuicao normal
jarque.bera.test(resid(fit))
jarque.bera.test(residuo)
shapiro.test(y2)


##### Analise de influencia
n<-dim(df)[1] # tamanho da amostra

# com a seguinte funcao se obtem varias medidas de influencia
t=influence.measures(fit)


hatvalues(fit)
h_bar<-fit$rank/ n
limite<-3*h_bar
abline(plot(hatvalues(fit),ylab="Alavancagem"), 
       col="red", h=limite,lty=2)
identify(hatvalues(fit),n=1)

dffits(fit)
limite<-3*sqrt(fit$rank / n)
abline(plot(dffits(fit),ylab="DFFITS"), 
       col="red", h=limite,lty=2)
identify(dffits(fit),n=3) 


limite<-4/(n-fit$rank )
abline(plot(cooks.distance(fit,ylab="Distancia de Cook"), 
       col="red", h=limite,lty=2))

# residuo
residuo <- rstudent(fit) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico

hist(residuo) # histograma dos residuos

# envelope simulado baseado nos residuos studentizados
hnp(fit,resid.type="student",halfnormal = F) # envelope simulado 


fit$residuals %>% 
  as_tibble() %>% 
  ggplot(aes(value)) +
   geom_density()
df %>% 
  ggplot(aes((log(charges))))+
  geom_density()


               