library(tidyverse)
library(rvest)
library(RCurl)
library(glue)


## Câmara dos deputados 

## Lista dos discursos

## É imporportante pedir aos poucos, se retorna código 500. Coloquei mês a mês.
## O script foi mantado para baixar a qualquer momento, desde que seja especificada 
## a data final.

inicio_legislatura<-"01/02/2015" %>% dmy()

inicio<-interval(inicio_legislatura, today()-1) %>%
  time_length("month") %>%
  trunc() %>% 
  {inicio_legislatura + months(1:.)}


fim<-inicio+months(1)-1

fim[length(fim)]<-today()-1

inicio<- format(inicio,"%d/%m/%Y")
fim<- format(fim,"%d/%m/%Y") 

c_url2<-paste0("http://www.camara.leg.br/sitcamaraws/SessoesReunioes.asmx/ListarDiscursosPlenario?dataIni=",inicio,"&dataFim=",fim,"&codigoSessao=&parteNomeParlamentar=&siglaPartido=&siglaUF=")

lista_discurso <- c_url2 %>% 
  map(~{
    Sys.sleep(.5)
    .x %>%
      read_xml()
    
  })



lista_sessoes<- lista_discurso %>% 
  map(~{
    .x %>% 
      xml_children() %>%
      xml_name() %>%
      length()
  }) %>% unlist()

l<-map2(lista_discurso,lista_sessoes,~{
  s<-.x
  map(1:.y,~{
    codSessao<- xml_find_all(s,paste0("//sessao[",.x,"]/codigo")) %>% xml_text(trim=T)
    data<- xml_find_all(s,paste0("//sessao[",.x,"]/data")) %>% xml_text(trim=T)
    numero_sessao<- xml_find_all(s,paste0("//sessao[",.x,"]/numero")) %>% xml_text(trim=T)
    tipo_sessao<- xml_find_all(s,paste0("//sessao[",.x,"]/tipo")) %>% xml_text(trim=T)
    codigo_faseSessao<- xml_find_all(s,paste0("//faseSessao[",.x,"]/codigo")) %>% xml_text(trim=T)
    descricao_faseSessao<- xml_find_all(s,paste0("//faseSessao[",.x,"]/descricao")) %>% xml_text(trim=T)
    hora_discurso<- xml_find_all(s,paste0("//discurso/horaInicioDiscurso")) %>% xml_text(trim=T)
    txtIndexaxao<- xml_find_all(s,paste0("//discurso/txtIndexacao")) %>% xml_text(trim=T)
    numeroQuarto<- xml_find_all(s,paste0("//discurso/numeroQuarto")) %>% xml_text(trim=T)
    numeroInsercao<- xml_find_all(s,paste0("//discurso/numeroInsercao")) %>% xml_text(trim=T)
    sumario<- xml_find_all(s,paste0("//discurso/sumario")) %>% xml_text(trim=T)
    numero_orador<- xml_find_all(s,paste0("//orador/numero")) %>% xml_text(trim=T)
    nome_orador<- xml_find_all(s,paste0("//orador/nome")) %>% xml_text(trim=T)
    partido_orador<- xml_find_all(s,paste0("//orador/partido")) %>% xml_text(trim=T)
    uf_orador<- xml_find_all(s,paste0("//orador/uf")) %>% xml_text(trim=T)
    
    cbind(codSessao,data,numero_sessao,tipo_sessao,
          codigo_faseSessao,descricao_faseSessao,
          hora_discurso,txtIndexaxao,numeroQuarto,numeroInsercao,sumario,
          numero_orador,nome_orador,partido_orador,uf_orador)  
  })
  
})

f<-function(x) x[[1]]

df<- map_dfr(seq_along(lista_sessoes),~pluck(l,.x,f) %>% as.data.frame(stringsAsFactor=FALSE))

## Discursos

discursos_url<-glue("http://www.camara.leg.br/SitCamaraWS/SessoesReunioes.asmx/obterInteiroTeorDiscursosPlenario?codSessao={df$codSessao}&numOrador={df$numero_orador}&numQuarto={df$numeroQuarto}&numInsercao={df$numeroInsercao}")

inteiro_teor<-discursos_url %>% 
  map_dfr(~{
    l<-.x %>% 
      read_xml()
    
    orador<-xml_find_first(l,"//nome") %>% xml_text()
    partido<-xml_find_first(l,"//partido") %>% xml_text()
    uf<-xml_find_first(l,"//uf") %>% xml_text()
    horaInicioDiscurso<-xml_find_all(l,"//horaInicioDiscurso") %>% xml_text()
    inteiro<-xml_find_all(l,"//discursoRTFBase64") %>%
      xml_text() %>% 
      decode_rtf()
    cbind(orador,partido,uf,horaInicioDiscurso,inteiro) %>% as_tibble()
  })


