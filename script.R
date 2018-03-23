senadores<-function(legislatura_inicio,legislatura_fim){
  url<-paste0("http://legis.senado.leg.br/dadosabertos/senador/lista/legislatura/",legislatura_inicio,"/",legislatura_fim)
  jsonlite::fromJSON(url)
}

identificacao<- leg_55_56$ListaParlamentarLegislatura$Parlamentares$Parlamentar$IdentificacaoParlamentar

f<-function(x){
  x$ListaParlamentarLegislatura$Parlamentares$Parlamentar$IdentificacaoParlamentar
}

df<-leg_55_56$ListaParlamentarLegislatura$Parlamentares$Parlamentar$IdentificacaoParlamentar


id<-leg_55_56 %>% 


mandatos<-leg_55_56$ListaParlamentarLegislatura$Parlamentares$Parlamentar$Mandatos$Mandato

leg_55_56<-senadores(55,56)

codigo_discurso<- leg_55_56 %>% 
  purrr::pluck(1) %>% 
  pluck(4) %>% 
  pluck(1) %>% 
  pluck(1) %>% 
  pull(1)

  url_discurso<-paste0("http://legis.senado.leg.br/dadosabertos/senador/",codigo_discurso,"/discursos")


  
d<-fromJSON(url_discurso[1])

base <- url_discurso %>% map(fromJSON)

b <- base %>%
  modify_depth(3,~pluck(.x,2))

b1<-b %>% 
  map(~pluck(.x,1))

b2<-b1 %>% 
  map(~{
    .x %>% 
      pluck(4) %>% 
      pluck(1) %>% 
      pluck(9)
  })

b3<-map2_dfr(codigo_discurso,b2,~{
  cbind(.x,.y) %>% as_tibble()
  
})

        
b4<-map(b3$.y,~{
  httr
  
})


s<- read_html(b3$.y[[3]])

f<-function(x){
  
  a<- read_html(x) %>% 
  html_nodes(xpath="//div[@id='content']") %>%
  html_text()
}


s<-httr::GET(b3$.y[[3]])

s<-map_chr(b3$.y[1:10000],possibly(~{
  if(!is.na(.x)){
  .x %>% 
    read_html() %>% 
    html_nodes(xpath="//div[@id='content']") %>%
    html_text()
}else{
  NA_character_
}
},NA_character_))



