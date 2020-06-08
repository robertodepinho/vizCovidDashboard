munsplit <- function(x, split) {
  x = gsub(pattern = " e ", ", ", x)
  x = trimws(x)
  return(strsplit(x, split = split))
}

mun = munsplit("Amapá, Calçoene, Macapá e Oiapoque", split = c(",", "e"))
ZC = data.frame(UF = "AP", municipio = mun[[1]], stringsAsFactors = F)

mun = munsplit("Augusto Corrêa, Bragança, Chaves, Curuçá, Magalhães Barata, Maracanã, Marapanim, Quatipuru, Salinópolis, São Caetano de Odivelas, São João de Pirabás, Soure, Tracuateua e Vizeu", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "PA", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Água Doce do Maranhão, Alcântara, Apicum-Açu, Araioses, Bacuri, Barreirinhas, Cândido Mendes, Carutapera, Cedral, Cururupu, Godofredo Viana, Guimarães, Humberto de Campos, Icatu, Luís Domingues, Mirinzal, Paço do Lumiar, Paulinho Neves, Porto Rico do Maranhão, Primeira Cruz, Raposa, Santo Amaro do Maranhão, São José de Ribamar, São Luís, Serrano do Maranhão, Turiaçu e Tutóia.", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "MA", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Cajueiro da Praia, Ilha Grande, Luís Correia e Parnaíba", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "PI", municipio = mun[[1]], stringsAsFactors = F))


mun = munsplit("Acaraú, Amontada, Aquiraz, Aracati, Barroquinha, Beberibe, Camocim, Cascavel, Caucaia, Cruz, Fortaleza, Fortim, Icapuí, Itapipoca, Itarema, Jijoca de Jericoacoara, Paracuru, Paraipaba, São Gonçalo do Amarante e Trairi", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "CE", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Areia Branca, Baia Formosa, Caiçara do Norte, Canguaretama, Ceará Mirim, Extremoz, Galinhos, Grossos, Guamaré, Macau, Maxaranguape, Natal, Nísia Floresta, Parnamirim, Pedra Grande, Porto do Mangue, Rio do Fogo, São Bento do Norte, São Miguel do Gostoso, Senador Georgino Avelino, Tibau, Tibau do Sul e Touros", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "RN", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Baía da Traição, Cabedelo, Conde, João Pessoa, Lucena, Marcação, Mataraca, Pitimbu, Rio Tinto e Santa Rita", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "PB", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Barreiros, Cabo de Santo Agostinho, Goiana, Igarassu, Ilha de Itamaracá, Ipojuca, Jaboatão Guararapes, Olinda, Paulista, Recife, Rio Formoso, S. José Coroa Grande, Sirinhaém e Tamandaré", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "PE", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Barra de Santo Antônio, Barra de São Miguel, Coruripe, Feliz Deserto, Japaratinga, Jequiá da Praia, Maceió, Maragogi, Marechal Deodoro, Paripueira, Passo de Camaragibe, Piaçabuçu, Porto de Pedras, Roteiro e São Miguel dos Milagres", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "AL", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Aracaju, Barra dos Coqueiros, Brejo Grande, Estância, Itaporanga D'Ajuda, Pacatuba e Pirambu", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "SE", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Alcobaça, Belmonte, Cairu, Camaçari, Camamu, Canavieiras, Caravelas, Conde, Entre Rios, Esplanada, Igrapiúna, Ilhéus, Itacaré, Ituberá, Jaguaripe, Jandaíra, Lauro de Freitas, Maraú, Mata de São João, Mucuri, Nilo Peçanha, Nova Viçosa, Porto Seguro, Prado, Salvador, Santa Cruz Cabrália, Uma, Uraçuca, Valença e Vera Cruz", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "BA", municipio = mun[[1]], stringsAsFactors = F))


mun = munsplit("Anchieta, Aracruz, Conceição da Barra, Fundão, Guarapari, Itapemirim, Linhares, Marataízes, Piúma, Presidente Kennedy, São Mateus, Serra, Vila Velha e Vitória", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "ES", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Angra dos Reis, Araruama, Armação dos Búzios, Arraial do Cabo, Cabo Frio, Campos dos Goytacazes, Carapebus, Casimiro de Abreu, Duque de Caxias, Guapimirim, Itaboraí, Itaguaí, Macaé, Magé, Mangaratiba, Maricá, Niterói, Paraty, Quissamã, Rio das Ostras, Rio de Janeiro, São Francisco do Itabapoana, São Gonçalo, São João da Barra e Saquarema", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "RJ", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit(" Bertioga, Cananéia, Caraguatatuba, Cubatão, Guarujá, Iguape, Ilha Comprida, Ilhabela, Itanhaém, Mongaguá, Peruíbe, Praia Grande, Santos, São Sebastião, São Vicente e Ubatuba", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "SP", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Guaraqueçaba, Guaratuba, Matinhos, Paranaguá, Pontal do Paraná e Pontal do Sul", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "PR", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Araquari, Araranguá, Balneário Arroio do Silva, Balneário Barra do Sul, Balneário Camboriú, Balneário Gaivota, Balneário Piçarras, Balneário Rincão, Barra Velha, Biguaçu, Bombinhas, Florianópolis, Garopaba, Governador Celso Ramos, Imbituba, Itajaí, Itapema, Itapoá, Jaguaruna, Joinville, Laguna, Navegantes, Palhoça, Passo de Torres, Paulo Lopes, Penha, Porto Belo, São Francisco do Sul, São José e Tijucas", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "SC", municipio = mun[[1]], stringsAsFactors = F))

mun = munsplit("Arroio do Sal, Balneário Pinhal, Capão da Canoa, Cidreira, Imbé, Mostardas, Osório, Palmares do Sul, Pelotas, Rio Grande, Santa Vitória do Palmar, São José do Norte, Tavares, Terra de Areia, Torres, Tramandaí e Xangri-Lá", split = c(",", "e"))
ZC = rbind(ZC, data.frame(UF = "RS", municipio = mun[[1]], stringsAsFactors = F))

ZC$municipio = trimws(ZC$municipio)

write.csv(ZC, "ZC.csv")
