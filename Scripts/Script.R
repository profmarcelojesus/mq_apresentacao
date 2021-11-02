#Pacakge----

pacakge_list <- c("bibliometrix", "dplyr", "foreach", "stringr", 
                  "knitr", "plotly", "wordcloud2", "rvest", "RColorBrewer")
new_packages <- pacakge_list[!(pacakge_list %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

library(rvest) # baixa infos da web
library(RColorBrewer) # paletas legais
library(htmlwidgets) # salva em html
library(webshot) # ajuda a salvar
library(wordcloud2)
library(bibliometrix)
library(dplyr)
library(foreach)
library(stringr)
library(knitr)
library(plotly)

webshot::install_phantomjs()

#Load----

Wos <- convert2df("Dados/Wos.bib", dbsource = "wos", format = "bibtex")
Sco <- convert2df("Dados/Sco.bib", dbsource = "scopus", format = "bibtex")

#MergeDB----

nW <- nrow(Wos)
nS <- nrow(Sco)
artigos <- mergeDbSources(Wos, Sco, remove.duplicated = T)
a_min <- min(artigos$PY[!is.na(artigos$PY)])
a_max <- max(artigos$PY[!is.na(artigos$PY)])
n_WS <- nrow(artigos)
n_excdup <- (nW+nS)-n_WS
n_antfilter <- nrow(artigos)
artigos <- filter(artigos, PY >= 2016)
n_filter <- nrow(artigos)

#Limpeza----

for(i in 1:ncol(artigos)) {
  artigos[,i] <- str_replace_all(artigos[,i],fixed("\\&"), "AND")
}

#Excluindo informações de copyright
artigos$AB <- str_replace_all(artigos$AB, "\\(C\\) \\d\\d\\d\\d \\w+.*\\.", "")

#DupDOI----

artigos <- artigos[order(artigos$DI),]
dup <- foreach(i = 1:nrow(artigos), .combine = rbind) %do% {
  doi <- artigos$DI[i]
  doi_2 <- artigos$DI[i+1]
  if( is.na(doi_2)) {
    return(F)
    break
  }
  return(doi == doi_2)
}
artigos <- cbind(artigos, dup)
Dup <- filter(artigos, dup == T)
artigos <- filter(artigos, dup == F)
n_doi <- nrow(Dup)

#DupSR----

artigos$dup <- NULL
n <- nrow(artigos)
artigos <- duplicatedMatching(artigos, Field = "SR", exact = T)
n_sr <- n - nrow(artigos)

#DupTI----

artigos$dup <- NULL
artigos <- artigos[order(artigos$TI, decreasing = T),]
dup <- foreach(i = 1:nrow(artigos), .combine = rbind) %do% {
  ti <- artigos$TI[i]
  ti_2 <- artigos$TI[i+1]
  if( is.na(ti_2)) {
    return(F)
    break
  }
  return(str_detect(ti, pattern = ti_2))
}
artigos <- cbind(artigos, dup)
Dup <- filter(artigos, dup == T)
artigos <- filter(artigos, dup == F)
n_ti <- nrow(Dup)
n <- nrow(artigos)

#Categorização----

artigos[, c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- c(NA, NA, NA, NA, NA, NA)

# resumos <- paste(artigos$SR, artigos$TI, tolower(artigos$AB), sep = " - ")

# resumos[1:10]

artigos["MCCAMPBELL M, 2018, NJAS-WAGEN J LIFE SCI", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Extension-Research", "Production Systems", "Plant diseases", 
    "Use of Information and Communication Technologies (ICT) and Citizen Science Tools", NA)

artigos["LALIKA MCS, 2017, ECOHYDROL HYDROBIOL", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["HERMANS TDG, 2021, LAND DEGRAD DEV", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Technology", "Technology Adoption", "Conservation Agriculture", 
    "Social dynamics and information transfer, Contextual costs and benefits, Experience and risk aversion, and Practice adaptation", NA)

artigos["WYCHE S, 2016, INFORM TECHNOL DEV", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Information", "Access to Information", "Agricultural Market Information Services Adoption", 
    "Ecological perspective to creating mobile applications, reconsidering the design of mobile phones, and developing innovative educational interventions", NA)

artigos["SLAVOVA M, 2018, J ASSOC INF SYST", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Information", "Institutional Change", "Evolution of the prevailing smallholder institutional logic", 
    "Information artifacts with different interaction modalities", 
    "In-depth qualitative interviews, focus groups, observations, and detailed secondary quantitative data.")

artigos["CHAPMAN SA, 2021, AFR EVAL J", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Information", "Policy evaluation", "Evaluation approaches appropriate", 
    "Diagnostic evaluation approach", NA)

artigos["BEZA E, 2017, PLOS ONE", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Farm", "Citizen science projects", "Farmers' motivations to participate as citizen scientists and mobile telephone usage", "Education level, gamification", "Principal component analysis")

artigos["STAUDACHER P, 2021, ENVIRON HEALTH", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Suppliers", "Application and handling of pesticides", 
    "Role of agro-input dealers in transmitting safety information to smallholder farmers", 
    "Affordable, accessible and repeated training and shop inspections", 
    "Structured interviews, observations")

artigos["DIRWAI TL, 2019, WATER POLICY", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["KIRSCH H, 2018, ESPACIO ABIERTO", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["HOANG HG, 2020, CLIM CHANGE", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Farm", "Climate change", "Determinants of smallholders' perception on climate change", 
    "Farming practice, ict owned, community-based organisation participation, 
    credit programme participation and education", "logistic regression")

artigos["MAVHUNDUSE F, 2019, AFR J LIBR ARCHIV INF SCI", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Extension-Research", "Utilisation of mobile phones", 
    "Use of mobile phones for information access with extension workers intermediation", 
    "Increasing the information handling skills of extension officers", "Interviews")

artigos["HUDSON HE, 2017, TELECOMMUN POLICY", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T,"Farm-Ongs","Radio and interactive icts", 
    "Strategies to provide information to smallholder farmers", 
    "ict-enhanced participatory radio campaign approach", NA)

artigos["CRONKLETON P, 2021, SUSTAINABILITY", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Farm", "Womens engagement", "Under-represented peoples in decision-making processes ", 
    "Social learning participatory approaches", 
    "Participatory action research (par), Facilitated knowledge exchange")

artigos["WRIGHT HJ, 2016, J AGRIC FOOD INF1", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Extension-Research", "Agricultural extension systems", "Use of icts", 
    "Tablets, Short message service (sms)", NA)

artigos["KIBRET KS, 2020, EUROPEAN J REMOTE SENS", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["HUNG GIA HOANG HGH, 2021, INF DEV", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["PRAMUWIDYATAMA MG, 2020, FRONT VET SCI", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["THINDA KT, 2020, LAND USE POL", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["ADEBIYI J, 2016, RENEW AGRIC FOOD SYST", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["HOFFECKER E, 2021, WORLD DEV", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["RAZA MH, 2019, J CLEAN PROD", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(F, NA, NA, NA, NA, NA)

artigos["THI TAM DUONG TTD, 2019, CROP PROT", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Farm-Extension-Research", "Biosecurity", "Non-english speaking farmers", 
    "relevant extension services and education programs with more transparent and direct communications of risk and compliances", NA)

artigos["HAYES L, 2017, PREV VET MED", 
        c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
  c(T, "Interinstitutional", "Communication network", 
    "relationships between smallholders and the organisations and individuals from which they seek information, assistance and support", "Engagement of government, industry organisations and all stakeholders involved with smallholders", NA)

# ANTERIORES

# artigos["MANZEKE GM, 2014, FIELD CROP RES", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(F, NA, NA, NA, NA, NA)

# artigos["SHACKLETON S, 2015, WILEY INTERDISCIP REV CLIM CHANGE", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(F, NA, NA, NA, NA, NA)

# artigos["VALDIVIA C, 2014, AGRIC HUMAN VALUES", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "Farm-Extension-Research", "Translational research", 
#     "The role of a translational research process to enhance farmers' voice", 
#     "two-way communication participatory process", "Translational research")

# artigos["DESTA TT, 2012, TROP ANIM HEALTH PROD", c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(F, NA, NA, NA, NA, NA)

# artigos["KILELU CW, 2013, AGRIC SYST", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "Interinstitutional", "Agricultural innovation systems", 
#     "platforms as intermediaries in innovation systems", 
#     "mechanisms that strengthen feedback, learning and adaptive management in innovation processes", 
#     "Case study")

# artigos["LYLE G, 2015, J RURAL STUD", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "Farm-Farm", "Climate change adaptation", 
#     "Climate change adaptation decision making process", 
#     "Tailored plans of communication and dissemination of cc and cca information", 
#     "Literature review")

# artigos["KASSIE BT, 2013, ENVIRON MANAGE", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "FARM-INFORMATION", "CLIMATE CHANGES", 
#     "SMALL-HOLDER FARMERS IN ETHIOPIA ARE FACING SEVERAL CLIMATE RELATED HAZARDS, IN PARTICULAR HIGHLY VARIABLE RAINFALL WITH SEVERE DROUGHTS WHICH CAN HAVE DEVASTATING EFFECTS ON THEIR LIVELIHOODS", "CRITICAL TECHNOLOGICAL, INSTITUTIONAL, AND MARKET-ACCESS CONSTRAINTS NEED TO BE REMOVED; BETTER COMMUNICATION AND CAPACITY BUILDING", "CASE STUDY; HOUSEHOLD QUESTIONNAIRE; INTERVIEWS; FOCUS GROUP DISCUSSIONS")
# 
# artigos["FLOR RJB, 2011, WILDL RES", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "FARM-EXTENSION-RESEARCH", "LOSSES TO RODENTS AND UNDERNOURISHMENT", 
#     "DEVELOPMENT OF COMMUNICATION CAMPAIGNS THAT INCREASE COMMUNITY INVOLVEMENT IN RODENT MANAGEMENT", 
#     "A MEDIA CAMPAIGN WITH SUPPORT FROM LOCAL LEADERS AND EXTENSION STAFF", NA)
# 
# artigos["CASTELLANOS EJ, 2013, ENVIRON SCI POLICY", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "FARM-EXTENSION-RESEARCH", "INTERDISCIPLINARY PROJECT OF KNOWLEDGE CO-PRODUCTION", 
#     "THE DIFFICULTIES IN ACHIEVING KNOWLEDGE 'CO-PRODUCTION' WITH STAKEHOLDERS WHOSE DAY-TO-DAY EXISTENCE FOCUSES ON ISSUES LARGELY OUTSIDE THE DOMAIN OF THE RESEARCH PROGRAM", "EXPERIMENTED DIVERSE MODES OF STAKEHOLDER INTERACTION AND, THROUGH COLLABORATION WITH LOCAL EXPERTS IN COMMUNICATION STRATEGIES, IDENTIFIED A SET OF TOOLS FOR SUCCESSFUL DISSEMINATION OF RESULTS", NA)
# 
# artigos["WURZINGER M, 2011, SMALL RUMINANT RES", 
#         c("INCLUDE","CONTEXT","THEME","PROBLEM","SOLUTION","METHOD")] <- 
#   c(T, "FARM-EXTENSION-RESEARCH", "SMALLHOLDER BREEDING PROGRAMS", 
#     "BREEDING PROGRAMS IN DEVELOPING COUNTRIES FAILED BECAUSE OF NEGLECTING BASIC CONDITIONS, PARTICULARLY THE NO INVOLVEMENT OF FARMERS IN A PARTICIPATORY MANNER FROM DESIGN TO THE IMPLEMENTATION PHASE OF THE PROGRAMS", 
#     "INSTITUTIONAL STRENGTHENING FOR EFFECTIVE APPLICATION OF PARTICIPATORY TOOLS AND KNOWLEDGE EXCHANGE BETWEEN FARMERS AND RESEARCHERS", "CASE STUDY")

n_inc <- dplyr::filter(artigos, INCLUDE == "TRUE")
n_inc <- nrow(n_inc)

#Resultados----

results <- biblioAnalysis(artigos)
S <- summary(object = results, k = 20, pause = F, verbose = F)

# Colaboração científica entre países
M <- metaTagExtraction(artigos, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
colab_country <- networkPlot(NetMatrix, n = 30, Title = "Colaboração Científica entre Países", 
                             type = "fruchterman", size=T, remove.multiple=F, labelsize=1, alpha = 1)
# # Rede de co-citações
# NetMatrix <- biblioNetwork(artigos, analysis = "co-citation", network = "references", sep = ";")
# net_cocitation <- networkPlot(NetMatrix, n = 50, Title = "Redes de Co-citações", type = "fruchterman", 
#                               size.cex=TRUE, size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 1, edges.min=5)

# Histórico de citações
# M=metaTagExtraction(artigos,"CR_SO",sep=";")
# histResults <- histNetwork(M, min.citations=quantile(M$TC,0.75), sep = ";")
# options(width = 130)
# net <- histPlot(histResults, n=20, size.cex=TRUE, size = 5, labelsize = 3, arrowsize = 0.5)

# Rede de Parcerias
NetMatrix <- biblioNetwork(artigos, analysis = "coupling", network = "authors", n = NULL, sep = ";", 
                           short = F, shortlabel = T)
net_coupling <- networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Parcerias", 
                            type = "fruchterman", remove.multiple=T, size=T, edgesize = 1, labelsize=1, alpha = 1)
# Ligações entre autores
net_authors <- couplingMap(artigos, analysis = "authors", field = "CR", n = 30, 
                           impact.measure="global", minfreq = 3, size = 0.5, repel = T)
# Ligações entre documentos
net_documents <- couplingMap(artigos, analysis = "documents", field = "DE", n = 50, 
                             impact.measure="global", minfreq = 3, size = 0.5, repel = T)

# Ligações entre fontes
net_sources <- couplingMap(artigos, analysis = "sources", field = "DE", n = 50, 
                           impact.measure="global", minfreq = 2, size = 0.5, repel = T)
# Co-ocorrências de Palavras-chaves
NetMatrix <- biblioNetwork(artigos, analysis = "co-occurrences", network = "author_keywords", sep = ";")
net_author_keywords <- networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, halo = F, 
                                   Title = "Co-ocorrências de Palavras-chaves", type = "fruchterman", 
                                   size=T, edgesize = 0.1, labelsize=1, verbose = F, alpha = 0.9)
# Estrutura Conceitual (MCA)
# B <- bradford(artigos)
# B <- left_join(artigos, B$table)
# B <- filter(B, Zone == "Zone 1")
CS <- conceptualStructure(artigos,field="AB", method="MCA", minDegree=10, clust="auto", ngrams = 2,
                          k.max=2, stemming=T, labelsize=12, documents=10, graph = F)

# Produtividade dos Principais Autores
aut <- authorProdOverTime(artigos, k = 10, graph = F)
aut$graph+labs(title = "Produtividade dos Autores no Período", 
               x = "Autores", y = "Anos")+theme(text = element_text(size = 10)) 

# Treemap
threeFieldsPlot(artigos, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

# Nuvem de palavras
# Renata Muylaert 
# 01/09/20
B <- termExtraction(artigos, Field = "AB", ngrams = 2, stemming = T,
                    language = "english", remove.numbers = T, remove.terms = NULL,
                    keep.terms = c("Extension", "Communication", "Research"), 
                    synonyms = NULL, verbose = F)

tsplit <- unlist(strsplit(B$AB_TM, ";"))
tsplit <- tolower(tsplit)
corpus <- data.frame(table(tsplit))
corpus <- corpus[rev(order(corpus$Freq)),]
corpus$tsplit <- as.character(corpus$tsplit)
padroes_indesejados <- c("smallhold farmer", "right reserv", "smallhold farm")
# Versao final do corpus de palavras 
corpus <- corpus[!corpus$tsplit %in% padroes_indesejados,]
corpus <- corpus[corpus$Freq > 4,]
# Criando a nuvem com wordcloud2

nuvem <- wordcloud2(data = corpus, size = 0.4, minRotation = 0.2, rotateRatio = 0.8)

library("htmlwidgets")

saveWidget(nuvem,"tmp.html",selfcontained = F)

webshot("tmp.html","Imagens/wordcloud.png", delay=20, vwidth=800, vheight=480)


