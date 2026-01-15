
# Throat clearing ---------------------------------------------------------


rm(list=ls())
library(skimr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(png)
library(patchwork)
library(countrycode)
library(tidyverse)
library(readxl)

mainpath<-"//data2/AFR/DATA/Regional Studies/High Frequency Monitor/Other Projects and Indicators/"

source(paste0(mainpath,"Weather Report/sourcetemplate.R"))
ssalist<-SSAlist
slidetheme<-slidetheme+
  theme(panel.border = element_blank(),
        axis.line = element_line(color="grey20"),
        axis.ticks = element_line(color="grey20"),
        axis.text.y = element_text(margin=margin(0,10,0,0,"pt")))
theme_set(slidetheme)
# code.list<-c("NGDPRPC")
library(reticulate)
# use_python("C:/ProgramData/Anaconda3", required=TRUE)
imf_datatools <- import("imf_datatools")
ecosextract<-function(code.list, country.list, database, start, end, frq){
  ecos<-imf_datatools$get_ecos_sdmx_data(database, country.list,code.list, freq = frq)%>%
    mutate(date=as.Date(row.names(.)))%>%
    mutate(year=year(date))%>%
    # select(-date)%>%
    filter(year>=start & year<=end)%>%
    select(date, everything())%>%
    gather(code, value, -c(year, date))%>%
    mutate(variable=str_sub(code,start=4, end=-3),
           imf=as.numeric(str_sub(code,1,3)))%>%
    mutate(iso3c=countrycode(imf,"imf", "iso3c"))%>%
    select(-code)%>%
    spread(variable, value) %>% 
    arrange(imf,date)%>%
    mutate(iso3c=countrycode(imf,"imf", "iso3c"))%>%
    ungroup() %>% 
    as.tibble()
  return(ecos)
}
bbextract<-function(code.list, start, end){
  ecos<-imf_datatools$get_ecos_bloomberg_data(code.list, field='PX_LAST')%>%
    mutate(date=as.Date(row.names(.)))%>%
    filter(date>=start & date<=end)%>%
    select(date, everything())%>%
    pivot_longer(!date, names_to = "code", values_to = "value")%>%
    mutate(variable=str_sub(code,start=1, end=8)) %>% 
    select(-code)%>%
    pivot_wider(names_from=variable, values_from=value) %>% 
    ungroup() %>% 
    mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
    as.tibble()
  return(ecos)
}
# bbseries<-imf_datatools$ecos_sdmx_utilities$get_all_series('ECDATA_BLOOMBERG') %>%
#   data.frame()
# Data for Spreads charts -----------------------------------------------------------


# Bloomberg data

series.list<-c('JPSSGAOB Index', # AGO Yes
               "JPSSGDGH Index", # GHA Yes
               "JPGCKESS Index", #KEN Yes
               "JPSSGNIG Index", #NGA Yes 
               "JPSSGSN Index", #SEN/WAEMU Yes
               "JPSSGSAF Index", #ZAF Yes
               # "JPSSGTZB Index", #TZA No
               "JPSSGZMB Index", #ZMB Yes
               "JPSSGCDI Index", #CIV Yes
               "JPEGETSS Index", #ETH Yes
               "JPSSGGA Index", #GAB Yes
               "JPSSGMZB Index",#MOZ Yes
               "JPSSGNMB Index",#NAM Yes
               # "JPGCCMSS Index",#CMR No
               "JPEIGLSP Index"
               # ,#EMBI Yes
               # "USGG10YR Index"#USA Yes
)

raw.s = bbextract(series.list, "2019-06-30", Sys.Date()) 
raw.s %>% tail()
max.date<-raw.s %>% na.omit %>% pull(date) %>% max()
# bbseries %>% slice(1:6) %>% unlist()
# AFCRSUEZ Comdty
# test<-imf_datatools$ecos_sdmx_utilities$get_ecos_sdmx_metadata("ECDATA_BLOOMBERG", bbseries %>% slice(1:6) %>% unlist())

data.s<-raw.s %>%  
  tidyr::fill(everything()) %>% 
  filter(date<=max.date)
plotlist<-data.frame(code=c('JPSSGAOB', # AGO Yes
                             "JPSSGDGH", # GHA Yes
                             "JPGCKESS", #KEN Yes
                             "JPSSGNIG", #NGA Yes 
                             "JPSSGSN ", #SEN/WAEMU Yes
                             "JPSSGSAF", #ZAF Yes
                             "JPSSGZMB", #ZMB Yes
                             "JPSSGCDI", #CIV Yes
                             "JPEGETSS", #ETH Yes
                             "JPSSGGA ", #GAB Yes
                             "JPSSGMZB",#MOZ Yes
                             "JPSSGNMB",#NAM Yes
                             "JPEIGLSP",
                            "JPEEBJSS"),
                     iso3c=c(c("AGO", "GHA", "KEN","NGA","SEN", "ZAF", "ZMB", "CIV",
                               "ETH", "GAB", "MOZ", "NAM", "EMBI", "BEN")
                     ))#EMBI Yes

data.s %>% tail(10)

# Add latest data from BBG terminal download


file<-"Market_indicators.xlsx"
latest.head.s<-read_excel(paste0(mainpath,file), sheet = "EMBIG Daily", range="B4:T5")
codes.head.s<-latest.head.s %>% unlist()
codes.latest.s<-latest.head.s %>%
  select(-1)%>% 
  pivot_longer(1:18, names_to = "country", values_to = "index") %>% 
  na.omit()
latest.data.s<-read_excel(paste0(mainpath,file), sheet = "EMBIG Daily", range="B7:T15",
                          col_types = c("date", rep("numeric", 18))) %>%
  purrr::set_names(nm=names(latest.head.s))%>%
  rename(date=1)%>% 
  filter(date>as.Date(max.date))%>% 
  pivot_longer(2:19, names_to = "country", values_to = "value") %>% 
  na.omit()
append.data.s<-codes.latest.s %>% left_join(latest.data.s)%>% 
  mutate(index=str_sub(index,start=1, end=8)) %>% 
  select(date, index, value) %>% 
  pivot_wider( names_from = index, values_from = value) %>% 
  mutate(JPSSGNMB=NA) %>% 
  select(names(data.s)) %>% 
  arrange(date)


data.s<-data.s %>% 
  rbind(append.data.s)%>% 
  filter(date!=as.Date(Sys.Date()))

data.s %>% tail()

# Add data from Benin, only available on terminal -------------------------
file<-"Market_indicators.xlsx"
benin.s<-read_excel(paste0(mainpath,file), sheet = "EURO EMBI",
                          range="B7:E1500", col_types =c("date", rep("numeric", 3))) %>% 
  select(c(1,4)) %>% 
  rename(date=1, JPEEBJSS=2) %>% 
  mutate(date=as.Date(date)) 

data.s<-data.s %>% left_join(benin.s) %>% 
  arrange(date) %>% 
  fill(JPEEBJSS)
data.s %>% tail()
# Ribbon Chart ------------------------------------------------------------
spreads.data<-data.s%>% 
  rowwise() %>% 
  mutate(SSAxZAM=mean(c(JPSSGSAF,JPSSGCDI,JPSSGNIG,`JPSSGGA `,JPSSGAOB,`JPSSGSN `,JPSSGDGH), na.rm=T),
         SSA=mean(c(JPSSGSAF,JPSSGCDI,JPSSGNIG,`JPSSGGA `,JPSSGAOB,`JPSSGSN `,JPSSGDGH, JPSSGZMB), na.rm=T))%>% 
  select(date, SSA, JPEIGLSP)%>% 
  # select(date, SSAxZAM, JPEIGLSP) %>% 
  rename(EMBI.Global=3) %>% 
  filter(date>=as.Date("2018-06-30"))%>% 
  ungroup() %>% 
  mutate(SSAbase=sum(ifelse(date==as.Date("2020-1-14"),SSA,0)),
         EMbase=sum(ifelse(date==as.Date("2020-1-14"),EMBI.Global,0)))%>% 
  mutate(SSA.new=SSA-SSAbase,
         EM.new=EMBI.Global-EMbase)
# spreads.data %>% filter(date>=as.Date("2022-02-21")) %>% data.frame()


plot.data<-spreads.data %>%
  filter(date>=as.Date("2019-6-30")) %>%
  mutate(gap=SSA-EMBI.Global)
gap1.data<-plot.data %>% filter(year(date)==2020) %>% filter(gap==max(gap, na.rm=T)) %>% mutate(gap=round(gap,0))
gap2.data<-plot.data %>% slice(n()) %>% mutate(gap=round(gap,0))
lastobs<-plot.data %>% slice(n()) %>% pull(date) %>% format("%d %b %Y")
g.ribbon<-ggplot(plot.data)+ 
  geom_ribbon(aes(date, ymin=EMBI.Global, ymax=SSA), fill=REOmedblue, alpha=.3)+
  geom_line(aes(date, SSA), col=REOred, size=1)+
  annotate("rect", xmin=as.Date("2022-2-22"), xmax = as.Date (Sys.Date()),
           ymin=-Inf, ymax = Inf, fill="grey40", alpha=.3)+
  # geom_line(aes(date, SSA.new), col=REOred, size=1)+
  geom_line(aes(date, EMBI.Global), col=REOblue, size=1)+
  # geom_line(aes(date, EM.new), col=REOblue, size=1)+
  scale_x_date(breaks=seq(as.Date("2020-1-1"), Sys.Date(), by="12 months")-1,
               date_labels = "%b %Y")+
  geom_segment(aes(x=date+5, xend=date+5,y=EMBI.Global+50, yend=SSA-120),
               arrow = arrow(ends = "both",
                             length=unit(0.01, "npc"),
                             type="closed"),
               col="grey30", data=gap1.data)+
  geom_segment(aes(x=date+5, xend=date+5,y=EMBI.Global+50, yend=SSA-50),
               arrow = arrow(ends = "both",
                             length=unit(0.01, "npc"),
                             type="closed"),
               col="grey30", data=gap2.data)+
  annotate("text", x=as.Date("2021-09-30"), y=600, label="SSA", col=REOred, fontface="bold", size=2.5)+
  annotate("text", x=as.Date("2022-01-05"), y=900, label="War", col="grey30", fontface="bold", size=2.5)+
  annotate("text", x=as.Date("2021-09-30"), y=400, label="EMBIG", col=REOblue, fontface="bold", size=2.5)+
  annotate("text", x=gap1.data$date+45, y=800, label=gap1.data$gap, col="grey30", fontface="bold", size=2.5)+
  annotate("text", x=gap2.data$date+45, y=600, label=gap2.data$gap, col="grey30", fontface="bold", size=2.5)+
  theme(axis.text = element_text(size=7),
        plot.subtitle = element_text(size=8))+
  plot_annotation(title="SSA Sovereign Spreads, 2019-22",
                  subtitle = paste0("(bps, simple average as of ",lastobs,")"),
                  caption="Source: Bloomberg and IMF staff calculations")&
  theme(plot.title=element_text(size=8, face="bold"),
        plot.subtitle=element_text(size=7),
        plot.caption = element_text(size=6, color="grey30"))
ggsave(g.ribbon,filename = "ribbon.png",  height = 2.5, width=5, dpi=600,units="in", device="png", bg = "transparent")
file.show("ribbon.png") 

# Weekly Spread dispersion ------------------------------------------------



# Spreads panel  -----------------------------------------------------------

  start="2022-1-1"
  plot.data.s<-data.s %>% 
    select(-c(JPEIGLSP)) %>% 
    filter(date>=as.Date(start)) %>% 
    pivot_longer(-date,names_to = "code", values_to = "value") %>% 
    arrange(code,date) %>% 
    group_by(code) %>% 
    fill(value) %>% 
    mutate(base=sum(ifelse(date==as.Date(start),value,0))) %>% 
    mutate(change=value-base) %>% 
    mutate(max=max(change),
           min=min(change),
           latest=last(value),
           week=nth(value, -8))%>% 
    slice(n()) %>% 
    ungroup() %>% 
    arrange(latest) %>% 
    mutate(labelcol=case_when(
      value<=700~"blue",
      value>700&value<=1000~"grey",
      value>1000~"red",
      TRUE~NA_character_
    )) %>% 
    left_join(plotlist, by="code") %>% 
    mutate(country=countrycode(iso3c, "iso3c", "country.name"))
  spark.data.s<-data.s %>% 
    select(-c(JPEIGLSP)) %>% 
    filter(date>=as.Date(start)) %>% 
    pivot_longer(-date,names_to = "code", values_to = "value") %>% 
    arrange(code,date) %>% 
    group_by(code) %>% 
    fill(value) %>% 
    mutate(base=sum(ifelse(date==as.Date(start),value,0))) %>% 
    mutate(change=value-base) %>% 
    mutate(max=max(change),
           min=min(change),
           latest=last(value),
           week=nth(change, -8)) %>% 
    mutate(pos=ifelse(change>=0,change,0),
           neg=ifelse(change<0,change,0))
last.date.s<-plot.data.s %>% pull(date) %>% max() %>% format("%d %b %Y")
p.1<-ggplot(plot.data.s)+geom_segment(aes(x=base, xend=latest, y=fct_reorder(country, latest), yend=country, col=labelcol), size=2, alpha=.7)+
    # geom_point(aes(max,country), size=8, col="grey70", shape="|")+
    # geom_point(aes(min,country), size=8, col="grey70", shape="|")+
    geom_point(aes(latest,country, col=labelcol), size=4)+
  scale_x_log10()+
    geom_point(aes(week,country), size=4, col="grey30", shape="|")+
    # geom_vline(aes(xintercept = 0), lty=2, col="grey30")+
    labs(title = paste0("(Basis points, change since 1 Jan 2022, as of ",last.date.s,". Log Scale)"))+
  scale_color_manual(values=c(REOblue, "grey30", "firebrick"), guide=F)+
    theme(plot.title.position = "plot",
          panel.grid.major.x = element_line(color="grey90", linetype=2),
          panel.grid.major.y=element_line(color="grey90", linetype=2, size=.5),
          plot.title = element_text(face="plain"))
p.1
p.2<- ggplot(spark.data.s)+
  geom_ribbon(aes(date, ymin=0, ymax=pos), fill="firebrick", alpha=.5)+
  geom_ribbon(aes(date, ymax=0, ymin=neg), fill=REOblue, alpha=.5)+
  geom_point(aes(date, change), col=REOred, size=1, data = . %>% slice(n()))+
  # geom_hline(aes(yintercept=max), col="grey80", size=.5)+
  # geom_hline(aes(yintercept=min), col="grey80", size=.5)+
  facet_grid(fct_reorder(code, -latest)~., scale="free")+
  scale_x_date(breaks = seq(as.Date("2022-1-1"), as.Date(Sys.Date()), by="3 months"),
               date_labels = "%b")+
  theme(strip.text = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x=element_line(color="grey30"),
        axis.ticks.y=element_blank())

p.3<-ggplot(plot.data.s)+
    geom_segment(aes(x=.5, xend=1.5, y=fct_reorder(country, latest), yend=country, col=labelcol), size=6, alpha=.9)+
    scale_color_manual(values=c(REOblue, "grey30", "firebrick"), guide=F)+
    geom_text(aes(1,fct_reorder(country,latest), label=format(round(value, 0), big.mark=",")), col="white", fontface="bold", size=3)+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank())+
    theme_map()+
  coord_cartesian(xlim = c(0,2))+
    labs(title = "(Latest Level)")
p.4<-ggplot()+geom_segment(aes(x=.5, xend=3.5, y=1, yend=1), size=2, col=REOblue)+
  geom_point(aes(3.5,1), size=4, col=REOblue)+
  geom_point(aes(1.5,1), size=4, col="grey30", shape="|")+
  # geom_point(aes(0,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(5,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(.5,1), size=3, col="grey30", shape="|")+
  # annotate("text", x=0,y=1, label="Range\nMinimum", vjust=1.6, hjust=0,col="grey50", size=3)+
  annotate("text", x=.5,y=1, label="1 Jan 2022", vjust=3.2, hjust=0.5,col=REOblue, size=3)+
  # annotate("text", x=5,y=1, label="Range\nMaximum", vjust=1.6, hjust=1, col="grey50", size=3)+
  annotate("text", x=3.5,y=1, label="Latest", vjust=3.2, col=REOblue, size=3)+
  annotate("text", x=1.5,y=1, label="Week\nago", vjust=1.5, col="grey30", size=3)+
  theme_void()
p.5<-ggplot()+
  geom_segment(aes(x=0, xend=3, y=1, yend=1), size=6, col=REOblue)+
  geom_segment(aes(x=4, xend=7, y=1, yend=1), size=6, col="grey30")+
  geom_segment(aes(x=8, xend=11, y=1, yend=1), size=6, col="firebrick")+
  annotate("text", x=1.5,y=1, label="Performing", col="white", fontface="bold", size=3)+
  annotate("text", x=5.5,y=1, label="Stressed", col="white", fontface="bold", size=3)+
  annotate("text", x=9.5,y=1, label="Distress", col="white", fontface="bold", size=3)+
  theme_void()
(panel1<-(p.1+p.2+p.3+plot_layout(widths = c(7,1.5,1.5)))/(p.4+p.5)+plot_layout(heights = c(4.5,1.5)))
ggsave(filename = "panela.png",  height = 4, width=9, dpi=600,units="in", device="png", bg = "transparent")
file.show("panela.png")

# Exchange Rate panel -----------------------------------------------------


# ifs.codes<-c("RAXG_USD", "RAFA_G_USD", "RAFA_MV_USD")
ifs.codes<-c("RAFA_MV_USD")
ifs<-ecosextract(ifs.codes, as.character(ssa.imf), "ECDATA_IFS", 2021, 2022, "M")
imports<-ecosextract("BMGS_BP6", as.character(ssa.imf), "WEO_WEO_LIVE", 2022, 2022, "A")


erplot.list<-c("AGO", "BWA", "BDI", "COD", "ERI", "ETH", "GMB", "GHA", "GIN",
               "KEN", "LBR", "MDG", "MWI", "MUS", "MOZ", "NGA", "RWA", "SYC",
               "SLE", "ZAF", "SSD", "TZA", "UGA", "ZMB")
countrycode(erplot.list, "iso3c", "country.name")
c(WAEMU, CEMAC)
setdiff(SSAlist,c(erplot.list, WAEMU,CEMAC))
erbbg.list<-c("USDAOA Curncy",#	Angola
"USDXOF Curncy",#	Cote d'Ivoire
"USDETB Curncy",#	Ethiopia
"USDXAF Curncy",#	Gabon
"USDGHS Curncy",#	Ghana
"USDKES Curncy",#	Kenya
"USDMUR Curncy",#	Mauritius
"USDMZN Curncy",#	Mozambique
"USDNGN Curncy",#	Nigeria
"RWF BNRW Curncy",#	Rwanda
# "USDXOF Curncy",#	Senegal
"USDZAR Curncy",#	South Africa
"USDTZS Curncy",#	Tanzania
"USDUGX Curncy",#	Uganda
"USDZMW Curncy",#	Zambia
"USDBWP Curncy",#	Botswana
"USDBIF Curncy",#	Burundi
"USDCDF Curncy",#	Congo, Dem. Rep.
# "USDERN Curncy",#	Eritrea
"USDGMD Curncy",#	Gambia
"USDGNF Curncy",#	Guinea
"USDLRD Curncy",#	Liberia
"USDMGA Curncy",#	Madagascar
"USDMWK Curncy",#	Malawi
"USDSCR Curncy",#	Seychelles
"USDSLL Curncy")#	Sierra Leone
# "USDSSP Curncy")#	South Sudan

raw.er<- bbextract(erbbg.list, "2021-06-30", Sys.Date()) 
max.date<-raw.er %>% na.omit %>% pull(date) %>% max()
er.data<-raw.er%>% 
  fill(everything()) %>% 
  filter(date<=max.date)
path<-"//data2/AFR/DATA/Regional Studies/High Frequency Monitor/Other Projects and Indicators/"

file<-"Market_indicators.xlsx"
latest.head.er<-read_excel(paste0(path,file), sheet = "Exchange Rate Daily", range="B4:BD5", na=c("#N/A N/A"))
codes.head.er<-latest.head.er %>% unlist()
codes.latest.er<-latest.head.er %>%
  select(-1) %>% 
  pivot_longer(1:54, names_to = "country", values_to = "index") 
latest.data.er<-read_excel(paste0(path,file), sheet = "Exchange Rate Daily", range="B7:BD15", na=c("#N/A N/A")) %>%
  purrr::set_names(nm=names(latest.head.er))%>%
  rename(date=1)%>% 
  filter(date>as.Date(max.date))%>% 
  pivot_longer(2:55, names_to = "country", values_to = "value")
append.data.er<-codes.latest.er %>% left_join(latest.data.er)%>% 
  mutate(index=str_sub(index,start=1, end=8)) %>% 
  select(date, index, value) %>% 
  unique()%>% 
  pivot_wider( names_from = index, values_from = value)%>% 
  select(names(er.data)) %>% 
  arrange(date)

plot.list.er<-codes.latest.er %>% 
  mutate(index=str_sub(index,start=1, end=8))

er.plot<-er.data %>% 
  filter(date>=as.Date(start)) %>% 
  rbind(append.data.er)%>% 
  filter(date!=as.Date(Sys.Date())) %>% 
  pivot_longer(-date, names_to = "index", values_to = "value") %>% 
  left_join(plot.list.er) %>% 
  select(-index) %>% 
  arrange(country, date) %>% 
  group_by(country)%>% 
  mutate(base=sum(ifelse(date==as.Date(start),value,0)))%>% 
  mutate(change=100*base/value-100) %>% 
  mutate(max=max(change),
         min=min(change),
         latest=last(change),
         week=nth(change, -8))%>% 
  slice(n())%>% 
  filter(!country%in%c("Cote d'Ivoire", "Gabon")) %>%
  mutate(country=ifelse(country=="Senegal","WAEMU/CEMAC", country)) %>% 
  mutate(country=ifelse(country=="South Africa","South Africa", country))
er.plot$country %>% unique()%>% countrycode("country.name", "iso3c")->templist
setdiff(SSAlist, c(WAEMU, CEMAC, templist))
spark.data.er<-er.data %>% 
  filter(date>=as.Date(start)) %>% 
  filter(date!=as.Date(Sys.Date())) %>% 
  pivot_longer(-date,names_to = "index", values_to = "value") %>% 
  left_join(plot.list.er) %>% 
  arrange(country,date) %>% 
  group_by(country) %>% 
  fill(value) %>% 
  mutate(base=sum(ifelse(date==as.Date(start),value,0)))%>% 
  mutate(change=100*base/value-100) %>% 
  mutate(max=max(change),
         min=min(change),
         latest=last(change),
         week=nth(change, -8))%>% 
  mutate(pos=ifelse(change>=0,change,0),
         neg=ifelse(change<0,change,0))%>% 
  filter(!country%in%c("Cote d'Ivoire", "Gabon")) %>%
  mutate(country=ifelse(country=="Senegal","WAEMU/CEMAC", country))
last.date.er<-er.plot %>% pull(date) %>% max() %>% format("%d %b %Y")

p.6<-ggplot(er.plot)+geom_segment(aes(x=0, xend=latest, y=fct_reorder(country, latest), yend=country), size=2, col=REOblue, alpha=.7)+
    # geom_point(aes(max,country), size=8, col="grey70", shape="|")+
    # geom_point(aes(min,country), size=8, col="grey70", shape="|")+
    geom_point(aes(latest,country), size=4, col=REOred)+
    geom_point(aes(week,country), size=4, col=REOred, shape="|")+
    geom_vline(aes(xintercept = 0), lty=2, col="grey30")+
    labs(title = paste0("(versus USD, percent change since 1 Jan 2022, as of ",last.date.er,")"))+
    theme(plot.title.position = "plot",
          panel.grid.major.x = element_line(color="grey90", linetype=2),
          panel.grid.major.y = element_line(color="grey90", linetype=2, size=.5),
          plot.title = element_text(face="plain"))
p.7<- ggplot(spark.data.er)+
  geom_ribbon(aes(date, ymin=0, ymax=pos), fill=REOblue, alpha=.5)+
  geom_ribbon(aes(date, ymax=0, ymin=neg), fill="firebrick", alpha=.5)+
  geom_point(aes(date, change), col=REOred, size=1, data = . %>% slice(n()))+
  # geom_hline(aes(yintercept=max), col="grey80", size=.5)+
  # geom_hline(aes(yintercept=min), col="grey80", size=.5)+
  facet_grid(fct_reorder(country, -latest)~., scale="free")+
  scale_x_date(breaks = seq(as.Date("2022-1-1"), as.Date(Sys.Date()), by="3 months"),
               date_labels = "%b")+
  theme(strip.text = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x=element_line(color="grey30"),
        axis.ticks.y=element_blank())


import.join<-imports %>% 
  select(c(iso3c, BMGS_BP6))

ifs.data<-ifs %>% 
  group_by(iso3c) %>% 
  na.omit() %>% 
  slice(n()) %>% 
  rename(reserves=5) %>% 
  mutate(date=date+months(1)-1) %>% 
  select(iso3c, date, reserves) %>% 
  left_join(import.join, by="iso3c") %>% 
  # mutate(cover=round(reserves/BMGS_BP6*100/12,1))
  mutate(cover=round(reserves/(BMGS_BP6/12),1))

ifs.plot<-er.plot %>% 
  select(-date) %>% 
  mutate(iso3c=countrycode(country, "country.name", "iso3c")) %>% 
  left_join(ifs.data, by="iso3c") %>% 
  select(country, latest, cover, date) %>% 
  mutate(labelcol=case_when(
    cover>=5~"blue",
    cover>=3 & cover<5~"grey",
    cover<3~"red", 
    TRUE~NA_character_
  ))

p.8<-ggplot(ifs.plot)+
  geom_segment(aes(x=.5, xend=1.5, y=fct_reorder(country, latest), yend=country, col=labelcol), size=6, alpha=.9)+
  scale_color_manual(values=c(REOblue, "grey30", "firebrick"), guide=F, na.value = "transparent")+
  geom_text(aes(1,fct_reorder(country,latest), label=format(round(cover, 1), big.mark=",")), col="white", fontface="bold", size=3)+
  geom_text(aes(2.3,fct_reorder(country,latest), label=format(date, "%b %y")), fontface="bold", size=3, col="grey30")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank())+
  coord_cartesian(xlim=c(0,3))+
  theme_map()+
  labs(title = "(Reserves)")
p.9<-ggplot()+geom_segment(aes(x=.5, xend=3.5, y=1, yend=1), size=2, col=REOblue)+
  geom_point(aes(3.5,1), size=5, col=REOred)+
  geom_point(aes(1.5,1), size=5, col=REOred, shape="|")+
  # geom_point(aes(0,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(5,1), size=6, col="grey70", shape="|")+
  geom_point(aes(.5,1), size=3, col="grey30", shape="|")+
  # annotate("text", x=0,y=1, label="Range\nMinimum", vjust=1.6, hjust=0,col="grey50", size=3)+
  annotate("text", x=.5,y=1, label="0", vjust=3.2, hjust=0.5,col="grey30", size=3)+
  # annotate("text", x=5,y=1, label="Range\nMaximum", vjust=1.6, hjust=1, col="grey50", size=3)+
  annotate("text", x=3.5,y=1, label="Latest", vjust=3.2, col="firebrick", size=3)+
  annotate("text", x=1.5,y=1, label="Week\nago", vjust=1.5, col="firebrick", size=3)+
  theme_void()

p.10<-ggplot()+
  geom_segment(aes(x=0, xend=3, y=1, yend=1), size=6, col=REOblue)+
  geom_segment(aes(x=4, xend=7, y=1, yend=1), size=6, col="grey30")+
  geom_segment(aes(x=8, xend=11, y=1, yend=1), size=6, col="firebrick")+
  annotate("text", x=1.5,y=1, label=">5 mths", col="white", fontface="bold", size=3)+
  annotate("text", x=5.5,y=1, label="3 - 5 mths", col="white", fontface="bold", size=3)+
  annotate("text", x=9.5,y=1, label="< 3 mths", col="white", fontface="bold", size=3)+
  theme_void()

(panel2<-((p.6+p.7+p.8+plot_layout(widths = c(7,1.5,1.5)))/(p.9+p.10)+plot_layout(heights = c(6.5,1.5))))

ggsave(filename = "panelb.png",  height = 6.2, width=9, dpi=600,units="in", device="png", bg = "transparent")
file.show("panelb.png")

# Capital flows -----------------------------------------------------------
# SSA Weekly bond flows ETFs and Mutual Funds
ssabond.list<-data.frame(codes=c(
        "CWBBAOC", "CWBBETC","CWBBGAC","CWBBGHC","CWBBICC",
        "CWBBKEC","CWBBMUC","CWBBMZC","CWBBNAC","CWBBNIC",
        "CWBBRWC","CWBBSFC","CWBBTZC","CWBBUGC","CWBBZAC"),
        database="EPFRBCF") %>% 
  mutate(index=paste0(codes,"@",database)) %>% 
  pull(index) %>% unlist()
raw.ssabonds<-imf_datatools$get_haver_data(ssabond.list)
raw.ssabonds[is.na(raw.ssabonds)] <- 0
data.ssabonds<-raw.ssabonds%>%
  mutate(date=as.Date(row.names(.))+6) %>%
  filter(date>=as.Date("2019-1-1"))%>% 
  select(date,(everything()))%>% 
  purrr::set_names(nm=c("date",
                        "AGO", "ETH", "GAB", "GHA", "CIV",
                        "KEN", "MUS", "MOZ", "NAM", "NGA",
                        "RWA", "ZAF", "TZA", "UGA", "ZMB"
                        )) %>% 
  pivot_longer(-date, names_to = "iso3c", values_to = "bonds")

# SSA Weekly equity flows ETFs and Mutual Funds
ssaequity.list<-data.frame(codes=c(
  "CWEBBOC","CWEBGHC","CWEBICC","CWEBKEC","CWEBMAC",
  "CWEBMUC","CWEBNIC","CWEBRWC","CWEBSFC","CWEBTZC",
  "CWEBZAC","CWEBZIC"),
  database="EPFRECF") %>% 
  mutate(index=paste0(codes,"@",database)) %>% 
  pull(index) %>% unlist()
raw.ssaequity<-imf_datatools$get_haver_data(ssaequity.list)  
raw.ssaequity[is.na(raw.ssaequity)] <- 0
data.ssaequity<-raw.ssaequity%>%
  mutate(date=as.Date(row.names(.))+6)%>%
  filter(date>=as.Date("2019-1-1")) %>% 
  select(date, everything()) %>% 
  purrr::set_names(nm=c("date",
                        "BWA", "GHA", "CIV", "KEN", "MWI",
                        "MUS", "NGA", "RWA", "ZAF","TZA",
                        "ZMB", "ZWE")) %>% 
  pivot_longer(-date, names_to = "iso3c", values_to = "equity")

data.ssatotal <-data.ssabonds %>% 
  full_join(data.ssaequity) %>% 
  arrange(iso3c, date) %>% 
  rowwise() %>% 
  mutate(total=sum(bonds,equity, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(iso3c) %>% 
  mutate(month=zoo::rollapply(total,4,sum, na.rm=T, align="right", fill=NA))

data.ssareg<-data.ssatotal %>% 
  group_by(date) %>% 
  summarize(total=sum(total),
            bonds=sum(bonds, na.rm=T),
            equity=sum(equity, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(month=zoo::rollapply(total,4,sum, na.rm=T, align="right", fill=NA)) %>% 
  mutate(iso3c="SSA") %>% 
  select(names(data.ssatotal)) %>% 
  filter(year(date)>=2022) %>% 
  mutate(ytd=cumsum(total))

lastobs<-data.ssareg %>% slice(n()) %>% pull(date) %>% format("%b %d")

plot.data<-data.ssareg %>% 
  select(date, bonds,equity) %>% 
  pivot_longer(-date,names_to = "type", values_to = "value")

g.cap<-ggplot(plot.data) +geom_bar(aes(date, value/1000, fill=type), stat="identity")+
  geom_hline(aes(yintercept=0), lty=2, col="grey30")+
  scale_fill_manual(values=c(REOblue, REOmedblue), guide="none")+
  annotate("text", x=as.Date("2022-2-25"), y=.3, label="Equity", col=REOmedblue, fontface="bold", size=3)+
  annotate("text", x=as.Date("2022-2-25"), y=-.3, label="Bonds", col=REOblue, fontface="bold", size=3)+
  scale_x_date(breaks=seq(as.Date("2022-2-1"), Sys.Date(), by="1 month")-1,
               date_labels = "%b")+
  theme(text=element_text(size=8))
g.cap+plot_annotation(title="Fund Flows to Sub-Saharan Africa, 2022",
                 subtitle = paste0("(USD billions per week, as of ",lastobs,")"),
                 caption="Source: EPFR and IMF staff calculations")&
  theme(plot.title=element_text(size=8, face="bold"),
        plot.subtitle=element_text(size=7),
        plot.caption = element_text(size=6, color="grey30"))
  
ggsave(filename = "capflows.png",  height = 2.4, width=3, dpi=600,units="in", device="png", bg = "transparent")
file.show("capflows.png")





table.data.ssa<-data.ssareg %>% 
  select(date,total) %>% 
  mutate(ytd=cumsum(total),
         month=zoo::rollapply(total,4, sum, na.rm=T, align="right", fill=NA)) %>% 
  rename(week=total) %>% 
  mutate(percent=100*week/abs(ytd))%>% 
  slice(n())

# Regional Weekly bond flows ETFs and Mutual Funds
regbond.list<-data.frame(codes=c(
  "CWBBEAC", "CWBBEEC","CWBBLAC","CWBBMSC"),
  database="EPFRBCF") %>% 
  mutate(index=paste0(codes,"@",database)) %>% 
  pull(index) %>% unlist()
raw.regbonds<-imf_datatools$get_haver_data(regbond.list)
raw.regbonds[is.na(raw.regbonds)] <- 0
data.regbonds<-raw.regbonds%>%
  mutate(date=as.Date(row.names(.))+6) %>%
  filter(date>=as.Date("2019-1-1"))%>% 
  select(date,(everything()))%>% 
  purrr::set_names(nm=c("date",
                        "Asia", "Europe", "Lat.Am", "Mid.East"
  )) %>% 
  pivot_longer(-date, names_to = "iso3c", values_to = "bonds")

# Regional Weekly equity flows ETFs and Mutual Funds
regequity.list<-data.frame(codes=c(
  "CWEBEAC", "CWEBEEC","CWEBLAC","CWEBMSC"),
  database="EPFRECF") %>% 
  mutate(index=paste0(codes,"@",database)) %>% 
  pull(index) %>% unlist()
raw.regequity<-imf_datatools$get_haver_data(regequity.list)
raw.regequity[is.na(raw.regequity)] <- 0
data.regequity<-raw.regequity%>%
  mutate(date=as.Date(row.names(.))+6) %>%
  filter(date>=as.Date("2019-1-1"))%>% 
  select(date,(everything()))%>% 
  purrr::set_names(nm=c("date",
                        "Asia", "Europe", "Lat.Am", "Mid.East"
  )) %>% 
  pivot_longer(-date, names_to = "iso3c", values_to = "equity")

data.regtotal <-data.regbonds %>% 
  full_join(data.regequity) %>% 
  arrange(iso3c, date) %>% 
  rowwise() %>% 
  mutate(total=sum(bonds,equity, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(iso3c) %>% 
  mutate(month=zoo::rollapply(total,4,sum, na.rm=T, align="right", fill=NA)) %>% 
  filter(year(date)>=2022) %>% 
  mutate(ytd=cumsum(total)) %>% 
  rbind(data.ssareg) %>% 
  ungroup()

table.data.reg<-data.regtotal %>% 
  group_by(iso3c) %>% 
  rename(week=total) %>% 
  mutate(percent=100*week/abs(ytd))%>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(iso3c,ytd,month, week, percent) %>% 
  mutate(week=round(week/1000,1),
         month=round(month/1000,1),
         ytd=round(ytd/1000,1),
         percent=round(percent,1))%>% 
  arrange(-percent) %>% 
  purrr::set_names(nm=c("Region", "YTD", "Past\nMonth", "Past\nWeek",
                        "Past Week as\n% of YTD"))
themefonts<-table.data.reg %>%  
  mutate(tabfont=ifelse(Region=="SSA", "bold", "plain")) %>% 
  pull(tabfont)
themecolors<-table.data.reg %>%  
  mutate(tabcol=ifelse(Region=="SSA", "firebrick", "grey20")) %>% 
  pull(tabcol)



library(gtable)
library(gridExtra)
t1 <- ttheme_default(base_size=8,
                     # widths = list(16,2,1,1,1),
                     core=list(
                       fg_params=list(fontface=themefonts,
                                      col=themecolors),
                       bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                                   length.out=nrow(table.data.reg))),
                                        alpha = rep(c(1,0.5), each=nrow(table.data.reg)))
                     ),
                     colhead=list(bg_params=list(fill=REOblue, alpha=.5)))
# t1<-ttheme_default()
gt <- tableGrob(table.data.reg, theme=t1, rows=NULL)
(wrap_elements(gt))+plot_annotation(title="Fund Flows to Emerging Markets, 2022",
                                    subtitle = paste0("(USD billions, week of ",lastobs,")"),
                                    caption="Source: EPFR and IMF staff calculations")&
                                    theme(plot.title=element_text(size=8, face="bold"),
                                          plot.subtitle=element_text(size=7),
                                          plot.caption = element_text(size=6, color="grey30"))
#for notes
ggsave(filename = "tab1.png",  height = 2.4, width=3, dpi=1000,units="in", device="png", bg = "transparent")
file.show("tab1.png")



# Spread dispersal --------------------------------------------------------

plot.data<-spark.data.s %>% 
  select(date, code, value) %>% 
  group_by(code) %>% 
  mutate(diff=value-lag(value,7),
         week=nth(value,-8),
         lag=lag(value,7))%>% 
  slice(n())%>% 
  left_join(plotlist) %>% 
  mutate(country=countrycode(iso3c, "iso3c", "country.name"))
plot.data.mean<-spark.data.s %>% 
  select(date, code, value) %>% 
  group_by(date)%>% 
  filter(code%in%c("JPSSGSAF","JPSSGCDI","JPSSGNIG","JPSSGGA ","JPSSGAOB","JPSSGSN ","JPSSGDGH","JPSSGZMB")) %>% 
  summarize(value=mean(value, na.rm=T)) %>% 
  mutate(diff=value-lag(value,7),
         week=nth(value,-8),
         lag=lag(value,7))%>% 
  slice(n())


lastobs=plot.data %>% slice(n()) %>% pull(date) %>% unique() %>% format("%b %d")

g.disp<-ggplot(plot.data)+
  geom_segment(aes(x=0, xend=diff, y=fct_reorder(country, diff), yend=country))+
  geom_point(aes(diff, country), size=3.5, col=REOred, shape="|", face="bold")+
  geom_vline(aes(xintercept = 0), col="grey30", lty=2, size=.2)+
  # geom_vline(aes(xintercept=plot.data.mean$diff), col="firebrick", size=.5, lty=2)+
  # geom_segment(aes(x=0.5, xend=plot.data.mean$diff-.5, y=12.5, yend=12.5), col="firebrick", size=.5, arrow = arrow(length=unit(.1, "inches")))+
  # annotate("text", x=plot.data.mean$diff+10, y=12.5, label=paste0("SSA Av.\n", round(plot.data.mean$diff, 0), " bps"), col="firebrick", hjust=0, size=2.5)+
  coord_cartesian(ylim=c(0,13.5))+
  theme(axis.text = element_text(size=7))+
  plot_annotation(title="Change in Eurobond Spreads over the Past Week",
                  subtitle = paste0("(Data as of ",lastobs,", 2022)"),
                  caption="Source: Bloomberg and IMF staff calculations")&
  theme(plot.title=element_text(size=8, face="bold"),
        plot.subtitle=element_text(size=7),
        plot.caption = element_text(size=6, color="grey30"))
(g.ribbon+labs(title="SSA Sovereign Spreads, 2019-22",
                    subtitle = paste0("(bps, simple average as of ",lastobs,")"),
                    caption="Source: Bloomberg and IMF staff calculations"))/
  (g.disp+labs(title="Change in Spreads over the Past Week",
              subtitle = paste0("(bps, as of ",lastobs,")"),
              caption="Source: Bloomberg and IMF staff calculations"))
ggsave(filename = "dispersal.png",  height = 5.5, width=6, dpi=1000,units="in", device="png", bg = "transparent")
file.show("dispersal.png") 
g.disp
ggsave(filename = "dispersal.png",  height = 2.7, width=5, dpi=1000,units="in", device="png", bg = "transparent")
file.show("dispersal.png") 
# Dollar index ------------------------------------------------------------

usd.raw<- bbextract("DXY Curncy", "2000-06-30", Sys.Date()) 

plot.usd<-usd.raw %>% 
  rename(index=2) %>% 
  fill(index)
lastobs=plot.usd %>% slice(n()) %>% pull(date) %>% unique() %>% format("%b %d")
g.usd<-ggplot(plot.usd %>% filter(date>=as.Date("2022-6-30")))+
  annotate("rect", ymin=-Inf, ymax=Inf, xmin=as.Date(Sys.Date())-7, xmax = as.Date(Sys.Date()), fill="grey30", alpha=.5)+
  geom_line(aes(date, index ), col="firebrick", size=1)+
  plot_annotation(title="US Dollar Index",
                  subtitle = paste0("(Data as of ",lastobs,", 2022)"),
                  caption="Source: Bloomberg and IMF staff calculations")&
  theme(plot.title=element_text(size=8, face="bold"),
        plot.subtitle=element_text(size=7),
        plot.caption = element_text(size=6, color="grey30"))
ggsave(filename = "dollar.png",  height = 2.5, width=5, dpi=1000,units="in", device="png", bg = "transparent")
file.show("dollar.png") 
g.disp+g.usd
ggsave(filename = "dispusd.png",  height = 2.7, width=5, dpi=1000,units="in", device="png", bg = "transparent")
file.show("dispusd.png") 
