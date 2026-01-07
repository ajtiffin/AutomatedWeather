
# rm(list=ls())
Sys.setenv(TZ="UTC")
library(ggthemes)
library(skimr)
library(lubridate)
library(scales)
library(countrycode)
library(readxl)
library(knitr)
library(kableExtra)
library(patchwork)
library(reticulate)
use_python("C:/ProgramData/Python3", required = TRUE)
library(ggforce)
library(fpp3)
library(ggdist)
library(tidyverse)

setwd("c:\\Projects\\AutomatedWeatherNote")

# REO-approved theme ----------------------------------

{fundblue<-"#004C97"
ssablue<-"#0065d2"
ssamedblue<-"#66C4EB"
ssapeach1<-"#fab58a"
ssapeach2<-"#ff8745"
ssapeach3<-"#fa5a00"
ssapeach4<-"#cc4700"
ssapeach5<-"#802b00"
ssagrey1<-"#b3b3b3"
ssagrey2<-"#949494"
ssagrey3<-"#707070"
ssagrey4<-"#404040"
ssagrey5<-"#171717"}

slidetheme<- theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    axis.line = element_line(color="grey20"),
    axis.ticks = element_line(color="grey20"),
    axis.ticks.length=unit(-0.10, "cm"),
    axis.text.y = element_text(margin=margin(0,10,0,0,"pt")),
    axis.text = element_text(size=8),
    plot.title=element_text(size=10, face="bold"),
    plot.subtitle=element_text(size=8),
    plot.caption = element_text(size=7, color="grey30", hjust=0),
    legend.text = element_text(size=7, color="grey30"),
    plot.title.position = "plot",
    plot.caption.position = "plot")
theme_set(slidetheme)
getwd()

CEMAC<-c("GAB", "CMR", "CAF", "TCD", "COG", "GNQ")
WAEMU<-c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO")

# IMF datatools helper functions----------------------------------------------------

imf_datatools <- import("imf_datatools")
imf_datatools$idata_utilities$PRIVATE <- TRUE
py_config()
db.list<-imf_datatools$idata_utilities$get_databases()
idata.basic<-function(var.list, country.list, database, start, end, frq){
  key.var<-paste0(var.list, collapse="+")
  key.ctry<-paste0(country.list, collapse="+")
  key.freq<-frq
  key<-paste0(key.ctry,".",key.var,".",key.freq)
  data<-imf_datatools$idata_utilities$get_idata_data(database, key=key, start=start, end=end, panel="COUNTRY") %>% 
    janitor::clean_names() %>% 
    rename(date=dates, iso3c=country) %>%
    mutate(date=as.Date(date)) %>% 
    select(date, everything())%>%
    pivot_longer(-c(date, iso3c), names_to = "code", values_to = "value") %>% 
    mutate(variable=str_sub(code,start=1, end=-3)) %>% 
    select(-code)%>%
    pivot_wider(names_from = variable, values_from = value) %>% 
    arrange(iso3c,date)%>%
    ungroup() %>% 
    as.tibble()
  print(key)
  return(data)
}
idata.reserves<-function(var.list, sector, country.list, database, start, end, frq){
  key.var<-paste0(var.list, collapse="+")
  key.sector<-paste0(sector, collapse="+")
  key.ctry<-paste0(country.list, collapse="+")
  key.freq<-frq
  key<-paste0(key.ctry,".",key.var,".",key.sector, ".",key.freq)
  data<-imf_datatools$idata_utilities$get_idata_data(database, key=key, start=start, end=end, panel="COUNTRY") %>% 
    janitor::clean_names() %>% 
    rename(date=dates, iso3c=country) %>%
    mutate(date=as.Date(date)) %>% 
    select(date, everything())%>%
    pivot_longer(-c(date, iso3c), names_to = "code", values_to = "value") %>% 
    mutate(variable=str_sub(code,start=1, end=-3)) %>% 
    select(-code)%>%
    pivot_wider(names_from = variable, values_from = value) %>% 
    arrange(iso3c,date)%>%
    ungroup() %>% 
    as.tibble()
  print(key)
  return(data)
}


bbiextract<-function(var.list, field, start, end, frq){
  key.var<-paste0(var.list, collapse="+")
  key.field<-paste(field, collapse="+")
  key.freq<-frq
  key<-paste0(key.var,".",key.field,".", key.freq)
  print(key)
  data<-imf_datatools$idata_utilities$get_idata_data("IMF.CSF:BBGDL", key=key, start=start, end=end)%>% 
    janitor::clean_names() %>% 
    mutate(date=as.Date(rownames(.)))%>% 
    select(date, everything())%>%
    pivot_longer(!date, names_to = "code", values_to = "value")%>%
    mutate(variable=sub("_.*", "", code) )%>% 
    select(-code)%>%
    pivot_wider(names_from=variable, values_from=value) %>% 
    ungroup() %>% 
    mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
    as.tibble()
  return(data)
}

# AFR-specific groups-------------------------------

weo.codes<-imf_datatools$ecos_sdmx_utilities$get_weo_country_codes() %>% janitor::clean_names()
ssa.list<-weo.codes %>%
  filter(sub_sahara_africa==1) %>% pull(iso_3_code)


# Spreads Data -----------------------------------------------------------


db='IMF.CSF:BBGDL'
bbseries<-imf_datatools$idata_utilities$get_dimension_values(db,"TICKER") %>% data.frame() %>% mutate(code=rownames(.))

# bbseries<-imf_datatools$idata_utilities$get_dimensions(db)


bbseries %>% 
  filter(str_detect(code, "INDEX"))%>%
  filter(str_detect(code, "JPEEBJSS")) %>%
  # head() %>% 
  kableExtra::kbl("rst")


series.list<-c('JPSSGAOB_INDEX', # AGO Yes
               "JPSSGDGH_INDEX", # GHA Yes
               "JPGCKESS_INDEX", #KEN Yes
               "JPSSGNIG_INDEX", #NGA Yes 
               "JPSSGSN_INDEX", #SEN/WAEMU Yes
               "JPSSGSAF_INDEX", #ZAF Yes
              #  "JPSSGTZB_INDEX", #TZA No
               "JPSSGZMB_INDEX", #ZMB Yes
               "JPSSGCDI_INDEX", #CIV Yes
               "JPEGETSS_INDEX", #ETH Yes
               "JPSSGGA_INDEX", #GAB Yes
               "JPSSGMZB_INDEX",#MOZ Yes
               "JPSSGNMB_INDEX",#NAM Yes
               "JPGCCMSS_INDEX",#CMR Yes 
               "JPEIGLSP_INDEX",#EMBI Yes
               "USGG10YR_INDEX"#USA Yes
               )


raw = bbiextract(series.list, "PX_LAST", "2014-01-1", as.character(Sys.Date()), "D") 
raw %>% tail()

data<-raw %>%  
  tidyr::fill(everything()) |> 
  filter(date!=as.Date(Sys.Date()))
names(raw)
plot.list<-data.frame(names(data)[2:length(names(data))],
iso3c=c("ETH", "EMBI", "CMR", "KEN", "AGO", "CIV", "GHA", "GAB", "MOZ", "NGA", "NAM", "ZAF", "SEN", "ZMB", "UST"))
names(plot.list)<-c("index", "iso3c")
data |> tail(10)
panel.lastobs<-format(max(data$date), "%b %d, %Y")
panel.data<-data %>%
  filter(date%in% c(max(date)-7, max(date)))|> 
  pivot_longer(2:15, names_to = "index", values_to = "value") %>% 
  left_join(plot.list)%>% 
  select(date, iso3c, value) %>% 
  pivot_wider(names_from = date, values_from = value)%>% 
  rename(then=2, now=3) %>% 
  mutate(diff=now-then)%>% 
  mutate(country=countrycode(iso3c, "iso3c", "country.name")) %>% 
  filter(iso3c!="EMBI") |> 
  mutate(abs=abs(diff)) |> 
  arrange(desc(abs)) |> 
  head(5)

spread.panel<-ggplot(panel.data)+
  geom_segment(aes(y=fct_reorder(country,diff), yend=country, x=0, xend = diff), color=fundblue)+
  geom_point(aes(x=diff, y=country), shape="|", size=4, col=fundblue)+
  geom_vline(aes(xintercept=0), lty=2, size=.5, col=ssagrey4)+
  labs(title = str_wrap("Change in Sovereign Spreads in the Past Week",30),
       subtitle = paste0("(Top five, bps, as of ", panel.lastobs,")"))
spread.panel
# Commodity prices --------------------------------------------------------
bbseries %>% 
  filter(str_detect(code, "_COMDTY"))%>%
  # filter(str_detect(code, "C1"))%>%
  filter(str_detect(Name, "maize")) %>%
  # head() %>% 
  kableExtra::kbl("rst")
comm.list<-c("GC1_COMDTY", # gold
             # "NG1_COMDTY",
             # "SPGSWHP_COMDTY", #wheat
             # "RR1_COMDTY",
             # "GC1_COMDTY",
             "CL1_COMDTY", #oil
             # "PA1_COMDTY",
             "LOCADS03_COMDTY", #LME copper
             "HG1_COMDTY", 
             # "C 1_COMDTY",
             "SF1_COMDTY", #coffee
             "CC1_COMDTY") #cocoa
             # "S 1_COMDTY",
             # "SB1_COMDTY")
raw.comm = bbiextract(comm.list, "PX_LAST", "2014-01-1", as.character(Sys.Date()), "D") 
raw.comm %>% tail()

comm.data<-raw.comm %>% 
  fill(everything()) %>% 
  filter(date!=Sys.Date()) %>% 
  rename(date=1, Gold=`gc1`, Copper=`locads03`, Coffee=`sf1`, Cocoa=`cc1`, Oil=`cl1`) %>%
  select(date, Gold, Copper, Coffee, Cocoa, Oil)
# , Wheat=`W 1 Comd`, Copper=`HG1 Comd`,
#          Corn=`C 1 Comd`, Gas=`NG1 Comd`, Gold=`GC1 Comd`, Rice=`RR1 Comd`,
#          Soybean=`S 1 Comd`, Sugar=`SB1 Comd`, Coffee=`DF1 Comd`, Cocoa=`QC1 Comd`)%>% 
  # select(date, Oil, Gas, Soybean, Wheat, Rice, Corn, Sugar, Gold, Copper)

comm.lastobs<-max(comm.data$date)%>% format("%b %d, %Y")
comm.data %>% tail() %>% kbl("rst")

panel.data.comm<-comm.data |> 
  filter(date%in%c(max(date)-7,max(date))) |>
  pivot_longer(2:6, names_to="comm", values_to = "value") |> 
  pivot_wider(names_from = date, values_from = value)%>% 
  rename(then=2, now=3) %>% 
  mutate(diff=100*(now/then)-100)%>% 
  mutate(abs=abs(diff)) |> 
  arrange(desc(abs)) |> 
  head(5)


comm.panel<-ggplot(panel.data.comm)+
  geom_segment(aes(y=fct_reorder(comm,diff), yend=comm, x=0, xend = diff), color=fundblue)+
  geom_point(aes(x=diff, y=comm), shape="|", size=4, col=fundblue)+
  geom_vline(aes(xintercept=0), lty=2, size=.5, col=ssagrey4)+
  labs(title = str_wrap("Commodity-Price Movements in the Past Week",30),
       subtitle = paste0("(Percent, as of ", comm.lastobs,")"))
comm.panel

# Spreads panel  -----------------------------------------------------------
data.s<-data |> 
  select(-jpegetss, -jpssgzmb)
start="2025-1-1"
plot.data.s<-data.s %>% 
    select(-c(jpeiglsp, usgg10yr)) %>% 
    filter(date>=as.Date(start)) %>% 
    pivot_longer(-date,names_to = "index", values_to = "value") %>% 
    arrange(index,date) %>% 
    group_by(index) %>% 
    fill(value) %>% 
    mutate(base=sum(ifelse(date==as.Date(start),value,0))) %>% 
    mutate(change=value-base) %>% 
    mutate(max=max(change),
           min=min(change),
           latest=last(value),
           week=nth(value, -5))%>% 
    slice(n()) %>% 
    ungroup() %>% 
    arrange(latest) %>% 
    mutate(labelcol=case_when(
      value<=700~"blue",
      value>700&value<=1000~"grey",
      value>1000~"red",
      TRUE~NA_character_
    )) %>% 
    left_join(plot.list, by="index") %>% 
    mutate(country=countrycode(iso3c, "iso3c", "country.name"))
spark.data.s<-data.s %>% 
    select(-c(usgg10yr,jpeiglsp)) %>% 
    filter(date>=as.Date(start)) %>% 
    pivot_longer(-date,names_to = "index", values_to = "value") %>% 
    arrange(index,date) %>% 
    group_by(index) %>% 
    fill(value) %>% 
    mutate(base=sum(ifelse(date==as.Date(start),value,0))) %>% 
    mutate(change=value-base) %>% 
    mutate(max=max(change),
           min=min(change),
           latest=last(value),
           week=nth(change, -5)) %>% 
    mutate(pos=ifelse(change>=0,change,0),
           neg=ifelse(change<0,change,0))
last.date.s<-plot.data.s %>% pull(date) %>% max() %>% format("%d %b, %Y")
p.1<-ggplot(plot.data.s)+geom_segment(aes(x=base, xend=latest, 
  y=fct_reorder(country, latest), yend=country, col=labelcol), size=1, alpha=.7)+
    # geom_point(aes(max,country), size=8, col="grey70", shape="|")+
    # geom_point(aes(min,country), size=8, col="grey70", shape="|")+
    geom_point(aes(latest,country, col=labelcol), size=4, shape="|")+
  scale_x_log10()+
    geom_point(aes(week,country), size=3, col="grey30", shape="|")+
    # geom_vline(aes(xintercept = 0), lty=2, col="grey30")+
    labs(title = paste0("(Basis points, change since 1 Jan 2025, as of ",last.date.s,". Log Scale)"))+
  scale_color_manual(values=c(ssablue, "grey30", "firebrick"), guide=F)+
    theme(plot.title.position = "plot",
          panel.grid.major.x = element_line(color="grey90", linetype=2),
          panel.grid.major.y=element_line(color="grey90", linetype=2, size=.5),
          plot.title = element_text(face="plain"))
p.1
p.2<- ggplot(spark.data.s)+
  geom_ribbon(aes(date, ymin=0, ymax=pos), fill="firebrick", alpha=.5)+
  geom_ribbon(aes(date, ymax=0, ymin=neg), fill=ssablue, alpha=.5)+
  geom_point(aes(date, change), col="firebrick", size=1, data = . %>% slice(n()))+
  # geom_hline(aes(yintercept=max), col="grey80", size=.5)+
  # geom_hline(aes(yintercept=min), col="grey80", size=.5)+
  facet_grid(fct_reorder(index, -latest)~., scale="free")+
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
    scale_color_manual(values=c(ssablue, "grey30", "firebrick"), guide=F)+
    geom_text(aes(1,fct_reorder(country,latest), label=format(round(value, 0), big.mark=",")), col="white", fontface="bold", size=3)+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank())+
    theme_map()+
  coord_cartesian(xlim = c(0,2))+
    labs(title = "(Latest Level)")
p.4<-ggplot()+geom_segment(aes(x=.5, xend=3.5, y=1, yend=1), size=1, col=ssablue)+
  geom_point(aes(3.5,1), size=4, shape="|", col=ssablue)+
  geom_point(aes(1.5,1), size=3, col="grey30", shape="|")+
  # geom_point(aes(0,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(5,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(.5,1), size=3, col="grey30", shape="|")+
  # annotate("text", x=0,y=1, label="Range\nMinimum", vjust=1.6, hjust=0,col="grey50", size=3)+
  annotate("text", x=.5,y=1, label="1 Jan 2022", vjust=3.2, hjust=0.5,col=ssablue, size=3)+
  # annotate("text", x=5,y=1, label="Range\nMaximum", vjust=1.6, hjust=1, col="grey50", size=3)+
  annotate("text", x=3.5,y=1, label="Latest", vjust=3.2, col=ssablue, size=3)+
  annotate("text", x=1.5,y=1, label="Week\nago", vjust=1.5, col="grey30", size=3)+
  theme_void()
p.5<-ggplot()+
  geom_segment(aes(x=0, xend=3, y=1, yend=1), size=6, col=ssablue)+
  geom_segment(aes(x=4, xend=7, y=1, yend=1), size=6, col="grey30")+
  geom_segment(aes(x=8, xend=11, y=1, yend=1), size=6, col="firebrick")+
  annotate("text", x=1.5,y=1, label="Performing", col="white", fontface="bold", size=3)+
  annotate("text", x=5.5,y=1, label="Stressed", col="white", fontface="bold", size=3)+
  annotate("text", x=9.5,y=1, label="Distress", col="white", fontface="bold", size=3)+
  theme_void()
(panel1<-(p.1+p.2+p.3+plot_layout(widths = c(7,1.5,1.5)))/(p.4+p.5)+plot_layout(heights = c(4.5,1.5)))
ggsave(filename = "panela.png",  height = 4, width=9, dpi=600,units="in", device="png", bg = "transparent")

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
  filter(year(date)>=2025) %>% 
  mutate(ytd=cumsum(total))

epfr.lastobs<-data.ssareg %>% slice(n()) %>% pull(date) %>% format("%b %d")

plot.data<-data.ssareg %>% 
  select(date, bonds,equity) %>% 
  pivot_longer(-date,names_to = "type", values_to = "value")

g.cap<-ggplot(plot.data) +geom_bar(aes(date, value/1000, fill=type), stat="identity")+
  geom_hline(aes(yintercept=0), lty=2, col="grey30")+
  scale_fill_manual(values=c(ssablue, ssamedblue), guide="none")+
  annotate("text", x=as.Date("2025-2-25"), y=.3, label="Equity", col=ssamedblue, fontface="bold", size=7/.pt)+
  annotate("text", x=as.Date("2025-2-25"), y=-.3, label="Bonds", col=ssablue, fontface="bold", size=7/.pt)+
  scale_x_date(breaks=seq(as.Date("2025-2-1"), Sys.Date(), by="3 months")-1,
               date_labels = "%b")+
  theme(text=element_text(size=7))
g.cap+plot_annotation(title=str_wrap("Fund Flows to Sub-Saharan Africa, 2025",30),
                 subtitle = paste0("(USD billions per week, as of week ending ",epfr.lastobs,")"),
                 caption="Source: EPFR and IMF staff calculations")&
  theme(plot.title=element_text(size=10, face="bold"),
        plot.subtitle=element_text(size=7),
        plot.caption = element_text(size=7, color="grey30"))
  
ggsave(filename = "capflows.png",  height = 3, width=2.5, dpi=600,units="in", device="png", bg = "transparent")

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
  filter(year(date)>=2025) %>% 
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
t1 <- ttheme_default(base_size=7,
                     # widths = list(16,2,1,1,1),
                     core=list(
                       fg_params=list(fontface=themefonts,
                                      col=themecolors),
                       bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                                   length.out=nrow(table.data.reg))),
                                        alpha = rep(c(1,0.5), each=nrow(table.data.reg)))
                     ),
                     colhead=list(bg_params=list(fill=ssablue, alpha=.5)))
# t1<-ttheme_default()
gt <- tableGrob(table.data.reg, theme=t1, rows=NULL)
(wrap_elements(gt))+plot_annotation(title=str_wrap("Fund Flows to Emerging Markets, 2025",30),
                                    subtitle = paste0("(USD billions, week ending ",epfr.lastobs,")"),
                                    caption="Source: EPFR and IMF staff calculations")&
                                    theme(plot.title=element_text(size=10, face="bold"),
                                          plot.subtitle=element_text(size=7),
                                          plot.caption = element_text(size=7, color="grey30"))
#for notes
ggsave(filename = "tab1.png",  height = 3, width=2.5, dpi=1000,units="in", device="png", bg = "transparent")

# Exchange Rate panel -----------------------------------------------------
db<-"IMF.STA:IL"
imf_datatools$idata_utilities$get_dimensions(db)
bbseries<-imf_datatools$idata_utilities$get_dimension_values(db,"UNIT") %>% data.frame() %>% mutate(code=rownames(.))

ifs.codes<-c("RXF11_REVS")
ifs.sector<-c("USD") # mon auth and CG
ifs<-idata.reserves(ifs.codes, ifs.sector, ssa.list, db, "2024", "2026", "M")

imports<-idata.basic("BMGS_BP6", ssa.list, "IMF.RES.WEO:WEO_LIVE", "2026", "2026", "A")


plot.list<-c("AGO", "BWA", "BDI", "COD", "ERI", "ETH", "GMB", "GHA", "GIN",
               "KEN", "LBR", "MDG", "MWI", "MUS", "MOZ", "NGA", "RWA", "SYC",
               "SLE", "ZAF", "SSD", "TZA", "UGA", "ZMB", "CIV", "GAB")
er.plotlist<-data.frame(index=plot.list) |>
  mutate(country=countrycode(index, "iso3c", "country.name")) |> 
  mutate(country=case_when(index=="COD"~"Dem.Rep. Congo",
index=="CIV"~"WAEMU",
index=="GAB"~"CEMAC",
TRUE~country
))
c(WAEMU, CEMAC)
erbbg.list<-c("USDAOA_CURNCY",#	Angola
"USDXOF_CURNCY",#	Cote d'Ivoire
"USDETB_CURNCY",#	Ethiopia
"USDXAF_CURNCY",#	Gabon
"USDGHS_CURNCY",#	Ghana
"USDKES_CURNCY",#	Kenya
"USDMUR_CURNCY",#	Mauritius
"USDMZN_CURNCY",#	Mozambique
"USDNGN_CURNCY",#	Nigeria
"RWF BNRW_CURNCY",#	Rwanda
# "USDXOF_CURNCY",#	Senegal
"USDZAR_CURNCY",#	South Africa
"USDTZS_CURNCY",#	Tanzania
"USDUGX_CURNCY",#	Uganda
"USDZMW_CURNCY",#	Zambia
"USDBWP_CURNCY",#	Botswana
"USDBIF_CURNCY",#	Burundi
"USDCDF_CURNCY",#	Congo, Dem. Rep.
# "USDERN_CURNCY",#	Eritrea
"USDGMD_CURNCY",#	Gambia
"USDGNF_CURNCY",#	Guinea
"USDLRD_CURNCY",#	Liberia
"USDMGA_CURNCY",#	Madagascar
"USDMWK_CURNCY",#	Malawi
"USDSCR_CURNCY",#	Seychelles
"USDSLL_CURNCY")#	Sierra Leone
# "USDSSP_CURNCY")#	South Sudan

raw.er= bbiextract(erbbg.list, "PX_LAST", "2014-01-1", as.character(Sys.Date()), "D") 
er.data<-raw.er%>% 
  fill(everything()) %>% 
  filter(date!=as.Date(Sys.Date()))

names(er.data)
names<-c( "AGO", "BDI", "BWA", "COD", "ETH",
         "GHA", "GMB", "GIN", "KEN", "LBR",
         "MDG", "MUS", "MWI", "MOZ", "NGA",
         "SYC", "SLE", "TZA", "UGA", "GAB",
         "CIV", "ZAF", "ZMB")
countrycode(names, "iso3c", "country.name")
names(er.data)<-c("date", names)

start<-as.Date("2025-1-1")

er.plot<-er.data %>% 
  filter(date>=as.Date(start)) %>% 
  filter(date!=as.Date(Sys.Date())) %>% 
  pivot_longer(-date, names_to = "index", values_to = "value")%>% 
  left_join(er.plotlist, by="index")%>% 
  # select(-index) %>% 
  arrange(country, date) %>% 
  group_by(country)%>%
  mutate(base=sum(ifelse(date==as.Date(start),value,0)))%>% 
  mutate(change=100*base/value-100) %>% 
  mutate(max=max(change),
         min=min(change),
         latest=last(change),
         week=nth(change, -5))%>% 
  slice(n())%>% 
  filter(!country%in%c("Cote d'Ivoire", "Gabon")) %>%
  mutate(country=ifelse(country=="Senegal","WAEMU/CEMAC", country)) %>% 
  mutate(country=ifelse(country=="South Africa","South Africa", country))
er.plot$country %>% unique()%>% countrycode("country.name", "iso3c")->templist
setdiff(ssa.list, c(WAEMU, CEMAC, templist))
spark.data.er<-er.data %>% 
  filter(date>=as.Date(start)) %>% 
  filter(date!=as.Date(Sys.Date())) %>% 
  pivot_longer(-date,names_to = "index", values_to = "value") %>% 
  left_join(er.plotlist) %>% 
  arrange(country,date) %>% 
  group_by(country) %>% 
  fill(value) %>% 
  mutate(base=sum(ifelse(date==as.Date(start),value,0)))%>% 
  mutate(change=100*base/value-100) %>% 
  mutate(max=max(change),
         min=min(change),
         latest=last(change),
         week=nth(change, -5))%>% 
  mutate(pos=ifelse(change>=0,change,0),
         neg=ifelse(change<0,change,0))%>% 
  filter(!country%in%c("Cote d'Ivoire", "Gabon")) %>%
  mutate(country=ifelse(country=="Senegal","WAEMU/CEMAC", country))
er.lastobs<-er.plot %>% pull(date) %>% max() %>% format("%d %b, %Y")

p.6<-ggplot(er.plot)+geom_segment(aes(x=0, xend=latest, y=fct_reorder(country, latest), yend=country), size=1, col=ssablue, alpha=.7)+
    geom_point(aes(latest,country), size=4, shape="|", col=ssablue)+
    geom_point(aes(week,country), size=3, col="firebrick", shape="|")+
    geom_vline(aes(xintercept = 0), lty=2, col="grey30")+
    labs(title = paste0("(versus USD, percent change since 1 Jan 2025, as of ",er.lastobs,")"))+
    theme(plot.title.position = "plot",
          panel.grid.major.x = element_line(color="grey90", linetype=2),
          panel.grid.major.y = element_line(color="grey90", linetype=2, size=.5),
          plot.title = element_text(face="plain"))
p.7<- ggplot(spark.data.er)+
  geom_ribbon(aes(date, ymin=0, ymax=pos), fill=ssablue, alpha=.5)+
  geom_ribbon(aes(date, ymax=0, ymin=neg), fill="firebrick", alpha=.5)+
  geom_point(aes(date, change), col="firebrick", size=1, data = . %>% slice(n()))+
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
  select(c(iso3c, bmgs_bp6))

ifs.data<-ifs %>% 
  group_by(iso3c) %>% 
  slice(n()) %>% 
  rename(reserves=3) %>% 
  mutate(date=date+months(1)-1) %>% 
  select(iso3c, date, reserves) %>% 
  left_join(import.join, by="iso3c") %>% 
  # mutate(cover=round(reserves/BMGS_BP6*100/12,1))
  mutate(cover=round(reserves/(bmgs_bp6/12),1))

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
  scale_color_manual(values=c(ssablue, "grey30", "firebrick"), guide=F, na.value = "transparent")+
  geom_text(aes(1,fct_reorder(country,latest), label=format(round(cover, 1), big.mark=",")), col="white", fontface="bold", size=3)+
  geom_text(aes(2.3,fct_reorder(country,latest), label=format(date, "%b %y")), fontface="bold", size=3, col="grey30")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank())+
  coord_cartesian(xlim=c(0,3))+
  theme_map()+
  labs(title = "(Reserves)")
p.9<-ggplot()+geom_segment(aes(x=.5, xend=3.5, y=1, yend=1), size=1, col=ssablue)+
  geom_point(aes(3.5,1), size=5, shape="|", col=ssablue)+
  geom_point(aes(1.5,1), size=4, col="firebrick", shape="|")+
  # geom_point(aes(0,1), size=6, col="grey70", shape="|")+
  # geom_point(aes(5,1), size=6, col="grey70", shape="|")+
  geom_point(aes(.5,1), size=3, col="grey30", shape="|")+
  # annotate("text", x=0,y=1, label="Range\nMinimum", vjust=1.6, hjust=0,col="grey50", size=3)+
  annotate("text", x=.5,y=1, label="0", vjust=3.2, hjust=0.5,col="grey30", size=3)+
  # annotate("text", x=5,y=1, label="Range\nMaximum", vjust=1.6, hjust=1, col="grey50", size=3)+
  annotate("text", x=3.5,y=1, label="Latest", vjust=3.2, col=ssablue, size=3)+
  annotate("text", x=1.5,y=1, label="Week\nago", vjust=1.5, col="firebrick", size=3)+
  theme_void()

p.10<-ggplot()+
  geom_segment(aes(x=0, xend=3, y=1, yend=1), size=6, col=ssablue)+
  geom_segment(aes(x=4, xend=7, y=1, yend=1), size=6, col="grey30")+
  geom_segment(aes(x=8, xend=11, y=1, yend=1), size=6, col="firebrick")+
  annotate("text", x=1.5,y=1, label=">5 mths", col="white", fontface="bold", size=3)+
  annotate("text", x=5.5,y=1, label="3 - 5 mths", col="white", fontface="bold", size=3)+
  annotate("text", x=9.5,y=1, label="< 3 mths", col="white", fontface="bold", size=3)+
  theme_void()

(panel2<-((p.6+p.7+p.8+plot_layout(widths = c(7,1.5,1.5)))/(p.9+p.10)+plot_layout(heights = c(6.5,1.5))))

ggsave(filename = "panelb.png",  height = 6.2, width=9, dpi=600,units="in", device="png", bg = "transparent")

er.panel.data<-spark.data.er |> 
  select(date, index, country, value) |> 
  fill(everything()) |> 
  group_by(index) |> 
  filter(date%in%c(max(date)-7,max(date))) |> 
  pivot_wider(names_from = date, values_from = value) |> 
  rename(then=3, now=4) |> 
  mutate(change=(100*then/now)-100) |> 
  mutate(abs=abs(change)) |> 
  arrange(desc(abs)) |> 
  head(5)
er.panel<-ggplot(er.panel.data)+
  geom_segment(aes(x=0, xend=change, y=fct_reorder(country, change), yend=fct_reorder(country, change)), col=fundblue)+
  geom_point(aes(x=change, y=country), shape="|", size=4, col=fundblue)+
  geom_vline(aes(xintercept=0), lty=2, size=.5, col=ssagrey4)+
  labs(title = str_wrap("Currency Movements in the Past Week",30),
       subtitle = paste0("(Top five, percent, as of ", er.lastobs,")"))
er.lastobs
er.panel

spread.panel+er.panel+comm.panel+plot_annotation(caption = "Source: Bloomberg LLC.")
ggsave("weeklycharts.png", height = 2.4, width=8, dpi=1000,units="in", device="png", bg = "transparent")
library(jsonlite)
spread.context<-spread.panel$data |> toJSON()
er.context<-er.panel$data |> toJSON()
com.context<-comm.panel$data |> toJSON()
save(spread.context, er.context, com.context, file="contextJSONs.rdta")
