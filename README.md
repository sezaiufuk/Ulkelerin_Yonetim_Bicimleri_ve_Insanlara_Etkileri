
# Ülkelerin Yönetim Biçimleri ve İnsanlara Etkileri

Veri Görselleştirme Dersi Final Projesi,
Sezai Ufuk Oral


## R Kodları

#### Kütüphanelerin Eklenmesi

```
library("readr")
library("ggplot2")
library("dplyr")
library("gridExtra")
library("ggcorrplot")
library("treemapify")
library("colorspace")
library("data.table")
library("reshape2")

my_palette <- function(quantity){
  return (colorspace::sequential_hcl(quantity,palette="BluGrn"))
}
```

#### Tema Ayarı

```
my_theme <- theme(axis.text = element_text(face="bold",
size=11,color = "black"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.background = element_blank())
```

#### Veri Düzenlemeleri

```
regime <- read_csv("regime.csv")
trust <- read_csv("trust.csv")
gdp <- read_csv("happiness_gdp.csv")

colnames(gdp)[1] <- "Country"

colnames(regime)[1] <- "Country"

regime <- regime %>% group_by(Year) %>% filter(Year==2020)

regime$regime_row_owid[regime$regime_row_owid == 0]
<- "Kapalı Otokrasi"
regime$regime_row_owid[regime$regime_row_owid == 1]
<- "Seçimli Otokrasi"
regime$regime_row_owid[regime$regime_row_owid == 2]
<- "Seçimli Demokrasi"
regime$regime_row_owid[regime$regime_row_owid == 3]
<- "Liberal Demokrasi"

country_names_tr <- read_csv("country_names_tr.csv")

country_names_tr <- select(country_names_tr,c(2,3))
colnames(country_names_tr) <- c("Country","Country_tr")

regime <- merge(country_names_tr,regime,by="Country")
trust <- merge(country_names_tr,trust,by="Country")

colnames(regime)[5] <- "Regime"

colnames(trust) <- c("Country","Country_tr","Mahalle","Devlet",
"Bilim İnsanları","Gazeteciler","Doktorlar","Hayır Kurumları",
"Geleneksel Tıp")
```

#### Devlete En Fazla ve En Az Güvenen Ülkeler

```
highest_government_trust <- trust %>% group_by(Devlet)
%>% arrange(desc(Devlet)) %>% head(3)
lowest_government_trust <- trust %>% group_by(Devlet)
%>% arrange(Devlet) %>% head(3)

highest_government_trust_plot <-ggplot(highest_government_trust,
aes(x=Country_tr,y=Devlet)) + geom_col(fill= my_palette(3))
highest_government_trust_plot + geom_text(size=8,aes(
              label = paste(Devlet,"%"),),
            hjust = 0.45,vjust =4,color="white") + my_theme

lowest_government_trust_plot <-ggplot(lowest_government_trust,
aes(x=Country_tr,y=Devlet)) + geom_col(fill= rev(my_palette(3)))
lowest_government_trust_plot + geom_text(size=8,aes(
              label = paste(Devlet,"%"),),
            hjust = 0.45,vjust =4,color="white") + my_theme
```

#### Güven Korelasyon Matrisi

```
population_trusts <- trust[,3:9]
population_trusts <- population_trusts %>% na.omit(Government)

cor_population_trusts <- cor(population_trusts)
corrp.mat <- cor_pmat(cor_population_trusts)


ggcorrplot(cor_population_trusts, method ="square",
type="lower",lab = TRUE,p.mat=corrp.mat,insig="blank",
colors = my_palette(3),outline.color = my_palette(3)[2],
show.legend = FALSE,hc.order = TRUE)
```

#### Güven Yüzdelikleri Ağaç Haritası

```
regime_trusts <- merge(regime,trust,by="Country")
regime_trusts <- select(regime_trusts,-c(2,6))

regime_trust_sum <- regime_trusts %>% group_by(Regime)
%>% summarise_at(c("Mahalle","Devlet","Bilim İnsanları",
"Gazeteciler","Doktorlar","Hayır Kurumları","Geleneksel Tıp"),
mean, na.rm = TRUE)

regime_trust_sum[2:8] <- round(regime_trust_sum[2:8],1)

regime_trust_sum_r <- melt(regime_trust_sum)

ggplot(regime_trust_sum_r, aes(area = value , fill = variable,
subgroup = Regime, label=paste(variable,"\n",value,"%"))) +
  geom_treemap() + geom_treemap_text(colour = "white",
  fontface="bold",
    place = "centre",
    size = 11) +
    scale_fill_manual(values = my_palette(8))+
    theme(legend.position = "none") +
    geom_treemap_subgroup_border(colour =
    "white",size = 5) +
        geom_treemap_subgroup_text(place =
        "centre", grow = TRUE,reflow = TRUE,
                alpha = 0.4, colour = "white",
                fontface = "italic")
```

#### Yönetim Şekline Göre Ömür Beklentisi İçin Ridge Plot

```
library("ggridges")
happiness_gdp_regime <- merge(regime,gdp[,c(1,7,9)],
by="Country")
happiness_gdp_regime <- select(happiness_gdp_regime,c(5,6,7))

colnames(happiness_gdp_regime)[2] <- "GDP"
colnames(happiness_gdp_regime)[3] <- "Healthy_Life_Expectation"

ggplot(happiness_gdp_regime, aes(x =
round(Healthy_Life_Expectation,1),
y = Regime,fill=Regime)) +
geom_density_ridges2(scale=2,alpha=0.75,
quantile_lines = TRUE,
jittered_points = TRUE,color=my_palette(1)[1],
quantiles =
c(0.025, 0.975)) +
scale_fill_manual(values=my_palette(4)) +
theme(
  axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(1,0,1,0), "cm"),
        panel.background = element_blank(),
        legend.position = "none"
) + scale_x_continuous(limits = c(38, 84),
breaks = seq(38, 84, 2))
```

### Kaynaklar
https://ourworldindata.org/grapher/political-regime?tab=chart
https://www.kaggle.com/datasets/elmoallistair/global-trust-rate?select=global_trust-rate.csv
https://www.kaggle.com/datasets/mathurinache/world-happiness-report