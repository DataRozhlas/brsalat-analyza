######################################################################
###                                                                ###
### (c) 2022 | Hynek Cígler                                        ###
### kontakt: cigler@fss.muni.cz                                    ###
### Instutit pro psychologický výzum (INPSY)                       ###
### Fakulta sociálních studií, Masarykova Universita               ###
###                                                                ###
### Suroviny pro Bramborový salát                                  ###
### Bradley-Terry model používaných surovin                        ###
### Analýza dat pro iRozhlas.cz (Petr Kočí)                        ###
###                                                                ###
######### Wed Dec 21 17:06:24 2022 ###################################

## Tento skript analýzuje data ze "souboje surovin", která probíhala na serveru 
##    iRozhlas.cz na https://data.irozhlas.cz/salat-or-not/ před Vánoci 2022. 
## Čtenáři měli za úkol postupně vybírat mezi dvojicí surovin, které byly náhodně losovány
##    ze souboru celkem 43 potenciálních možných surovin. 
## Tato data jsou vhodná pro analýzu s pomocí tzv. Bradleyho-Terryho modelu (BT), viz např.
##    https://en.wikipedia.org/wiki/Bradley%E2%80%93Terry_model . Model byl konstruován ručně
##    s využitím generalizovaného (smíšeného) lineárního modelu pomocí funkcí glm a lme4::glmer
##    v prostředí R. Informace o verzích R a balíčků jsou dostupné na konci skriptu.
## Pořadí prezentovaných modelů neodpovídá pořadí analýz.

# Load data and packages --------------------------------------------------

library(lme4)
library(psych)
library(lattice)

dat <- read.csv("geosalat.csv", fileEncoding  = "utf-8") ## data
codebook <- read.csv("codebook.csv", fileEncoding  = "utf-8") ## identifikace surovin

# Deskriptivy -------------------------------------------------------------

head(dat)
nrow(dat)
barplot(table(dat$loserID))
barplot(table(dat$winnerID))
plot(cbind(table(dat$winnerID), table(dat$loserID)), xlab = "winner", ylab = "loser") ## linearita výher a proher je důležitým předpokladem BT modelu.
cor(cbind(table(dat$winnerID), table(dat$loserID)))
table(dat$country)
table(dat$regionName[dat$country == "Czechia"])

winners <- table(dat$winnerID)
names(winners) <- codebook$item

windows(10, 15)
barplot(sort(winners), horiz = T, las =1, cex.names = .5, 
        main = "Absolutní počet výher napříč surovinami")


# Data management ---------------------------------------------------------

## přidání názvů surovin přímo mezi proměnné
dat$loser <- dat$winner <- NA
for (i in 0:42) {
  dat$loser[dat$loserID == i] <- codebook$item[i + 1]
  dat$winner[dat$winnerID == i] <- codebook$item[i + 1]
}

## nachystání širokých dat - co surovina, to jeden sloupec
datbt <- matrix(0, nrow = nrow(dat), ncol = nrow(codebook), dimnames = list(NULL, codebook$item))
datbt <- cbind(dat, datbt)
datbt <- as.data.frame(datbt)

set.seed(45765)
datbt$response <- rbinom(n = nrow(dat), size = 1, prob = .5) ## nachystání binární odpovědi s informací o tom, která surovina vyhrála match

names(datbt)


## Nachystání dat pro BT model ---------------------------------------------------------------
## Imputace hodnot pro prediktory. Příkladem budiž match suroviny A vs. B. 
## BT model modeluje logartimus šance na výběr suroviny A jako log[P(A>B)/P(B>A)] = A - B
## A, B jsou modelem odhadované parametry surovin, tedy jejich "vhodnost".
## Aby R mohlo model odhadnout, rovnice je rozšířena na log[P(A>B)/P(B>A)] = a*A + b*B , kde
## a, b jsou hodnoty prediktorů, které v níže uvedeném skriptu imputujeme. 
## Jeden má vždy hodnotu 1, druhý -1, což vede k rozdílu z první rovnice.
## Hodnotu závislé binární proměnné jsme vygenerovali v dřívější části skriptu.
## Řekněmě, že hodnota této binární proměnné je 1.
## Vybral-li čtenář surovinu A, je hodnota prediktorů a=1, b=-1
## Vybral-li čtenář surovinu B, je hodnota prediktorů a=-1, b=1
## Je-li hodnota náhodně genrované binární proměnné 0, pak poslední dva kroky jsou právě opačné. 
## Ostatní prediktory mají vždy hodnotu 0. Kladná, resp. záporná hodnota má za následek, že se...
## ... odhadnutá "vhodnost" suroviny přičítá či odčítá

for (i in 1:43) {
  datbt[(datbt$response == 1 & datbt$winnerID == i-1) | (datbt$response == 0 & datbt$loserID == i-1), 10+i] <- 1
  datbt[(datbt$response == 0 & datbt$winnerID == i-1) | (datbt$response == 1 & datbt$loserID == i-1), 10+i] <- -1
}

head(datbt)

## příprava kraje + zahraničí
datbt$kraj <- datbt$regionName
datbt$kraj[datbt$country != "Czechia"] <- "Zahraničí"
datbt$kraj <- as.factor(datbt$kraj)
datbt$kraj <- relevel(datbt$kraj, ref = "Hlavní město Praha") ## Nastavení referenčního kraje na Prahu


# Odhad Bradley-Terry modelu ------------------------------------------------------------------

## BT model bez random efektů -----------------------------------------------------
## Tedy bez rozdílu mezi kraji
## Suroviny v následujícím syntaxu chybí, jsou totiž "referenční kategorií".
## Jejich zařazení by vedlo k singulární matici prediktorů (jedna ze surovin lze vždy z ostatních dopočítat)
## Z toho důvodu jsou brambory vyřazeny, a jejich "hodnota" je tedy referenčním bodem 0.
## Parametry ostatních surovin tak mají "věcný" význam ve srovnání právě s bramborami.
## Čím je hodnota koeficientu vyšší a bližší jedné, tím surovina do salátu patří stejně, jako brambory.
## pomocí exp(B) lze parametr každé suroviny převést na poměr šancí...
## Intercept není odhadován, protože byl z povahy dat (náhodně generovaná odpověď) právě nula.
## ... tedy kolikrát je surovina méně vhodná do salátu ve srovnání s bramborami.

bt_mod3 <- glm(response ~ 0 + `Avokádo` + `Balkánský sýr` + `Celer` + `Čerstvá pažitka` + `Červená řepa` + `Česnek` + 
                 `Cherry rajčata` + `Cibule` + `Cibulová nať` + `Citron` + `Cizrna` + `Cuketa` + `Cukr` + `Dijonská hořčice` + 
                 `Gothaj` + `Hrášek` + `Jablko` + `Kmín` + `Kvasný ocet` + `Ledový salát` + `Majonéza` + `Med` + `Mletá paprika` + 
                 `Mrkev` + `Nakládané okurky` + `Olivový olej` + `Olomoucký syreček` + `Paprika` + `Pepř` + `Petržel` + 
                 `Plnotučná hořčice` + `Pomeranč` + `Řapíkatý celer` + `Ředkvičky` + `Salám Junior` + `Šalotka` + `Slunečnicový olej` + 
                 `Šunka` + `Tatarská omáčka` + `Uzené maso` + `Vejce` + `Vinný ocet`, 
               datbt, family = binomial())
summary(bt_mod3) 
bt_coef3 <- coef(bt_mod3)


## Přidání mezikrajových rozdílů jako (fixed effects) ------------------------------------------------------
## Referenční hodnota brambor je v každém kraji = 0 (proto je vyřazen přímý efekt kraje).
## Ze syntaktických důvodů je do definice modelu přidán intercept, aby byly lépe definovány interakce.
## Intercept je následně od parametrů odečten (jinak by parametr brambor nebyl roven nule, ale tomuto interceptu)
## Řádově rychlejší estimace ve srovnání s pozdějším odhadem s náhodnými proměnnými
## Praha je referenční kraj (hlavní efekt). Interakce představují rozdíly krajů oproti Praze.
system.time(bt_mod4 <- glm(response ~ 1 + `Avokádo` + `Balkánský sýr` + `Celer` + `Čerstvá pažitka` + `Červená řepa` + `Česnek` + 
                             `Cherry rajčata` + `Cibule` + `Cibulová nať` + `Citron` + `Cizrna` + `Cuketa` + `Cukr` + `Dijonská hořčice` + 
                             `Gothaj` + `Hrášek` + `Jablko` + `Kmín` + `Kvasný ocet` + `Ledový salát` + `Majonéza` + `Med` + `Mletá paprika` + 
                             `Mrkev` + `Nakládané okurky` + `Olivový olej` + `Olomoucký syreček` + `Paprika` + `Pepř` + `Petržel` + 
                             `Plnotučná hořčice` + `Pomeranč` + `Řapíkatý celer` + `Ředkvičky` + `Salám Junior` + `Šalotka` + `Slunečnicový olej` + 
                             `Šunka` + `Tatarská omáčka` + `Uzené maso` + `Vejce` + `Vinný ocet` +
                             (`Avokádo` + `Balkánský sýr` + `Celer` + `Čerstvá pažitka` + `Červená řepa` + `Česnek` + 
                                `Cherry rajčata` + `Cibule` + `Cibulová nať` + `Citron` + `Cizrna` + `Cuketa` + `Cukr` + `Dijonská hořčice` + 
                                `Gothaj` + `Hrášek` + `Jablko` + `Kmín` + `Kvasný ocet` + `Ledový salát` + `Majonéza` + `Med` + `Mletá paprika` + 
                                `Mrkev` + `Nakládané okurky` + `Olivový olej` + `Olomoucký syreček` + `Paprika` + `Pepř` + `Petržel` + 
                                `Plnotučná hořčice` + `Pomeranč` + `Řapíkatý celer` + `Ředkvičky` + `Salám Junior` + `Šalotka` + `Slunečnicový olej` + 
                                `Šunka` + `Tatarská omáčka` + `Uzené maso` + `Vejce` + `Vinný ocet`) : kraj, 
                           datbt, family = binomial()))
summary(bt_mod4)
anova(bt_mod3, bt_mod4, test = "LRT") ## model s krajovými rozdíly popisuje data lépe
cbind(BIC(bt_mod3, bt_mod4), AIC(bt_mod3, bt_mod4))[, -3] ## a rozdíl je ubjektivně poměrně velký
bt_coef4 <- coef(bt_mod4) ## dále pracujeme jen s touto verzí modelu.

plot(bt_coef3, bt_coef4[2:43]) ## koeficienty pro Prahu se prakticky neliší od celkových koeficientů pro ČR z předchozího modelu 


### Odhad reliability parametrů --------------------------------------------------------------

partab <- summary(bt_mod4)
## Reliabilita odhadu parametrů surovin uvnitř jednotlivých krajů
rel_kraj <- NULL
for (i in 0:13) {
  x <- var(partab$coefficients[c(44 + (0:41)*14) +i, "Estimate"])
  rmse <- mean(partab$coefficients[c(44 + (0:41)*14) +i, "Std. Error"]**2)
  rel_kraj <- c(rel_kraj, x/(x+rmse))
}
names(rel_kraj) <- levels(datbt$kraj)[-1]
rel_kraj ## hodně malá

## Reliabilita odhadu parametrů krajů napříč surovinami
rel_surovina <- NULL
for (i in 0:41) {
  x <- var(partab$coefficients[c(44:57) + i*14, "Estimate"])
  rmse <- mean(partab$coefficients[c(44:57) + i, "Estimate"]**2)
  rel_surovina <- c(rel_surovina, x/(x+rmse))
}
names(rel_surovina) <- names(bt_coef4)[2:43]
sort(rel_surovina) ## zpravidla malá; u majonézy jsou krajové rozdíly zanedbatelné, u Gothaje naopak


## Tvorba tabulky s parametry ----------------------------------------------------------------

## Sloupce: kraje; řádky: suroviny
bt_coef4_tab <- matrix(bt_coef4[-c(1:43)], nrow = 42, byrow = T, 
                       dimnames = list(codebook$item[codebook$item != "Brambory"], 
                                       levels(datbt$kraj)[-1])) ## jednotlivé kraje (rozdíl oproti Praze)
bt_coef4_tab <- cbind(`Hlavní město Praha` = bt_coef4[2:43], bt_coef4_tab) ## přidání main efektu (Praha)
bt_coef4_tab <- bt_coef4_tab - bt_coef4[1] ## odečtení interceptu a posunutí celé škály (tak, aby brambory byly referenční hodnotou =0 v každém z krajů)
bt_coef4_tab[, -1] <- bt_coef4_tab[, -1] + bt_coef4_tab[,1] ## sečtení parametrů pro jednotlivé kraje s main efektem (celkový efekt pro jednotlivé kraje)

## přidání průměrného efektu za celou ČR. Tento krok je problematický, ... 
## ... protože každý kraj má stejnou váhu. Hodně odlišné kraje a kraje s ... 
## ... hodně málo respondenty tak mají příliš velký vliv na výsledek. 
bt_coef4_tab <- cbind(`Česká republika` = rowMeans(bt_coef4_tab), bt_coef4_tab) 
bt_coef4_tab <- bt_coef4_tab[order(rowMeans(bt_coef4_tab), decreasing = T), ] ## seřazení podle průměru
bt_coef4_tab <- rbind(Brambory = 0, bt_coef4_tab) ## doplnění referenčních brambor na první řádek.
bt_coef4_tab_p <- exp(bt_coef4_tab)

(bt_coef4_tab_sd <- apply(bt_coef4_tab[,-1], 1, sd)) ## směrodatná odchylka surovin
sort(bt_coef4_tab_sd) ## Kraje s vyšší hodnotou se výrazněji liší. Pořadí odpovídá reliabilitě výše (jiný pohled na data)


### EXPORT PARAMETRŮ ----------------------------------------------------------------------------

bt_coef4_tab ## KLÍČOVÁ TABULKA S PARAMETRY PRO JEDNOTLIVÉ KRAJE ("hrubá" škála, logity)
bt_coef4_tab_p ## KLÍČOVÁ TABULKA S PARAMETRY PRO JEDNOTLIVÉ KRAJE ("procentuální" škála)


## Grafy -------------------------------------------------------------------------------------

cols <- rainbow(15)

## Graf na původní hrubé škále
windows(15, 15)
par(mar = c(5.1, 8.1, 4.1, 2.1))
plot(bt_coef4_tab[,1], 43:1, 
     yaxt = "n", xlab = "logit", ylab="", xlim=c(-5, .1), 
     pch = 16, cex = 2, main = "logitová škála (brambory = 0)")
axis(2, at=43:1, labels = rownames(bt_coef4_tab), las=1, cex.axis = .7)
abline(h = 1:43, col="gray")
for (i in 2:43) {
  points(bt_coef4_tab[i, -1], rep(44-i, ncol(bt_coef4_tab[, -1])), 
         col=cols, pch=16)
  lines(c(bt_coef4_tab[i,1] - quant*bt_coef4_tab_sd[i], 
          bt_coef4_tab[i,1] + quant*bt_coef4_tab_sd[i]), 
        c(44-i, 44-i), lwd = 2)
}
legend("bottomright", colnames(bt_coef4_tab), col=cols, cex=.75, pch = 16, bg = "white")

## Graf na "procentuální" škále
windows(15, 15)
par(mar = c(5.1, 8.1, 4.1, 2.1))
plot(rowMeans(bt_coef4_tab_p), 43:1, 
     yaxt = "n", xlab = "pravděpodobnost", ylab="", xlim=c(0, 1.2), 
     pch = 16, cex = 2, main = "Srovnání s bramborami")
axis(2, at=43:1, labels = rownames(bt_coef4_tab), las=1, cex.axis = .7)
abline(h = 1:43, col="gray")
for (i in 2:43) {
  points(bt_coef4_tab_p[i, ], rep(44-i, ncol(bt_coef4_tab_p)), 
         col=cols, pch=16)
  lines(c(exp(rowMeans(bt_coef4_tab)[i] - quant*bt_coef4_tab_sd[i]), 
          exp(rowMeans(bt_coef4_tab)[i] + quant*bt_coef4_tab_sd[i])), 
        c(44-i, 44-i), lwd = 2)
}
legend("bottomright", colnames(bt_coef4_tab), col=cols, cex=.75, pch = 16, bg = "white")



#  ________________ ---------------------------------------------------------------------------
# ALTERNATIVNÍ MODEL --------------------------------------------------------------------------------
# Toto je alternativní model, kde jsou efekty krajů odhadovány jako náhodné efekty.
# Model je výpočetně náročnější. Z estimačních důvodů navíc předpokládáme, že náhodné efekty nekorelují.
# Zařazení kovariancí do modelu by vedlo k potížím v estimaci, šlo by o příliš mnoho parametrů.
# Dále předpokládám, že surovin existuje nekonečně velké množství, z nichž "náhodně" nějaké vybíráme.
# PROS: Model je konzervativní, vede spíše k menším rozdílům mezi kraji, méně náchylný na rozdílnou velikost vzorku napříč kraji.
# CONS: Výsledky parametrů mohou být zkreslené, kvůli nedodržení předpokladů (viz výše)

## Odhad modelu. Pozor, trvá dlouho; na Intel i5-7200U CPU s 2.5 GHz celkem 49 minut
system.time(bt_mod2 <- glmer(response ~ 0 + `Avokádo` + `Balkánský sýr` + `Brambory` + `Celer` + `Čerstvá pažitka` + `Červená řepa` + `Česnek` + 
                               `Cherry rajčata` + `Cibule` + `Cibulová nať` + `Citron` + `Cizrna` + `Cuketa` + `Cukr` + `Dijonská hořčice` + 
                               `Gothaj` + `Hrášek` + `Jablko` + `Kmín` + `Kvasný ocet` + `Ledový salát` + `Majonéza` + `Med` + `Mletá paprika` + 
                               `Mrkev` + `Nakládané okurky` + `Olivový olej` + `Olomoucký syreček` + `Paprika` + `Pepř` + `Petržel` + 
                               `Plnotučná hořčice` + `Pomeranč` + `Řapíkatý celer` + `Ředkvičky` + `Salám Junior` + `Šalotka` + `Slunečnicový olej` + 
                               `Šunka` + `Tatarská omáčka` + `Uzené maso` + `Vejce` + `Vinný ocet` + 
                               (0 + `Avokádo` + `Balkánský sýr` + `Brambory` + `Celer` + `Čerstvá pažitka` + `Červená řepa` + `Česnek` + 
                                  `Cherry rajčata` + `Cibule` + `Cibulová nať` + `Citron` + `Cizrna` + `Cuketa` + `Cukr` + `Dijonská hořčice` + 
                                  `Gothaj` + `Hrášek` + `Jablko` + `Kmín` + `Kvasný ocet` + `Ledový salát` + `Majonéza` + `Med` + `Mletá paprika` + 
                                  `Mrkev` + `Nakládané okurky` + `Olivový olej` + `Olomoucký syreček` + `Paprika` + `Pepř` + `Petržel` + 
                                  `Plnotučná hořčice` + `Pomeranč` + `Řapíkatý celer` + `Ředkvičky` + `Salám Junior` + `Šalotka` + `Slunečnicový olej` + 
                                  `Šunka` + `Tatarská omáčka` + `Uzené maso` + `Vejce` + `Vinný ocet` || kraj), 
                             verbose = 2, nAGQ = 0, 
                             datbt2, family = binomial()))
save(bt_mod2, file= "bt_mod2.rData")
summary(bt_mod2)

## export parametrů
mod2_fixef <- fixef(bt_mod2) ## pevné efekty
mod2_varef <- as.data.frame(VarCorr(bt_mod2)) ## náhodné efekty (rozptylové komponenty)
mod2_ranef <- ranef(bt_mod2) ## náhodné efekty (odhady pro jednotlivé kraje)

## Srovnání s fixed efekty
t(mod2_ranef$kraj)

co <- "`Plnotučná hořčice`" ## Např u majonézy prakticky žádný vztah, u Gothaje vše funguje, jak má.
plot(t(mod2_ranef$kraj)[co, ], bt_coef4_tab[co, -1], 
     main = co, xlab = "náhodné efekty", ylab = "pevné efekty (v článku)", 
     sub = paste0("Pearsonovo r = ", 
                  round(cor(t(mod2_ranef$kraj)[co, ], bt_coef4_tab[co, -1]), 3)))



## Tvorba alternativního seřazení surovin ----------------------------------------------------

## tvorba tabulky s parametry
mod2_varef$fixef <- mod2_fixef
quant <- qnorm(.95) ## kvantil pro 90% CI
mod2_varef <- mod2_varef[order(mod2_varef$fixef, decreasing = T), ]
mod2_varef$lower <- mod2_varef$fixef - quant*mod2_varef$sdcor ## spodní hranice CI
mod2_varef$upper <- mod2_varef$fixef + quant*mod2_varef$sdcor ## spodní hranice CI

## Zde se liší logika přepočtu logitů na pravděpodobnosti
## Přepočet logitů na pravděpodobnost v rovnici log(P(A>M)/P(M>A)) = A
## kde A je nejvhodnější (brambory), resp. nejméně vhodná surovina (pomeranč). Jde o pravděpodobnost vybrání vůči průměrné surovině
exp(mod2_varef$fixef[1])/(1 + exp(mod2_varef$fixef[1])) ## hrubá pravděpodobnost zařazení brambor: 0.9192305
exp(mod2_varef$fixef[42])/(1 + exp(mod2_varef$fixef[42])) ## hrubá pravděpodobnost zařazení pomeranče: 0.1105389

## Rozšíříme rovnici BT modelu jako log(P(A>M)/P(M>A)) = a*A + b.
## Chceme, aby ppravděpodobnost zařazení pomeranče byla zanedbatelná (0,001) a naopak brambor vysoká (0,999).
## Získáváme dvě rovnice o dvou neznámých:
##    log(.999/(1-.999)) = a*brambory + b
##    log(.001/(1-.001)) = a*pomeranč + b

x <- matrix(c(mod2_fixef["Brambory"], 1, 
              mod2_fixef["Pomeranč"], 1), nrow = 2, byrow = T) ## pravá strana rovnice
y <- matrix(c(log(.999/.001), log(.001/.999))) ## levá strana rovnice
res <- solve(x, y) ## řešení
a <- res[1] ## slope
b <- res[2] ## intercept

mod2_varef$fixef_p <- exp(a*mod2_varef$fixef + b)/(1 + exp(a*mod2_varef$fixef + b))
mod2_varef$lower_p <- exp(a*mod2_varef$lower + b)/(1 + exp(a*mod2_varef$lower + b))
mod2_varef$upper_p <- exp(a*mod2_varef$upper + b)/(1 + exp(a*mod2_varef$upper + b))

## graf ------------------------------------------------------------------

## GRAF 1: původní škála bez jednoznačného měřítka
windows(10, 15)
par(mar = c(5.1, 8.1, 4.1, 2.1))
plot(mod2_varef$fixef, 43:1, yaxt = "n", xlab = "logit", ylab="", pch = 16, main = "původní logitová škála")
axis(2, at=43:1, labels = mod2_varef$var1, las=1, cex.axis = .7)
abline(h = 1:43, col="gray")

for (i in 1:43) {
  lines(c(mod2_varef$lower[i], mod2_varef$upper[i]), c(44-i, 44-i), lwd = 2)
}

## GRAF 1: původní škála převedená na pravděpodobnost tak, že pomeranče mají P=.001 a brambory P=.999
windows(10, 15)
par(mar = c(5.1, 8.1, 4.1, 2.1))
plot(mod2_varef$fixef_p, 43:1, yaxt = "n", xlab = "logit", ylab="", pch = 16, 
     main = "pravděpodobnost zařazení suroviny podle kraje", 
     xlim = c(0,1))
axis(2, at=43:1, labels = mod2_varef$var1, las=1, cex.axis = .7)
abline(h = 1:43, col="gray")

for (i in 1:43) {
  lines(c(mod2_varef$lower_p[i], mod2_varef$upper_p[i]), c(44-i, 44-i), lwd = 2)
}

points(mod2_varef$fixef_p, 43:1, pch = 16, cex = 2)


windows()
ranef_plot <- dotplot(mod2_ranef) ## vytvoří objekt s grafem
which_plot <- which(names(fixef(bt_mod2)) == "`Salám Junior`") ## kterou surovinu zobrazit? Dávají smysl ty, které měly na předchozích vysokou variabilitu
which_plot <- which(names(fixef(bt_mod2)) == "Avokádo") ## kterou surovinu zobrazit? Dávají smysl ty, které měly na předchozích vysokou variabilitu
plot(ranef_plot$kraj[which_plot]) ## zobrazit


sessionInfo()
# >  sessionInfo()
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Czech_Czechia.utf8  LC_CTYPE=Czech_Czechia.utf8    LC_MONETARY=Czech_Czechia.utf8
# [4] LC_NUMERIC=C                   LC_TIME=Czech_Czechia.utf8    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lattice_0.20-45 psych_2.2.9     lme4_1.1-31     Matrix_1.5-1   
# 
# loaded via a namespace (and not attached):
#   [1] minqa_1.2.5            colorspace_2.0-3       deldir_1.0-6           estimability_1.4.1    
# [5] circlize_0.4.15        htmlTable_2.4.1        corpcor_1.6.10         parameters_0.20.0     
# [9] GlobalOptions_0.1.2    base64enc_0.1-3        rstudioapi_0.14        mice_3.15.0           
# [13] lavaan_0.6-12          MatrixModels_0.5-1     fansi_1.0.3            mvtnorm_1.1-3         
# [17] splines_4.2.2          mnormt_2.1.1           knitr_1.41             glasso_1.11           
# [21] Formula_1.2-4          nloptr_2.0.3           broom_1.0.1            cluster_2.1.4         
# [25] yarrr_0.1.5            png_0.1-8              miceadds_3.15-21       effectsize_0.8.2      
# [29] compiler_4.2.2         emmeans_1.8.2          backports_1.4.1        assertthat_0.2.1      
# [33] fastmap_1.1.0          cli_3.3.0              htmltools_0.5.3        tools_4.2.2           
# [37] igraph_1.3.5           OpenMx_2.20.6          coda_0.19-4            gtable_0.3.1          
# [41] glue_1.6.2             reshape2_1.4.4         dplyr_1.0.10           Rcpp_1.0.8.3          
# [45] carData_3.0-5          vctrs_0.5.1            Amelia_1.8.1           nlme_3.1-160          
# [49] lisrelToR_0.1.5        insight_0.18.8         xfun_0.35              stringr_1.4.0         
# [53] openxlsx_4.2.5.1       lifecycle_1.0.3        semTools_0.5-6         gtools_3.9.4          
# [57] XML_3.99-0.12          MASS_7.3-58.1          scales_1.2.1           BayesFactor_0.9.12-4.4
# [61] kutils_1.70            parallel_4.2.2         RColorBrewer_1.1-3     pbapply_1.6-0         
# [65] gridExtra_2.3          ggplot2_3.4.0          rpart_4.1.19           latticeExtra_0.6-30   
# [69] stringi_1.7.8          bayestestR_0.13.0      corrplot_0.92          sem_3.1-15            
# [73] checkmate_2.1.0        ppcor_1.1              boot_1.3-28            zip_2.2.2             
# [77] shape_1.4.6            rlang_1.0.6            pkgconfig_2.0.3        arm_1.13-1            
# [81] purrr_0.3.5            htmlwidgets_1.5.4      tidyselect_1.2.0       plyr_1.8.8            
# [85] magrittr_2.0.3         R6_2.5.1               generics_0.1.3         Hmisc_4.7-2           
# [89] DBI_1.1.3              pillar_1.8.1           foreign_0.8-83         rockchalk_1.8.157     
# [93] datawizard_0.6.4       survival_3.4-0         semPlot_1.1.6          abind_1.4-5           
# [97] nnet_7.3-18            tibble_3.1.8           interp_1.1-3           fdrtool_1.2.17        
# [101] utf8_1.2.2             jpeg_0.1-10            grid_4.2.2             qgraph_1.9.2          
# [105] data.table_1.14.6      pbivnorm_0.6.0         digest_0.6.29          xtable_1.8-4          
# [109] mi_1.1                 tidyr_1.2.1            RcppParallel_5.1.5     stats4_4.2.2          
# [113] munsell_0.5.0          mitools_2.4           