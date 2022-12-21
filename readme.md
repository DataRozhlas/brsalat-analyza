# Analýza dat o popularitě ingrediencí do bramborového salátu

Anonymizovaná data pocházejí z aplikace serveru iROZHLAS.cz [Rozstřel bramborových salátů](https://data.irozhlas.cz/salat-or-not/).

Výsledný článek: [Co do bramborového salátu patří, a co už radši ne? Máme opověď na otázku, která umí zkazit Vánoce](https://www.irozhlas.cz/node/8894604)

Autorem analýzy je psychometrik [Hynek Cígler](https://github.com/hynekcigler) z Masarykovy univerzity.

Pro zopoakování analýzy potřebujete volně dostupný software [R](https://www.r-project.org/), doporučujeme také nějaké pohodlné IDE jako je [RStudio](https://www.rstudio.com/).

V něm pak stačí otevřít soubor s analýzou `geosalat_BT_v03publish.R` a užít si ji krok za krokem.

Pokud preferujete jiné nástroje a postupy, data i vysvětlivky najdete v `geosalat.csv` a `codebook.csv`.

Skripty s příponou `.ts` slouží k transformaci a geolokaci zdrojových dat, která nejsou součástí tohoto repozitáře.