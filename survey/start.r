## ---- load data ---- ##
# data
df<-read.table("rdata.csv", sep="\t", encoding="UTF-8", header=TRUE)
# module names
modules<-c("ALP1", "ALP2", "ALP3", "ALP4", "ALP5", "SWP", "MafI1", "MafI2", "MafI3", "GTI", "PS", "DBS", "TI1", "TI2", "TI3", "TI4", "AWS", "SWT")
# weeks per sem
wps    <-c( 17,     13,     17,     13,     17,     15,    17,      13,      17,      13,    17,   13,    17,    13,    17,    13,    3,    13)
# LP
lps    <-c( 8,      8,      8,      5,      5,      10,    8,       8,       8,       7,     3,    7,     5,     5,     5,     5,     4,     6)
# load plots
source("plotting.r")
# data points above will be assumed outsiders
dpupperbound<-80
## ------------ ##


## ---- remove unwanted ---- ##
df$SERIAL<-NULL
df$REF<-NULL
df$QUESTNNR<-NULL
df$MODE<-NULL
df$STARTED<-NULL
df$MAILSENT<-NULL
df$LASTDATA<-NULL
df$LASTPAGE<-NULL
df$MISSING<-NULL
df$MISSREL<-NULL
df$DEG_MISS<-NULL
df$DEG_TIME<-NULL
df$DEGRADE<-NULL
df$TIME001<-NULL
df$TIME002<-NULL
df$TIME003<-NULL
df$TIME004<-NULL
df$TIME005<-NULL
df$TIME006<-NULL
df$TIME007<-NULL
df$TIME008<-NULL
df$TIME009<-NULL
df$TIME010<-NULL
df$TIME011<-NULL
df$TIME012<-NULL
df$TIME013<-NULL
df$TIME014<-NULL
df$TIME015<-NULL
df$TIME016<-NULL
df$TIME017<-NULL
df$TIME018<-NULL
df$TIME019<-NULL
df$TIME020<-NULL
df$TIME021<-NULL
df$TIME022<-NULL
df$TIME023<-NULL
df$TIME024<-NULL
df$TIME025<-NULL
df$TIME026<-NULL
df$TIME027<-NULL
df$TIME028<-NULL
df$TIME029<-NULL
df$TIME030<-NULL
df$TIME031<-NULL
df$TIME032<-NULL
df$TIME033<-NULL
df$TIME034<-NULL
df$TIME035<-NULL
df$TIME036<-NULL
df$TIME037<-NULL
df$TIME038<-NULL
df$TIME039<-NULL
df$TIME040<-NULL
## ------------ ##


## ---- choose labels and data types ---- ##
df$InZielgruppe<-factor(df$AD01, levels=c(1, 2), labels=c("Nein", "Ja"))
df$AD01<-NULL
df$Abgeschlossen<-factor(df$AD02, levels=c(1, 2), labels=c("Ja", "Nein"))
df$AD02<-NULL
df$Erststudium<-factor(df$AD03, levels=c(1, 2), labels=c("Ja", "Nein"))
df$AD03<-NULL
df$Fachsemester<-factor(df$AD04, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", ">14"))
df$AD04<-NULL
df$TeilVollzeit<-factor(df$AD05, levels=c(1, 2, 3), labels=c("Vollzeit", "Teilzeit", "andere Regelung"))
df$AD05<-NULL
df$Gesundheitszustand<-factor(df$AD08_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$AD08_01<-NULL
df$eigenerComputer<-factor(df$AD10, levels=c(1, 2), labels=c("Ja", "Nein"))
df$AD10<-NULL
df$Vorwissen<-factor(df$AD12, levels=c(1, 2), labels=c("Ja", "Nein"))
df$AD12<-NULL
df$ALP1Absolviert<-df$AM01_01
df$AM01_01<-NULL
df$ALP2Absolviert<-df$AM01_02
df$AM01_02<-NULL
df$ALP3Absolviert<-df$AM01_03
df$AM01_03<-NULL
df$ALP4Absolviert<-df$AM01_04
df$AM01_04<-NULL
df$ALP5Absolviert<-df$AM01_05
df$AM01_05<-NULL
df$SWPAbsolviert<-df$AM01_18
df$AM01_18<-NULL
df$MafI1Absolviert<-df$AM01_06
df$AM01_06<-NULL
df$MafI2Absolviert<-df$AM01_07
df$AM01_07<-NULL
df$MafI3Absolviert<-df$AM01_08
df$AM01_08<-NULL
df$GTIAbsolviert<-df$AM01_09
df$AM01_09<-NULL
df$PSAbsolviert<-df$AM01_17
df$AM01_17<-NULL
df$DBSAbsolviert<-df$AM01_14
df$AM01_14<-NULL
df$TI1Absolviert<-df$AM01_10
df$AM01_10<-NULL
df$TI2Absolviert<-df$AM01_11
df$AM01_11<-NULL
df$TI3Absolviert<-df$AM01_12
df$AM01_12<-NULL
df$TI4Absolviert<-df$AM01_13
df$AM01_13<-NULL
df$AWSAbsolviert<-df$AM01_15
df$AM01_15<-NULL
df$SWTAbsolviert<-df$AM01_16
df$AM01_16<-NULL
df$ALP1KlausurNachklausur<-factor(df$AM02_01, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_01<-NULL
df$ALP2KlausurNachklausur<-factor(df$AM02_02, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_02<-NULL
df$ALP3KlausurNachklausur<-factor(df$AM02_03, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_03<-NULL
df$ALP4KlausurNachklausur<-factor(df$AM02_04, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_04<-NULL
df$ALP5KlausurNachklausur<-factor(df$AM02_05, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_05<-NULL
df$MafI1KlausurNachklausur<-factor(df$AM02_06, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_06<-NULL
df$MafI2KlausurNachklausur<-factor(df$AM02_07, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_07<-NULL
df$MafI3KlausurNachklausur<-factor(df$AM02_08, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_08<-NULL
df$GTIKlausurNachklausur<-factor(df$AM02_09, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_09<-NULL
df$TI1KlausurNachklausur<-factor(df$AM02_10, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_10<-NULL
df$TI2KlausurNachklausur<-factor(df$AM02_11, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_11<-NULL
df$TI3KlausurNachklausur<-factor(df$AM02_12, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_12<-NULL
df$TI4KlausurNachklausur<-factor(df$AM02_13, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_13<-NULL
df$DBSKlausurNachklausur<-factor(df$AM02_14, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_14<-NULL
df$AWSKlausurNachklausur<-factor(df$AM02_15, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_15<-NULL
df$SWTKlausurNachklausur<-factor(df$AM02_16, levels=c(1, 2, 3), labels=c("Klausur", "Nachklausur", "nicht bestanden"))
df$AM02_16<-NULL
df$ALP1Fachsemester<-factor(df$AM04_01, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_01<-NULL
df$ALP2Fachsemester<-factor(df$AM04_02, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_02<-NULL
df$ALP3Fachsemester<-factor(df$AM04_03, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_03<-NULL
df$ALP4Fachsemester<-factor(df$AM04_04, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_04<-NULL
df$ALP5Fachsemester<-factor(df$AM04_05, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_05<-NULL
df$MafI1Fachsemester<-factor(df$AM04_06, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_06<-NULL
df$MafI2Fachsemester<-factor(df$AM04_07, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_07<-NULL
df$MafI3Fachsemester<-factor(df$AM04_08, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_08<-NULL
df$GTIFachsemester<-factor(df$AM04_09, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_09<-NULL
df$TI1Fachsemester<-factor(df$AM04_10, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_10<-NULL
df$TI2Fachsemester<-factor(df$AM04_11, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_11<-NULL
df$TI3Fachsemester<-factor(df$AM04_12, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_12<-NULL
df$TI4Fachsemester<-factor(df$AM04_13, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_13<-NULL
df$DBSFachsemester<-factor(df$AM04_14, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_14<-NULL
df$AWSFachsemester<-factor(df$AM04_15, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_15<-NULL
df$SWTFachsemester<-factor(df$AM04_16, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_16<-NULL
df$PSFachsemester<-factor(df$AM04_17, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_17<-NULL
df$SWPFachsemester<-factor(df$AM04_18, levels=c(1, 2, 3, 4), labels=c("nicht bestanden", "1", "2", "3 oder mehr"))
df$AM04_18<-NULL
df$ALP1Beschäftigung<-as.numeric(df$AM05_01)
df$AM05_01<-NULL
df$ALP2Beschäftigung<-as.numeric(df$AM05_02)
df$AM05_02<-NULL
df$ALP3Beschäftigung<-as.numeric(df$AM05_03)
df$AM05_03<-NULL
df$ALP4Beschäftigung<-as.numeric(df$AM05_04)
df$AM05_04<-NULL
df$ALP5Beschäftigung<-as.numeric(df$AM05_05)
df$AM05_05<-NULL
df$SWPBeschäftigung<-as.numeric(df$AM05_06)
df$AM05_06<-NULL
df$MafI1Beschäftigung<-as.numeric(df$AM05_07)
df$AM05_07<-NULL
df$MafI2Beschäftigung<-as.numeric(df$AM05_08)
df$AM05_08<-NULL
df$MafI3Beschäftigung<-as.numeric(df$AM05_09)
df$AM05_09<-NULL
df$GTIBeschäftigung<-as.numeric(df$AM05_10)
df$AM05_10<-NULL
df$PSBeschäftigung<-as.numeric(df$AM05_11)
df$AM05_11<-NULL
df$DBSBeschäftigung<-as.numeric(df$AM05_12)
df$AM05_12<-NULL
df$TI1Beschäftigung<-as.numeric(df$AM05_13)
df$AM05_13<-NULL
df$TI2Beschäftigung<-as.numeric(df$AM05_14)
df$AM05_14<-NULL
df$TI3Beschäftigung<-as.numeric(df$AM05_15)
df$AM05_15<-NULL
df$TI4Beschäftigung<-as.numeric(df$AM05_16)
df$AM05_16<-NULL
df$AWSBeschäftigung<-as.numeric(df$AM05_17)
df$AM05_17<-NULL
df$SWTBeschäftigung<-as.numeric(df$AM05_18)
df$AM05_18<-NULL
df$Alter<-as.factor(df$PD01_01)
df$PD01_01<-NULL
df$Geschlecht<-factor(df$PD02, levels=c(1, 2, 3), labels=c("maennlich", "weiblich", "anderes"))
df$PD02<-NULL
df$Familienstand<-factor(df$PD03, levels=c(1, 2, 3), labels=c("Single", "verheiratet", "geschieden"))
df$PD03<-NULL
df$Kinder<-factor(df$PD04, levels=c(1, 2, 3, 4), labels=c("0", "1", "2", "3 oder mehr"))
df$PD04<-NULL
df$StaatsangehörigkeitDeutsch<-factor(df$PD05_01, levels=c(1, 2), labels=c("Nein", "Ja"))
df$PD05_01<-NULL
df$StaatsangehörigkeitAndere<-factor(df$PD05_02, levels=c(1, 2), labels=c("Nein", "Ja"))
df$PD05_02<-NULL
df$Wohnsituation<-factor(df$PD07, levels=c(1, 2, 3, 4, 6), labels=c("alleine", "mit Eltern", "mit Partner", "in einer Wohngemeinschaft", "andere Wohnsituation"))
df$PD07<-NULL
df$Einnahmen<-factor(df$PD08, levels=c(1, 2, 3, 4), labels=c("bis zu 400", "bis zu 800", "bis zu 1200", "mehr als 1200"))
df$PD08<-NULL
df$ALP1GesamterWorkload<-as.numeric(df$M113_01, levels=c(), labels=c())
df$M113_01<-NULL
df$ALP1Schwierigkeit<-factor(df$M106_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M106_01<-NULL
df$ALP1Interesse<-factor(df$M105_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M105_01<-NULL
df$ALP1Vorkenntnisse<-factor(df$M107_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M107_01<-NULL
df$ALP1VBesucht<-factor(df$M114_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M114_01<-NULL
df$ALP1UBearbeitet<-factor(df$M115_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M115_01<-NULL
df$ALP1WorkloadDiff<-factor(df$M117_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M117_01<-NULL
df$ALP2GesamterWorkload<-as.numeric(df$M213_01, levels=c(), labels=c())
df$M213_01<-NULL
df$ALP2Schwierigkeit<-factor(df$M206_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M206_01<-NULL
df$ALP2Interesse<-factor(df$M205_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M205_01<-NULL
df$ALP2Vorkenntnisse<-factor(df$M207_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M207_01<-NULL
df$ALP2VBesucht<-factor(df$M214_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M214_01<-NULL
df$ALP2UBearbeitet<-factor(df$M215_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M215_01<-NULL
df$ALP2WorkloadDiff<-factor(df$M217_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M217_01<-NULL
df$ALP3GesamterWorkload<-as.numeric(df$M313_01, levels=c(), labels=c())
df$M313_01<-NULL
df$ALP3Schwierigkeit<-factor(df$M306_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M306_01<-NULL
df$ALP3Interesse<-factor(df$M305_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M305_01<-NULL
df$ALP3Vorkenntnisse<-factor(df$M307_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M307_01<-NULL
df$ALP3VBesucht<-factor(df$M314_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M314_01<-NULL
df$ALP3UBearbeitet<-factor(df$M315_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M315_01<-NULL
df$ALP3WorkloadDiff<-factor(df$M317_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M317_01<-NULL
df$ALP4GesamterWorkload<-as.numeric(df$M413_01, levels=c(), labels=c())
df$M413_01<-NULL
df$ALP4Schwierigkeit<-factor(df$M406_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M406_01<-NULL
df$ALP4Interesse<-factor(df$M405_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M405_01<-NULL
df$ALP4Vorkenntnisse<-factor(df$M407_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M407_01<-NULL
df$ALP4VBesucht<-factor(df$M414_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M414_01<-NULL
df$ALP4UBearbeitet<-factor(df$M415_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M415_01<-NULL
df$ALP4WorkloadDiff<-factor(df$M417_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M417_01<-NULL
df$ALP5GesamterWorkload<-as.numeric(df$M513_01, levels=c(), labels=c())
df$M513_01<-NULL
df$ALP5Schwierigkeit<-factor(df$M506_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M506_01<-NULL
df$ALP5Interesse<-factor(df$M505_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M505_01<-NULL
df$ALP5Vorkenntnisse<-factor(df$M507_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M507_01<-NULL
df$ALP5VBesucht<-factor(df$M514_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M514_01<-NULL
df$ALP5UBearbeitet<-factor(df$M515_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M515_01<-NULL
df$ALP5WorkloadDiff<-factor(df$M517_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M517_01<-NULL
df$SWPGesamterWorkload<-as.numeric(df$M613_01, levels=c(), labels=c())
df$M613_01<-NULL
df$SWPSchwierigkeit<-factor(df$M606_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M606_01<-NULL
df$SWPInteresse<-factor(df$M605_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M605_01<-NULL
df$SWPVorkenntnisse<-factor(df$M607_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M607_01<-NULL
df$SWPPBearbeitet<-factor(df$M615_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M615_01<-NULL
df$SWPWorkloadDiff<-factor(df$M617_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M617_01<-NULL
df$Gebiet<-factor(df$M618, levels=c(1, 2, 3, 4), labels=c("Angewandte Informatik", "Praktische Informatik", "Technische Informatik", "Theoretische Informatik"))
df$M618<-NULL
df$MafI1GesamterWorkload<-as.numeric(df$M713_01, levels=c(), labels=c())
df$M713_01<-NULL
df$MafI1Schwierigkeit<-factor(df$M706_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M706_01<-NULL
df$MafI1Interesse<-factor(df$M705_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M705_01<-NULL
df$MafI1Vorkenntnisse<-factor(df$M707_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M707_01<-NULL
df$MafI1VBesucht<-factor(df$M714_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M714_01<-NULL
df$MafI1UBearbeitet<-factor(df$M715_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M715_01<-NULL
df$MafI1WorkloadDiff<-factor(df$M717_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M717_01<-NULL
df$MafI2GesamterWorkload<-as.numeric(df$M813_01, levels=c(), labels=c())
df$M813_01<-NULL
df$MafI2Schwierigkeit<-factor(df$M806_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M806_01<-NULL
df$MafI2Interesse<-factor(df$M805_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M805_01<-NULL
df$MafI2Vorkenntnisse<-factor(df$M807_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M807_01<-NULL
df$MafI2VBesucht<-factor(df$M814_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M814_01<-NULL
df$MafI2UBearbeitet<-factor(df$M815_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M815_01<-NULL
df$MafI2WorkloadDiff<-factor(df$M817_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M817_01<-NULL
df$MafI3GesamterWorkload<-as.numeric(df$M913_01, levels=c(), labels=c())
df$M913_01<-NULL
df$MafI3Schwierigkeit<-factor(df$M906_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$M906_01<-NULL
df$MafI3Interesse<-factor(df$M905_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M905_01<-NULL
df$MafI3Vorkenntnisse<-factor(df$M907_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$M907_01<-NULL
df$MafI3VBesucht<-factor(df$M914_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M914_01<-NULL
df$MafI3UBearbeitet<-factor(df$M915_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$M915_01<-NULL
df$MafI3WorkloadDiff<-factor(df$M917_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$M917_01<-NULL
df$GTIGesamterWorkload<-as.numeric(df$N113_01, levels=c(), labels=c())
df$N113_01<-NULL
df$GTISchwierigkeit<-factor(df$N106_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N106_01<-NULL
df$GTIInteresse<-factor(df$N105_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N105_01<-NULL
df$GTIVorkenntnisse<-factor(df$N107_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N107_01<-NULL
df$GTIVBesucht<-factor(df$N114_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N114_01<-NULL
df$GTIUBearbeitet<-factor(df$N115_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N115_01<-NULL
df$GTIWorkloadDiff<-factor(df$N117_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N117_01<-NULL
df$PSGesamterWorkload<-as.numeric(df$N213_01, levels=c(), labels=c())
df$N213_01<-NULL
df$PSSchwierigkeit<-factor(df$N206_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N206_01<-NULL
df$PSInteresse<-factor(df$N205_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N205_01<-NULL
df$PSVorkenntnisse<-factor(df$N207_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N207_01<-NULL
df$PSPSBearbeitet<-factor(df$N214_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N214_01<-NULL
df$PSWorkloadDiff<-factor(df$N217_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N217_01<-NULL
df$Gebiet<-factor(df$N218, levels=c(1, 2, 3, 4), labels=c("Angewandte Informatik", "Praktische Informatik", "Technische Informatik", "Theoretische Informatik"))
df$N218<-NULL
df$DBSGesamterWorkload<-as.numeric(df$N313_01, levels=c(), labels=c())
df$N313_01<-NULL
df$DBSSchwierigkeit<-factor(df$N306_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N306_01<-NULL
df$DBSInteresse<-factor(df$N305_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N305_01<-NULL
df$DBSVorkenntnisse<-factor(df$N307_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N307_01<-NULL
df$DBSVBesucht<-factor(df$N314_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N314_01<-NULL
df$DBSUBearbeitet<-factor(df$N315_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N315_01<-NULL
df$DBSPBearbeitet<-factor(df$N318_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, -1), labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "Es gab kein Projekt"))
df$N318_01<-NULL
df$DBSWorkloadDiff<-factor(df$N317_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N317_01<-NULL
df$TI1GesamterWorkload<-as.numeric(df$N413_01, levels=c(), labels=c())
df$N413_01<-NULL
df$TI1Schwierigkeit<-factor(df$N406_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N406_01<-NULL
df$TI1Interesse<-factor(df$N405_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N405_01<-NULL
df$TI1Vorkenntnisse<-factor(df$N407_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N407_01<-NULL
df$TI1VBesucht<-factor(df$N414_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N414_01<-NULL
df$TI1UBearbeitet<-factor(df$N415_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N415_01<-NULL
df$TI1WorkloadDiff<-factor(df$N417_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N417_01<-NULL
df$TI2GesamterWorkload<-as.numeric(df$N513_01, levels=c(), labels=c())
df$N513_01<-NULL
df$TI2Schwierigkeit<-factor(df$N506_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N506_01<-NULL
df$TI2Interesse<-factor(df$N505_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N505_01<-NULL
df$TI2Vorkenntnisse<-factor(df$N507_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N507_01<-NULL
df$TI2VBesucht<-factor(df$N514_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N514_01<-NULL
df$TI2UBearbeitet<-factor(df$N515_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N515_01<-NULL
df$TI2WorkloadDiff<-factor(df$N517_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N517_01<-NULL
df$TI3GesamterWorkload<-as.numeric(df$N613_01, levels=c(), labels=c())
df$N613_01<-NULL
df$TI3Schwierigkeit<-factor(df$N606_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N606_01<-NULL
df$TI3Interesse<-factor(df$N605_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N605_01<-NULL
df$TI3Vorkenntnisse<-factor(df$N607_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N607_01<-NULL
df$TI3VBesucht<-factor(df$N614_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N614_01<-NULL
df$TI3UBearbeitet<-factor(df$N615_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N615_01<-NULL
df$TI3WorkloadDiff<-factor(df$N617_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N617_01<-NULL
df$TI4GesamterWorkload<-as.numeric(df$N901_01, levels=c(), labels=c())
df$N901_01<-NULL
df$TI4Schwierigkeit<-factor(df$N902_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N902_01<-NULL
df$TI4Interesse<-factor(df$N903_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N903_01<-NULL
df$TI4Vorkenntnisse<-factor(df$N904_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N904_01<-NULL
df$TI4PBearbeitet<-factor(df$N905_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N905_01<-NULL
df$TI4WorkloadDiff<-factor(df$N906_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N906_01<-NULL
df$AWSGesamterWorkload<-as.numeric(df$N713_01, levels=c(), labels=c())
df$N713_01<-NULL
df$AWSSchwierigkeit<-factor(df$N706_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N706_01<-NULL
df$AWSInteresse<-factor(df$N705_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N705_01<-NULL
df$AWSVorkenntnisse<-factor(df$N707_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N707_01<-NULL
df$AWSVBesucht<-factor(df$N714_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N714_01<-NULL
df$AWSUBearbeitet<-factor(df$N715_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N715_01<-NULL
df$AWSWorkloadDiff<-factor(df$N717_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N717_01<-NULL
df$SWTGesamterWorkload<-as.numeric(df$N813_01, levels=c(), labels=c())
df$N813_01<-NULL
df$SWTSchwierigkeit<-factor(df$N806_01, levels=c(1, 2, 3, 4, 5, 6), labels=c("1", "2", "3", "4", "5", "6"))
df$N806_01<-NULL
df$SWTInteresse<-factor(df$N805_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N805_01<-NULL
df$SWTVorkenntnisse<-factor(df$N807_01, levels=c(1, 2, 3, 4, 5, 6, 7), labels=c("1", "2", "3", "4", "5", "6", "7"))
df$N807_01<-NULL
df$SWTVBesucht<-factor(df$N814_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N814_01<-NULL
df$SWTUBearbeitet<-factor(df$N815_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))
df$N815_01<-NULL
df$SWTWorkloadDiff<-factor(df$N817_01, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1), labels=c("-100%", "-75%", "-50%", "-25%", "0", "+25%", "+50%", "+75%", "+100%", "nicht zutreffend"))
df$N817_01<-NULL
df$ALP1WorkloadU<-as.numeric(df$K101_01, levels=c(), labels=c())
df$K101_01<-NULL
df$ALP1WorkloadK<-as.numeric(df$K102_01, levels=c(), labels=c())
df$K102_01<-NULL
df$ALP1WorkloadV<-as.numeric(df$K103_01, levels=c(), labels=c())
df$K103_01<-NULL
df$ALP1Ereignis<-factor(df$K105, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K105<-NULL
df$ALP2WorkloadU<-as.numeric(df$K201_01, levels=c(), labels=c())
df$K201_01<-NULL
df$ALP2WorkloadK<-as.numeric(df$K202_01, levels=c(), labels=c())
df$K202_01<-NULL
df$ALP2WorkloadV<-as.numeric(df$K203_01, levels=c(), labels=c())
df$K203_01<-NULL
df$ALP2Ereignis<-factor(df$K205, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K205<-NULL
df$ALP3WorkloadU<-as.numeric(df$K301_01, levels=c(), labels=c())
df$K301_01<-NULL
df$ALP3WorkloadK<-as.numeric(df$K302_01, levels=c(), labels=c())
df$K302_01<-NULL
df$ALP3WorkloadV<-as.numeric(df$K303_01, levels=c(), labels=c())
df$K303_01<-NULL
df$ALP3Ereignis<-factor(df$K305, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K305<-NULL
df$ALP4WorkloadU<-as.numeric(df$K401_01, levels=c(), labels=c())
df$K401_01<-NULL
df$ALP4WorkloadK<-as.numeric(df$K402_01, levels=c(), labels=c())
df$K402_01<-NULL
df$ALP4WorkloadV<-as.numeric(df$K403_01, levels=c(), labels=c())
df$K403_01<-NULL
df$ALP4Ereignis<-factor(df$K405, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K405<-NULL
df$ALP5WorkloadU<-as.numeric(df$K501_01, levels=c(), labels=c())
df$K501_01<-NULL
df$ALP5WorkloadK<-as.numeric(df$K502_01, levels=c(), labels=c())
df$K502_01<-NULL
df$ALP5WorkloadV<-as.numeric(df$K503_01, levels=c(), labels=c())
df$K503_01<-NULL
df$ALP5Ereignis<-factor(df$K505, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K505<-NULL
df$SWPWorkloadKomm<-as.numeric(df$K601_01, levels=c(), labels=c())
df$K601_01<-NULL
df$SWPWorkloadProgrammieren<-as.numeric(df$K603_01, levels=c(), labels=c())
df$K603_01<-NULL
df$SWPEreignis<-factor(df$K605, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K605<-NULL
df$MafI1WorkloadU<-as.numeric(df$K701_01, levels=c(), labels=c())
df$K701_01<-NULL
df$MafI1WorkloadK<-as.numeric(df$K702_01, levels=c(), labels=c())
df$K702_01<-NULL
df$MafI1WorkloadV<-as.numeric(df$K703_01, levels=c(), labels=c())
df$K703_01<-NULL
df$MafI1Ereignis<-factor(df$K705, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K705<-NULL
df$MafI2WorkloadU<-as.numeric(df$K801_01, levels=c(), labels=c())
df$K801_01<-NULL
df$MafI2WorkloadK<-as.numeric(df$K802_01, levels=c(), labels=c())
df$K802_01<-NULL
df$MafI2WorkloadV<-as.numeric(df$K803_01, levels=c(), labels=c())
df$K803_01<-NULL
df$MafI2Ereignis<-factor(df$K805, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K805<-NULL
df$MafI3WorkloadU<-as.numeric(df$K901_01, levels=c(), labels=c())
df$K901_01<-NULL
df$MafI3WorkloadK<-as.numeric(df$K902_01, levels=c(), labels=c())
df$K902_01<-NULL
df$MafI3WorkloadV<-as.numeric(df$K903_01, levels=c(), labels=c())
df$K903_01<-NULL
df$MafI3Ereignis<-factor(df$K905, levels=c(1, 2), labels=c("Ja", "Nein"))
df$K905<-NULL
df$GTIWorkloadU<-as.numeric(df$L101_01, levels=c(), labels=c())
df$L101_01<-NULL
df$GTIWorkloadK<-as.numeric(df$L102_01, levels=c(), labels=c())
df$L102_01<-NULL
df$GTIWorkloadV<-as.numeric(df$L103_01, levels=c(), labels=c())
df$L103_01<-NULL
df$GTIEreignis<-factor(df$L105, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L105<-NULL
df$PSWorkloadVortrag<-as.numeric(df$L201_01, levels=c(), labels=c())
df$L201_01<-NULL
df$PSEreignis<-factor(df$L205, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L205<-NULL
df$DBSWorkloadU<-as.numeric(df$L301_01, levels=c(), labels=c())
df$L301_01<-NULL
df$DBSWorkloadProjekt<-as.numeric(df$L307_01, levels=c(), labels=c())
df$L307_01<-NULL
df$DBSWorkloadK<-as.numeric(df$L302_01, levels=c(), labels=c())
df$L302_01<-NULL
df$DBSWorkloadV<-as.numeric(df$L303_01, levels=c(), labels=c())
df$L303_01<-NULL
df$DBSEreignis<-factor(df$L305, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L305<-NULL
df$TI1WorkloadU<-as.numeric(df$L401_01, levels=c(), labels=c())
df$L401_01<-NULL
df$TI1WorkloadK<-as.numeric(df$L402_01, levels=c(), labels=c())
df$L402_01<-NULL
df$TI1WorkloadV<-as.numeric(df$L403_01, levels=c(), labels=c())
df$L403_01<-NULL
df$TI1Ereignis<-factor(df$L405, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L405<-NULL
df$TI2WorkloadU<-as.numeric(df$L501_01, levels=c(), labels=c())
df$L501_01<-NULL
df$TI2WorkloadK<-as.numeric(df$L502_01, levels=c(), labels=c())
df$L502_01<-NULL
df$TI2WorkloadV<-as.numeric(df$L503_01, levels=c(), labels=c())
df$L503_01<-NULL
df$TI2Ereignis<-factor(df$L505, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L505<-NULL
df$TI3WorkloadU<-as.numeric(df$L601_01, levels=c(), labels=c())
df$L601_01<-NULL
df$TI3WorkloadK<-as.numeric(df$L602_01, levels=c(), labels=c())
df$L602_01<-NULL
df$TI3WorkloadV<-as.numeric(df$L603_01, levels=c(), labels=c())
df$L603_01<-NULL
df$TI3Ereignis<-factor(df$L605, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L605<-NULL
df$TI4WorkloadProgrammieren<-as.numeric(df$L701_01, levels=c(), labels=c())
df$L701_01<-NULL
df$TI4WorkloadDoku<-as.numeric(df$L703_01, levels=c(), labels=c())
df$L703_01<-NULL
df$TI4Ereignis<-factor(df$L705, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L705<-NULL
df$AWSWorkloadU<-as.numeric(df$L801_01, levels=c(), labels=c())
df$L801_01<-NULL
df$AWSWorkloadK<-as.numeric(df$L802_01, levels=c(), labels=c())
df$L802_01<-NULL
df$AWSWorkloadV<-as.numeric(df$L803_01, levels=c(), labels=c())
df$L803_01<-NULL
df$AWSEreignis<-factor(df$L805, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L805<-NULL
df$SWTWorkloadU<-as.numeric(df$L901_01, levels=c(), labels=c())
df$L901_01<-NULL
df$SWTWorkloadK<-as.numeric(df$L902_01, levels=c(), labels=c())
df$L902_01<-NULL
df$SWTWorkloadV<-as.numeric(df$L903_01, levels=c(), labels=c())
df$L903_01<-NULL
df$SWTEreignis<-factor(df$L905, levels=c(1, 2), labels=c("Ja", "Nein"))
df$L905<-NULL
## ------------ ##

## ---- compute total workload ---- ##
for(m in modules) {
	if(m == "SWP") {
		df$SWPWorkloadCalc<-df$SWPWorkloadKomm + df$SWPWorkloadProgrammieren
		df$SWPWorkloadCalcSc<-df$SWPWorkloadKomm + df$SWPWorkloadProgrammieren * as.numeric(df$SWPPBearbeitet) / 100
	}
	else if(m == "PS") {
		df$PSWorkloadCalc<-df$PSWorkloadVortrag / wps[[which(modules==m)]]
		df$PSWorkloadCalcSc<-df$PSWorkloadVortrag * as.numeric(df$PSPSBearbeitet) / (wps[[which(modules==m)]] * 100)
	}
	else if(m == "DBS") {
		df$DBSWorkloadCalc<-df$DBSWorkloadV + df$DBSWorkloadK / wps[[which(modules==m)]] + df$DBSWorkloadU + df$DBSWorkloadProjekt / wps[[which(modules==m)]]
		df$DBSWorkloadCalcSc<-df$DBSWorkloadV * as.numeric(df$DBSVBesucht) / 100 + df$DBSWorkloadK / wps[[which(modules==m)]] + df$DBSWorkloadU * as.numeric(df$DBSUBearbeitet) / 100 + df$DBSWorkloadProjekt * as.numeric(df$DBSPBearbeitet) / (wps[[which(modules==m)]] * 100)
	}
	else if(m == "TI4") {
		df$TI4WorkloadCalc<-df$TI4WorkloadProgrammieren + df$TI4WorkloadDoku
		df$TI4WorkloadCalcSc<-(df$TI4WorkloadProgrammieren + df$TI4WorkloadDoku) * as.numeric(df$TI4PBearbeitet) / 100
	}
	else {
		df[paste(m, "WorkloadCalc", sep="")]<-df[paste(m, "WorkloadV", sep="")] + df[paste(m, "WorkloadK", sep="")] / wps[[which(modules==m)]] + df[paste(m, "WorkloadU", sep="")]
		df[paste(m, "WorkloadCalcSc", sep="")]<-df[paste(m, "WorkloadV", sep="")] * as.numeric(df[[paste(m, "VBesucht", sep="")]]) / 100 + df[paste(m, "WorkloadK", sep="")] / wps[[which(modules==m)]] + df[paste(m, "WorkloadU", sep="")] * as.numeric(df[[paste(m, "UBearbeitet", sep="")]]) / 100
	}
}
## ------------ ##


## ---- compute reliabilty of workload in [-1,1]. "0" means reliability is high, ##
## "-1"/"1" means overall workload was very low/high compared to the computed    ##
## workload ----                                                                 ##
for(m in modules)
	df[paste(m, "WorkloadReliability", sep="")]<-as.numeric(sapply(df[paste(m, "GesamterWorkload", sep="")] / df[paste(m, "WorkloadCalc", sep="")], function(x) ifelse(is.na(x), NA, ifelse(x<1, x-1, 1-1/x))))
## ------------ ##


## ---- remove outsiders and scale workload per lp ---- ##
for(m in 1:length(modules))
	for(i in 1:length(df[[1]])) {
		v<-df[[paste(modules[m], "GesamterWorkload", sep="")]][i] * wps[[m]] / lps[m]
		ifelse((!is.na(df[[paste(modules[m], "WorkloadReliability", sep="")]][i]) && abs(df[[paste(modules[m], "WorkloadReliability", sep="")]][i])>0.5)
				|| is.na(df[[paste(modules[m], "GesamterWorkload", sep="")]][i])
				|| v >= dpupperbound || v < 0,
			df[[paste(modules[m], "WLperLP", sep="")]][i]<-NA,
			df[[paste(modules[m], "WLperLP", sep="")]][i]<-v)
		v<-df[[paste(modules[m], "WorkloadCalc", sep="")]][i] * wps[[m]] / lps[m]
		ifelse((!is.na(df[[paste(modules[m], "WorkloadReliability", sep="")]][i]) && abs(df[[paste(modules[m], "WorkloadReliability", sep="")]][i])>0.5)
				|| is.na(df[[paste(modules[m], "WorkloadCalc", sep="")]][i])
				|| v >= dpupperbound || v < 0,
			df[[paste(modules[m], "WLperLPCalc", sep="")]][i]<-NA,
			df[[paste(modules[m], "WLperLPCalc", sep="")]][i]<-v)
		v<-df[[paste(modules[m], "WorkloadCalcSc", sep="")]][i] * wps[[m]] / lps[m]
		ifelse((!is.na(df[[paste(modules[m], "WorkloadReliability", sep="")]][i]) && abs(df[[paste(modules[m], "WorkloadReliability", sep="")]][i])>0.5)
				|| is.na(df[[paste(modules[m], "WorkloadCalcSc", sep="")]][i])
				|| v >= dpupperbound || v < 0,
			df[[paste(modules[m], "WLperLPCalcSc", sep="")]][i]<-NA,
			df[[paste(modules[m], "WLperLPCalcSc", sep="")]][i]<-v)
	}
## ------------ ##


## ---- define functions ---- ##
# get unification of columns with given suffix for all modules
getUnionForAllModules<-function(name) {
	r<-NULL
	for(m in modules)
		r<-c(r, df[[paste(m, name, sep="")]])
	return(r)
}

# get subset of df with all modules for a given suffix
getSubsetForAllModules<-function(name) {
	l<-NULL
	for(m in modules)
		l<-c(l, paste(m, name, sep=""))
	r<-df[, c(l)]
	names(r)<-modules
	return(r)
}

# add count of non-NAs to columnnames
countToName<-function(frame) {
	r<-NULL
	for(n in names(frame))
		r<-c(r, paste(n, " (", length(which(!is.na(frame[[n]]))), ")", sep=""))
	names(frame)<-r
	return(frame)
}

# add count of levels to levelnames
countToLevel<-function(column) {
	r<-NULL
	for(n in levels(column))
		r<-c(r, paste(n, " (", length(which(column==n)), ")", sep=""))
	return(factor(column, levels=levels(column), labels=r))
}

# list available plots
plots<-function() {
	writeLines("\navailable plots (call ectsplot.plot(name) to draw):\n-----------------------")
	for(p in ectsplots)
		writeLines(paste(p@name, ":  ", p@description, "\n", sep=""))
	writeLines("-----------------------\n")
}

# write plots to file
writePlots<-function() {
	if(!file.exists("plots"))
		dir.create("plots")
	for(p in ectsplots) {
		print(paste("writing: ", p@name, sep=""))
		png(paste("plots", .Platform$file.sep, p@name, ".png", sep=""), 600, 600)
		p@f()
		dev.off()
	}
	writeLines("done.")
}
## ------------ ##


## ----  print instructions ---- ##
writeLines("
---- Instructions: ----
df: provides access to a dataframe wich contains the whole data

modules: provides a list of all modulenames in case You want to iterate over them (for instance with for(m in modules){...})

getUnionForAllModules(name): returns the unification of all columns where a modulename is followed by \"name\". For instance getUnionForAllModules(\"GesamterWorkload\") returns the data of <X>GesamterWorkload for every module X in a single vector.

getSubsetForAllModules(name): returns a subset of df with all columns where a modulename is followed by \"name\". For instance getSubsetForAllModules(\"GesamterWorkload\") returns a dataframe with every \"GesamterWorkload\"-column of df.

plots(): show a list of implemented plots. To plot a plot p write ectsplot.plot(p)

writePlots(): write every implemented plot to a file

If You want to start playing around with the data, try:
barplot(colMeans(getSubsetForAllModules(\"GesamterWorkload\"), na.rm=TRUE), las=2)
plot(getUnionForAllModules(\"WorkloadReliability\"))
summary(df)
plots()
-----------------------
")
## ------------ ##