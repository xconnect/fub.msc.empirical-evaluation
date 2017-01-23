## ---- global declarations ---- ##
ectsplots<<-NULL
setClass("ECTSPlot", slots=c(name="character", description="character", f="function"))
ECTSPlot<-function(name, description, f) new("ECTSPlot", name=name, description=description, f=f)
ectsplot.plot<-function(x) {
	x11()
	x@f()
}
ectsplot.new<-function(name, description, f) {
	assign(name, ECTSPlot(name, description, f), envir=.GlobalEnv)
	ectsplots<<-c(ectsplots, get(name))
}
## ------------ ##


## ---- plots ---- ##

## template for new plots:
## ectsplot.new("name",
## 	"description",
## 	function() {
## 		# body	
## })

ectsplot.new("wlperlpComp",
	"Workload per LP. Values according to GesamterWorkload and WorkloadCalc (not scaled).",
	function() {
		barplot(rbind(colMeans(countToName(getSubsetForAllModules("WLperLP")), na.rm=TRUE),
			colMeans(countToName(getSubsetForAllModules("WLperLPCalc")), na.rm=TRUE)),
			las=2, col=c("blue", "green"), main="Arbeitsstunden pro LP", beside=TRUE, ylim=c(0, 50))
		legend("topright", c("gesamt", "berechnet"), col=c("blue", "green"), pch=15)
		text(x=25, y=32, "Richtlinie nach ECTS", col="brown1", font=2)
		abline(h=30, lty="dashed", col="brown1")
})

ectsplot.new("wlperlp",
	"Workload per LP. Values according to GesamterWorkload are scaled by weeksPerSem and lpPerModule. Rows with low reliability are taken as NA.",
	function() {
		d<-countToName(getSubsetForAllModules("WLperLP"))
		barplot(colMeans(d, na.rm=TRUE),
			las=2, col="cadetblue1", main="Arbeitsstunden pro LP")
		text(x=10, y=32, "Richtlinie nach ECTS", col="brown1", font=2)
		abline(h=30, lty="dashed", col="brown1")
		par(new = TRUE)
		plot(seq(0.5, length(d)-0.5, 1), apply(d, MARGIN=2, FUN=function(x) length(which(!is.na(x)))),
			type="l", axes=FALSE, xlab="", ylab="", ylim=c(0, 100), xlim=c(0, 18))
})

ectsplot.new("wlperlpCalc",
	"Workload per LP. Values according to computed workload are scaled by weeksPerSem and lpPerModule. Rows with low reliability are taken as NA.",
	function() {
		d<-countToName(getSubsetForAllModules("WLperLPCalc"))
		barplot(colMeans(d, na.rm=TRUE),
			las=2, col="cadetblue1", main="Arbeitsstunden pro LP")
		text(x=10, y=32, "Richtlinie nach ECTS", col="brown1", font=2)
		abline(h=30, lty="dashed", col="brown1")
		par(new = TRUE)
		plot(seq(0.5, length(d)-0.5, 1), apply(d, MARGIN=2, FUN=function(x) length(which(!is.na(x)))),
			type="l", axes=FALSE, xlab="", ylab="", ylim=c(0, 100), xlim=c(0, 18))
})

ectsplot.new("wlperlpCombined",
	"Workload per LP. Values according to GesamterWorkload and WorkloadCalc (not scaled).",
	function() {
		d<-countToName(rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc")))
		boxplot(d,
			las=2, ylim=c(0, 60), col="cadetblue1", main="Arbeitsstunden pro LP")
		text(x=10, y=32, "Richtlinie nach ECTS", col="brown1", font=2)
		abline(h=30, lty="dashed", col="brown1")
		points(seq(d), colMeans(d, na.rm=TRUE), pch=23, bg="red")
})

ectsplot.new("rel",
	"Reliability of questioned workload. -1: total wl is less then computed, 0: values are matching, 1: total wl is higher then computed.",
	function() {
		plot(getUnionForAllModules("WorkloadReliability"),
			main="Diskrepanz zwischen totalem und berechnetem Workload", xlab="Datenpunkte für Workload", ylab="Abweichung", xaxt='n')
		abline(h=0)
		abline(h=-0.5, lty="dashed")
		abline(h=0.5, lty="dashed")
		abline(h=-0.7, lty="dashed")
		abline(h=0.7, lty="dashed")
})

ectsplot.new("skills",
	"Prior knowledge compared to total workload.",
	function() {
		boxplot(getUnionForAllModules("WLperLP") ~ countToLevel(as.factor(getUnionForAllModules("Vorkenntnisse"))),
			ylim=c(0, 50), main="Angegebener Workload nach Vorkenntnissen", col="cadetblue1",
			xlab="Vorkenntnisse", ylab="Gesamter Workload")
})

ectsplot.new("concern",
	"Concern compared to total workload.",
	function() {
		boxplot(getUnionForAllModules("WLperLP") ~ countToLevel(as.factor(getUnionForAllModules("Interesse"))),
			ylim=c(0, 50), main="Angegebener Workload nach Interesse", col="cadetblue1",
			xlab="Interesse", ylab="Gesamter Workload")
})

ectsplot.new("concernGrouped",
	"Concern compared to total workload. Grouped.",
	function() {
		d<-getUnionForAllModules("Interesse")
		d[d==2] = 1
		d[d==3] = 2
		d[d==4] = 2
		d[d==5] = 2
		d[d==6] = 3
		d[d==7] = 3
		boxplot(getUnionForAllModules("WLperLP") ~ countToLevel(as.factor(d)),
			ylim=c(0, 50), main="Angegebener Workload nach Interesse", col="cadetblue1",
			xlab="Interesse", ylab="Gesamter Workload")
})

ectsplot.new("diff",
	"Difficulty compared to total workload.",
	function() {
		boxplot(getUnionForAllModules("WLperLP") ~ countToLevel(as.factor(getUnionForAllModules("Schwierigkeit"))),
			ylim=c(0, 50), main="Angegebener Workload nach Schwierigkeit", col="cadetblue1",
			xlab="Schwierigkeit", ylab="Gesamter Workload")
})

ectsplot.new("money",
	"Einkommen compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Einnahmen"]]), 2 * length(modules)),
			main="Angegebener Workload nach Einkommen", col="cadetblue1",
			xlab="Einkommen", ylab="Gesamter Workload")
})

ectsplot.new("health",
	"Gesundheit compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Gesundheitszustand"]]), 2 * length(modules)),
			main="Angegebener Workload nach Gesundheitszustand", col="cadetblue1",
			xlab="Gesundheitszustand", ylab="Gesamter Workload")
})

ectsplot.new("wohn",
	"Wohnsituation compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		par(oma=c(10, 0, 0, 0))
		boxplot(r ~ rep(countToLevel(df[["Wohnsituation"]]), 2 * length(modules)),
			main="Angegebener Workload nach Wohnsituation", col="cadetblue1",
			xlab=" ", ylab="Gesamter Workload", las=2)
		par(omi=c(0, 0, 0, 0))
})

ectsplot.new("kinder",
	"Kinder compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Kinder"]]), 2 * length(modules)),
			main="Angegebener Workload nach Anzahl Kinder", col="cadetblue1",
			xlab="Anzahl Kinder", ylab="Gesamter Workload")
})

ectsplot.new("kinderWOcount",
	"Kinder compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(df[["Kinder"]], 2 * length(modules)),
			main="Angegebener Workload nach Anzahl Kinder", col="cadetblue1",
			xlab="Anzahl Kinder", ylab="Gesamter Workload")
})

ectsplot.new("family",
	"Familienstand compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Familienstand"]]), 2 * length(modules)),
			main="Angegebener Workload nach Familienstand", col="cadetblue1",
			xlab="Familienstand", ylab="Gesamter Workload")
})

ectsplot.new("erststudium",
	"Erststudium compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Erststudium"]]), 2 * length(modules)),
			main="Angegebener Workload nach Erststudium", col="cadetblue1",
			xlab="Erststudium", ylab="Gesamter Workload")
})

ectsplot.new("vorwissen",
	"Vorwissen compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Vorwissen"]]), 2 * length(modules)),
			main="Angegebener Workload nach Vorbildung", col="cadetblue1",
			xlab="Vorbildung", ylab="Gesamter Workload")
})

ectsplot.new("gender",
	"Gender compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Geschlecht"]]), 2 * length(modules)),
			main="Angegebener Workload nach Geschlecht", col="cadetblue1",
			xlab="Geschlecht", ylab="Gesamter Workload")
})

ectsplot.new("computer",
	"Computerbesitz compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["eigenerComputer"]]), 2 * length(modules)),
			main="Angegebener Workload nach Computerbesitz", col="cadetblue1",
			xlab="Computerbesitz", ylab="Gesamter Workload")
})

ectsplot.new("sem",
	"Fachsemester compared to total workload.",
	function() {
		d<-rbind(getSubsetForAllModules("WLperLP"), getSubsetForAllModules("WLperLPCalc"))
		r<-NULL
		for(m in modules)
			r<-c(r, d[[m]])
		boxplot(r ~ rep(countToLevel(df[["Fachsemester"]]), 2 * length(modules)),
			main="Angegebener Workload nach Fachsemester", col="cadetblue1",
			xlab="Fachsemester", ylab="Gesamter Workload", las=2)
})

ectsplot.new("wlperlpCombinedBar",
	"Workload per LP. Values according to GesamterWorkload and WorkloadCalc (not scaled).",
	function() {
		d<-countToName(rbind(getSubsetForAllModules("WLperLP")))
		barplot(colMeans(d, na.rm=TRUE),
			las=2, ylim=c(0, 40), col="cadetblue1", main="Arbeitsstunden pro LP")
		text(x=10, y=32, "Richtlinie nach ECTS", col="brown1", font=2)
		abline(h=30, lty="dashed", col="brown1")
		par(new = TRUE)
		plot(seq(0.5, length(d)-0.5, 1), apply(d, MARGIN=2, FUN=function(x) length(which(!is.na(x)))),
			type="l", axes=FALSE, xlab="", ylab="", ylim=c(0, 100), xlim=c(0, 18))
})
## ------------ ##