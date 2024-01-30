.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}

var_labels <- c(
  "Männlich",
  "Körperliche Gesundheit SE",
  "Relative selbsteingeschätzte Gesundheit",
  "Arbeitsunfähige Tage im letzten Monat",
  "Hatte jemals Herzprobleme",
  "Herzinfarkt-Risiko SE",
  "Hatte jemals Herz-OP",
  "Nahestehender hatte Herz-OP",
  "Needs to be recoded",
  "Einnahme von Blutdruckmedikamenten",
  "Letzter Blutdruck N-H",
  "Hatte jemals Krebs",
  "Subjektives Krebsrisiko",
  "Alkoholhäufigkeit im konsumstärksten Jahr",
  "Anzahl der Drinks im konsumstärksten Jahr",
  "Sorgen machen",
  "Bildungsniveau",
  "Alter mit erster bezahlter Tätigkeit",
  "Jahr der Eheschließung",
  "Anzahl leiblicher Kinder",
  "Lebt bei den biologischen Eltern",
  "Aktuelle Gesundheit SE",
  "Lebenszufriedenheit",
  "Gesundheitliche Selbstwirksamkeit",
  "Anzahl verschreibungspflichtiger Medikamente",
  "Mittlere körperliche Symptome",
  "Limitierungen durch Gesundheit",
  "Moderate Aktivität pro Monat",
  "Fortgeschrittene Dyspnoe",
  "Körpergröße",
  "Aktuelles Gewicht",
  "Anzahl Hospitalisierungen im letzten Jahr",
  "In Selbsthilfegruppe gewesen (Substanzmissbrauch)",
  "Anzahl Alkoholkonsum höher als beabsichtigt letztes Jahr",
  "Mutter in den USA geboren",
  "Vater in den USA geboren",
  "Mittlere mütterliche Disziplin",
  "PWB Meisterung der Umwelt",
  "PWB Positive Beziehungen zu anderen",
  "PWB Lebenszweck",
  "Stereotypische Einstellungen zur Familie",
  "Rat suchend",
  "Durchsetzungsvermögen",
  "Finanzielle Lage SE",
  "Haushaltseinkommen",
  "Hauptverpflichtung",
  "Kontakt mit Nachbarn Häufigkeit",
  "Wohnt zur Miete (vs. Eigenheim)",
  "Besuch von Gottesdiensten Häufigkeit H-N",
  "Trost durch Religion suchen H-N",
  "Identifiziert sich mit der eigenen Ethnie H-N",
  "Verheiratet",
  "Alter bei Heirat",
  "Jemals 3+ Drinks pro Woche",
  "Alter zuletzt oft getrunken",
  "Derzeit beschäftigt",
  "Zum Wohlergehen anderer beitragen SE H-N",
  "Kontrolle über die Gesundheit",
  "Somatische Amplifikation",
  "Selbstwahrnehmung: Über- bis Untergewicht",
  "PWB Autonomie",
  "Persönliche Meisterschaft",
  "Gewissenhaftigkeit Persönlichkeitsmerkmal",
  "Offenheit Persönlichkeitsmerkmal",
  "Bewertung der aktuellen Arbeitssituation",
  "Mittlerer Stress zu Hause",
  "Mittlere Belohnungen am Arbeitsplatz",
  "Einkommen des Befragten und Partners",
  "Unterstützung der Familie / Freunde (Std./Mon.)",
  "Soziale Integration (soziales Wohlbefinden)",
  "Wahrgenommene Qualität der Nachbarschaft",
  "Wahrgenommene Ungleichheit im Haushalt",
  "Kontakt mit Freunden (Häufigkeit)",
  "Freundschaft - affektive Solidarität",
  "Entscheidungen aufgrund religiöser Überzeugungen",
  "Andere Sprache",
  "Alter bei Befragung",
  
  "Subjektives Herzinfarktrisiko",
  "Brustschmerzen bergauf/schnell gehen",
  "Gesundheitliche Kontrollüberzeugung - Andere",
  "Anzahl der chronischen Erkrankungen (12 Monate)",
  "Häufigkeit der Arztbesuche (12 Monate)",
  "Gesundheit der Mutter H-N",
  "Eigene Anstrengung für die Arbeit",
  "Sozialer Beitrag (soziales Wohlergehen)",
  "Summe der Treffen der sozialen Gruppe",
  "Affektive Solidarität in der Familie",
  "Belastung durch die Familie",
  "Belastung durch Freunde",
  
  "Summe konsumierter Ergänzungsmittel",
  "Anzahl biologischer Familienmitglieder mit Herzinfarkt",
  "Wie viel Sorge ums Herz",
  "Krebsrisiko im Vergleich zu anderen N-H",
  "Anzahl der Ehen insgesamt",
  "Lebenszufriedenheit gegenwärtig H-N",
  "Regeln für die Zeitgestaltung H-N",
  "Vorausschauende Planung und Antizipation",
  "Gespräch/Treffen mit dem Nachbarn (Häufigkeit) H-N",
  "Kontakt mit Familienmitgliedern (Häufigkeit) H-N",
  "Sexuelle Zufriedenheit gegenwärtig",
  "Lebenszufriedenheit gegenwärtig",
  
  "Alter beim ersten Alkoholkonsum",
  "Arztbesuche für psychische Gesundheit (12 Monate)",
  "Schwierigkeit monatliche Rechnungen zu bezahlen H-N",
  "Akzeptanz gegenüber anderen (soziales Wohlbefinden)",
  
  "Loyola-Generativitätsskala",
  
  "Diskriminierung über die Lebenszeit",
  "Bemühungen zur finanziellen Situation",
  "Gefühl der Verantwortung / Altruismus",
  
  "Arbeitspflicht - 5-Faktoren-Modell",
  "Positive Neubewertung (sekundäre Kontrolle)"
  
  
  
  # "Male",
  # "Physical Health SE",
  # "Health Compared SE",
  # "Days Off Work Last Month",
  # "Ever Had Heart Troubles",
  # "Heart Attack Risk SE",
  # "Ever Had Major Heart Procedure",
  # "Someone Close had Major Heart Procedure",
  # "Angina Class Rating L-H",
  # "Taking Blood Pressure Medication",
  # "Last Blood Pressure L-H",
  # "Ever Had Cancer",
  # "Subjective Cancer Risk",
  # "Alcohol Frequency in Worst Year",
  # "Number of Drinks in Worst Year",
  # "Worrying",
  # "Education",
  # "Age with First Payed Job",
  # "Year Married",
  # "Number of Biological Children",
  # "Lives with Biological Parents",
  # "Current Health SE",
  # "Life Satisfaction",
  # "Health Self-efficacy",
  # "Number of Prescription Medicines Taking",
  # "Mean Physical Symptoms",
  # "Mean Health Limits",
  # "Moderate Activity per Month",
  # "Progressive levels of dyspnea",
  # "Height",
  # "Current Weight",
  # "Times Hispitalized last Year",
  # "Ever Attended Substance Problem Group",
  # "Times Alcohol Consume Higher than Intendet Last Year",
  # "Mother born in US",
  # "Father born in US",
  # "Mean Maternal Discipline",
  # "PWB Environmental mastery",
  # "PWB Positive Relations With Others",
  # "PWB Purpose in Life",
  # "Mean Stereotypical Attitudes Family",
  # "Advice Seeking",
  # "Agency Personality Trait",
  # "Financial Situation SE",
  # "Household Total Income",
  # "Primary Obligation",
  # "Contact With Neighbors Frequency",
  # "Owns Home to Rent",
  # "Attends Religious Services Frequency H-L",
  # "Seek Comfort through Religion H-L",
  # "Identifies with own Race H-L",
  # "Is Married"
)
names(var_labels) <- c(
  "a1prsex1",
  "a1pa4",
  "a1pa6",
  "a1pa7",
  "a1pa111",
  "a1pa13",
  "a1pa161",
  "a1pa171",
  "a1pangin",
  "a1pa331",
  "a1pa31",
  "a1pa361",
  "a1pcacrs",
  "a1pa54",
  "a1pa55",
  "a1pa80",
  "a1peducp",
  "a1pb2",
  "a1pb18yr",
  "a1pb35",
  "a1pc11",
  "a1sa1",
  "a1ssatis",
  "a1shlocs",
  "a1srxmed",
  "mean_physical_symptoms",
  "mean_health_limits",
  "a1smoder",
  "a1sdyspn",
  "a1sa25",
  "a1sa27",
  "a1sa33",
  "a1sa38a1",
  "a1sa45",
  "a1se31",
  "a1se41",
  "mean_maternal_discipline",
  "a1spwbe",
  "a1spwbr",
  "a1spwbu",
  "mean_stereotypical_attitudes_family",
  "a1sadvic",
  "a1sagenc",
  "a1sj1",
  "a1shhtot",
  "a1spriob",
  "a1sl1",
  "a1sl4",
  "a1sr4",
  "a1sr5",
  "a1ss8",
  "married1",
  "married_age",
  "a1pa53",
  "a1pa56",
  "a1pb3e1",
  "a1pd8",
  "a1sa4",
  "a1samoli",
  "a1sa26",
  "a1spwba",
  "a1smaste",
  "a1scons",
  "a1sopen",
  "a1si2",
  "mean_stress_at_home",
  "mean_rewards_at_work",
  "a1shwearn",
  "a1spsupi",
  "a1sswbsi",
  "a1shomet",
  "a1spihom",
  "a1sm10",
  "a1sfdsol",
  "a1sr6",
  "other_language1",
  "a1page_m2",
  
  "a1phrtrs",
  "a1pa231",
  "a1shloco",
  "a1schron",
  "a1susemd",
  "a1sd1",
  "a1si6",
  "a1sswbsc",
  "sum_social_group_meetings",
  "a1sfamso",
  "a1skinne",
  "a1sfdsne",
  
  "sum_supplements_consumed",
  "a1pa14",
  "a1pa15",
  "a1pa37",
  "a1pb19",
  "a1pd1",
  "a1se12",
  "a1sforsg",
  "a1sl2",
  "a1sm1",
  "a1sq1",
  "a1st1",
  
  "a1pa52",
  "a1sa37b",
  "a1sj7",
  "a1sswbao",
  
  "a1sgener",
  
  "a1slfedi",
  "a1sj5",
  "a1saltru",
  
  "a1swkob",
  "a1sreapp"
  
  
  # "a1prsex1",
  # "a1pa4",
  # "a1pa6",
  # "a1pa7",
  # "a1pa111",
  # "a1pa13",
  # "a1pa161",
  # "a1pa171",
  # "a1pangin",
  # "a1pa331",
  # "a1pa31",
  # "a1pa361",
  # "a1pcacrs",
  # "a1pa54",
  # "a1pa55",
  # "a1pa80",
  # "a1peducp",
  # "a1pb2",
  # "a1pb18yr",
  # "a1pb35",
  # "a1pc11",
  # "a1sa1",
  # "a1ssatis",
  # "a1shlocs",
  # "a1srxmed",
  # "mean_physical_symptoms",
  # "mean_health_limits",
  # "a1smoder",
  # "a1sdyspn",
  # "a1sa25",
  # "a1sa27",
  # "a1sa33",
  # "a1sa38a1",
  # "a1sa45",
  # "a1se31",
  # "a1se41",
  # "mean_maternal_discipline",
  # "a1spwbe",
  # "a1spwbr",
  # "a1spwbu",
  # "mean_stereotypical_attitudes_family",
  # "a1sadvic",
  # "a1sagenc",
  # "a1sj1",
  # "a1shhtot",
  # "a1spriob",
  # "a1sl1",
  # "a1sl4",
  # "a1sr4",
  # "a1sr5",
  # "a1ss8",
  # "married1",
  # "married_age",
  # "a1pa53",
  # "a1pa56",
  # "a1pb3e1",
  # "a1pd8",
  # "a1sa4",
  # "a1samoli",
  # "a1sa26",
  # "a1spwba",
  # "a1smaste",
  # "a1scons",
  # "a1sopen",
  # "a1si2",
  # "mean_stress_at_home",
  # "mean_rewards_at_work",
  # "a1shwearn",
  # "a1spsupi",
  # "a1sswbsi",
  # "a1shomet",
  # "a1spihom",
  # "a1sm10",
  # "a1sfdsol",
  # "a1sr6",
  # "other_language1",
  # "A1PAGE_M2"
)


ggforest_custom <- function(model, data = NULL,
                            main = "Hazard ratio", cpositions=c(0.02, 0.22, 0.4),
                            fontsize = 0.7, refLabel = "reference", noDigits=2) {
  conf.high <- conf.low <- estimate <- NULL
  stopifnot(inherits(model, "coxph"))
  
  # get data and variables/terms from cox model
  data  <- .get_data(model, data = data)
  terms <- attr(model$terms, "dataClasses")[-1]
  # removed as requested in #388
  #  terms <- terms[intersect(names(terms),
  #    gsub(rownames(anova(model))[-1], pattern = "`", replacement = ""))]
  
  # use broom to get some required statistics
  coef <- as.data.frame(tidy(model, conf.int = TRUE))
  gmodel <- glance(model)
  
  # extract statistics for every variable
  allTerms <- lapply(seq_along(terms), function(i){
    var <- names(terms)[i]
    if (terms[i] %in% c("factor", "character")) {
      adf <- as.data.frame(table(data[, var]))
      cbind(var = var, adf, pos = 1:nrow(adf))
    }
    else if (terms[i] == "numeric") {
      data.frame(var = var, Var1 = "", Freq = nrow(data),
                 pos = 1)
    }
    else {
      vars = grep(paste0("^", var, "*."), coef$term, value=TRUE)
      data.frame(var = vars, Var1 = "", Freq = nrow(data),
                 pos = seq_along(vars))
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  inds <- apply(allTermsDF[,1:2], 1, paste0, collapse="")
  
  # use broom again to get remaining required statistics
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  toShow <- cbind(allTermsDF, coef[inds,])[,c("var", "level", "p.value", "estimate", "conf.low", "conf.high", "pos")]
  toShowExp <- toShow[,4:6]
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits=noDigits)
  toShowExpClean <- data.frame(toShow,
                               pvalue = signif(toShow[,3],noDigits+1),
                               toShowExp)
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, noDigits+1), " ",
                                 ifelse(toShowExpClean$p.value < 0.05, "*",""),
                                 ifelse(toShowExpClean$p.value < 0.01, "*",""),
                                 ifelse(toShowExpClean$p.value < 0.001, "*",""))
  toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
  toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
  toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
  toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
  
  varNames  <- as.character(toShowExpClean$var)
  varLabels <- var_labels[varNames]
  varNames  <- ifelse(is.na(varLabels), varNames, varLabels)
  unname(varNames)
  
  toShowExpClean$var = varNames
  toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
  # make label strings:
  # toShowExpClean$N <- paste0("(N=",toShowExpClean$N,")")
  
  #flip order
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]
  
  rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  # make plot twice as wide as needed to create space for annotations
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # increase white space on right for p-vals:
  rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)
  
  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_nlevel <- rangeplot[1]  +  cpositions[2] * width
  y_cistring <- rangeplot[1]  +  cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(toShowExpClean))
  
  # geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
  annot_size_mm <- fontsize *
    as.numeric(convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  p <- ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(var) - .5, xmax = seq_along(var) + .5,
                  ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                  fill = ordered(seq_along(var) %% 2 + 1))) +
    scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    geom_point(pch = 15, size = 4) +
    geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
    geom_hline(yintercept = 1, linetype = 3) +
    coord_flip(ylim = exp(rangeplot)) +
    ggtitle(main) +
    scale_y_log10(
      name = "",
      labels = sprintf("%g", breaks),
      expand = c(0.02, 0.02),
      breaks = breaks) +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.border=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    annotate(geom = "text", x = x_annotate, y = exp(y_variable),
             label = toShowExpClean$var, fontface = "bold", hjust = 0,
             size = annot_size_mm) +
    # annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
    #          label = toShowExpClean$level, vjust = -0.1, size = annot_size_mm) +
    # annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
    #          label = toShowExpClean$N, fontface = "italic", hjust = 0,
    #          vjust = ifelse(toShowExpClean$level == "", .5, 1.1),
    #          size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
             label = toShowExpClean$estimate.1, size = annot_size_mm,
             vjust = ifelse(toShowExpClean$estimate.1 == "reference", .5, -0.1)) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
             label = toShowExpClean$ci, size = annot_size_mm,
             vjust = 1.1,  fontface = "italic") +
    annotate(geom = "text", x = x_annotate, y = exp(y_stars),
             label = toShowExpClean$stars, size = annot_size_mm,
             hjust = -0.2,  fontface = "italic") +
    annotate(geom = "text", x = 0.5, y = exp(y_variable),
             label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ",
                            format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC,2),
                            "; Concordance Index: ", round(gmodel$concordance,2)),
             size = annot_size_mm, hjust = 0, vjust = 1.2,  fontface = "italic")
  # switch off clipping for p-vals, bottom annotation:
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  # invisible(p)
  ggpubr::as_ggplot(gt)
}