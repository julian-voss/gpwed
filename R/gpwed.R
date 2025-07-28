#' German Post-War Election Database (GPWED), 1949–1969
#'
#' This dataset provides harmonized municipal-level election results for West Germany's first six federal elections (1949–1969).
#'
#' @format A data frame with 94424 rows and 43 variables:
#' \describe{
#'   \item{key_state}{State-level identifier}
#'   \item{ags}{Official municipality code (Amtlicher Gemeindeschlüssel)}
#'   \item{municipality_name}{Name of the municipality}
#'   \item{independent_city}{Logical indicator for urban districts (kreisfreie Städte)}
#'   \item{year}{Election year (1949, 1953, 1957, 1961, 1965, 1969)}
#'   \item{vote_type}{Type of vote: "party" or "candidate"}
#'   \item{share_imputed}{Proportion of values imputed for this row (0–1)}
#'   \item{voters_eligible}{Number of eligible voters}
#'   \item{votes_valid}{Number of valid votes}
#'   \item{spd}{Sozialdemokratische Partei Deutschlands (SPD)}
#'   \item{cdu_csu}{Christlich Demokratische Union Deutschlands / Christlich-Soziale Union in Bayern (CDU/CSU)}
#'   \item{dp}{Deutsche Partei (DP)}
#'   \item{fdp}{Freie Demokratische Partei (FDP)}
#'   \item{kpd}{Kommunistische Partei Deutschlands (KPD)}
#'   \item{drp}{Deutsche Konservative Partei – Deutsche Rechtspartei / Deutsche Reichspartei (DRP)}
#'   \item{rsf}{Radikal-Soziale Freiheitspartei (RSF)}
#'   \item{rvwp}{Rheinisch-Westfälische Volkspartei (RVwP)}
#'   \item{ssw}{Südschleswigscher Wählerverband (SSW)}
#'   \item{gb_bhe}{Gesamtdeutscher Block / Bund der Heimatvertriebenen und Entrechteten (GB/BHE)}
#'   \item{gvp}{Gesamtdeutsche Volkspartei (GVP)}
#'   \item{pdgd_dns}{Partei der guten Deutschen / Dachverband der Nationalen Sammlung (PDGD/DNS)}
#'   \item{bdd}{Bund der Deutschen (BdD)}
#'   \item{dg}{Deutsche Gemeinschaft (DG)}
#'   \item{dms}{Deutscher Mittelstand (DMS)}
#'   \item{dfu}{Deutsche Friedens-Union (DFU)}
#'   \item{npd}{Nationaldemokratische Partei Deutschlands (NPD)}
#'   \item{aud}{Aktionsgemeinschaft Unabhängiger Deutscher (AUD)}
#'   \item{fsu}{Frei-Soziale Union (FSU)}
#'   \item{cvp}{Christlichdemokratische Volkspartei (CVP)}
#'   \item{uap}{Unabhängige Arbeiter-Partei (UAP)}
#'   \item{adf}{Aktion Demokratischer Fortschritt (ADF)}
#'   \item{ep}{Europa Partei / Europäische Föderalistische Partei (EP)}
#'   \item{gpd}{Gesamtdeutsche Partei / Gesamtdeutsche Partei Deutschlands (GPD)}
#'   \item{bp}{Bayernpartei (BP)}
#'   \item{wav}{Wirtschaftliche Aufbau-Vereinigung (WAV)}
#'   \item{vu}{Vaterländische Union (VU)}
#'   \item{ap}{Arbeiterpartei (AP)}
#'   \item{center_fu}{Deutsche Zentrumspartei / Föderalistische Union (Center/FU)}
#'   \item{other}{Other parties and independent candidates}
#'   \item{cdu_county_deviation}{County-level deviation for CDU/CSU > 1 p.p.}
#'   \item{spd_county_deviation}{County-level deviation for SPD > 1 p.p.}
#'   \item{fdp_county_deviation}{County-level deviation FDP > 1 p.p.}
#'   \item{mail_votes}{Vote counts include mail-in voting}
#' }
#' @source Voss, Julian. German Post-War Election Database (GPWED).
#' @usage data(gpwed)
"gpwed"
