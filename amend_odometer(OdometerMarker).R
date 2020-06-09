setwd("//ahmct-065/teams/PMRF/Amir")
library(data.table)
library(dplyr)
library(tidyr)

CHP.df=fread(file = "./bin/CHP_All.odom.csv", sep=",", header=TRUE)
CHP.df[CHP.df==""]=NA

RouteMarker.df=fread(file="./OdometerMarkers.csv")

RouteMarker.df=setDT(RouteMarker.df)[order(route), .(start=min(odometer), end=max(odometer)),
                                     by=.(route, county_code)]
RouteMarker.df=setDT(RouteMarker.df)[order(route, start)]
RouteMarker.df=RouteMarker.df[-which(RouteMarker.df$route==5 & RouteMarker.df$county_code==35.1230582),]

df84=as.data.frame(cbind.data.frame(route=c(84, 84, 84, 84),
                                    county_code=c("SM", "ALA", "SOL", "YOL"),
                                    start=c(0, 30.06, 57.238, 70.776),
                                    end=c(30.06, 57.238, 70.776, 86.463)))
df92=as.data.frame(cbind.data.frame(route=c(92, 92),
                                    county_code=c("SM", "ALA"),
                                    start=c(0, 19.527),
                                    end=c(19.527, 27.56)))
df805=as.data.frame(cbind.data.frame(route=c(805),
                                     county_code=c("SD"),
                                     start=c(0),
                                     end=c(28.725)))
df83=as.data.frame(cbind.data.frame(route=c(83),
                                    county_code=c("SBD"),
                                    start=c(0),
                                    end=c(10.918)))
df91=as.data.frame(cbind.data.frame(route=c(91, 91, 91),
                                    county_code=c("LA", "ORA", "RIV"),
                                    start=c(0, 14.739, 37.232),
                                    end=c(14.739, 37.232, 54.412)))
df880=as.data.frame(cbind.data.frame(route=c(880, 880),
                                     county_code=c("SCL", "ALA"),
                                     start=c(0, 10.502),
                                     end=c(10.502, 46.024)))
df79=as.data.frame(cbind.data.frame(route=c(79, 79),
                                    county_code=c("SD", "RIV"),
                                    start=c(2.703, 55.709),
                                    end=c(55.709, 95.611)))
df94=as.data.frame(cbind.data.frame(route=c(94),
                                    county_code=c("SD"),
                                    start=c(0),
                                    end=c(63.25)))
df8=as.data.frame(cbind.data.frame(route=c(8, 8),
                                   county_code=c("SD", "IMP"),
                                   start=c(1.977, 77.343),
                                   end=c(77.343, 171.891)))
df85=as.data.frame(cbind.data.frame(route=c(85),
                                    county_code=c("SCL"),
                                    start=c(0),
                                    end=c(24.235)))
df99=as.data.frame(cbind.data.frame(route=c(99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99),
                                    county_code=c("KER", "TUL", "FRE", "MAD", "MER", "STA", "SJ", "SAC", "SUT", "BUT", "TEH"),
                                    start=c(0.822, 58.441, 112.354, 143.949, 173.346, 210.788, 235.846, 274.629, 303.596, 344.777, 390.489),
                                    end=c(58.441, 112.354, 143.949, 173.346, 210.788, 235.846, 274.629, 303.596, 344.777, 390.489, 415.418)))
df80=as.data.frame(cbind.data.frame(route=c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80),
                                    county_code=c("SF", "ALA", "CC", "SOL", "NAP", "YOL", "SAC", "PLA", "NEV", "PLA", "NEV", "SIE"),
                                    start=c(0, 5.236, 13.352, 27.491, 34.305, 72.206, 83.924, 101.519, 160.214, 164.251, 171.143, 202.367),
                                    end=c(5.236, 13.352, 27.491, 72.206, 35.495, 83.924, 101.519, 160.214, 164.251, 171.143, 202.367, 203.96)))
df88=as.data.frame(cbind.data.frame(route=c(88, 88, 88),
                                    county_code=c("SJ", "AMA", "ALP"),
                                    start=c(0, 25.412, 96.412),
                                    end=c(25.412, 96.056, 121.916)))
df89=as.data.frame(cbind.data.frame(route=c(89, 89, 89, 89, 89, 89, 89, 89, 89, 89),
                                    county_code=c("MNO", "ALP", "ED", "PLA", "NEV", "SIE", "PLU", "TEH", "SHA", "SIS"),
                                    start=c(0, 7.596, 31.406, 58.802, 80.583, 89.209, 118.793, 160.935, 165.328, 208.658),
                                    end=c(7.596, 31.406, 58.802, 80.538, 89.209, 118.793, 160.935, 165.328, 208.658, 243.028)))
df87=as.data.frame(cbind.data.frame(route=c(87),
                                    county_code=c("SCL"),
                                    start=c(0),
                                    end=c(9.22)))
df82=as.data.frame(cbind.data.frame(route=c(82, 82, 82),
                                    county_code=c("SCL", "SM", "SF"),
                                    start=c(0, 16.636, 41.786),
                                    end=c(16.636, 41.786, 41.994)))
df90=as.data.frame(cbind.data.frame(route=c(90, 90),
                                    county_code=c("LA", "ORA"),
                                    start=c(0, 3.27),
                                    end=c(3.324, 12.117)))
df905=as.data.frame(cbind.data.frame(route=c(905),
                                     county_code=c("SD"),
                                     start=c(0),
                                     end=c(8.891)))
df86=as.data.frame(cbind.data.frame(route=c(86, 86),
                                    county_code=c("IMP", "RIV"),
                                    start=c(0, 68.805),
                                    end=c(68.805, 91.727)))
df9=as.data.frame(cbind.data.frame(route=c(9, 9, 9, 9),
                                   county_code=c("SCR", "SM", "SCR", "SCL"),
                                   start=c(0, 21.208, 24.705, 27.079),
                                   end=c(21.208, 24.705, 27.079, 38.528)))
df96=as.data.frame(cbind.data.frame(route=c(96, 96),
                                    county_code=c("HUM", "SIS"),
                                    start=c(0, 44.132),
                                    end=c(44.132, 146.519)))
df980=as.data.frame(cbind.data.frame(route=c(980),
                                     county_code=c("ALA"),
                                     start=c(0),
                                     end=c(2.027)))
df95=as.data.frame(cbind.data.frame(route=c(95, 95),
                                    county_code=c("RIV", "SBD"),
                                    start=c(0.39, 36.587),
                                    end=c(36.587, 116.915)))
df97=as.data.frame(cbind.data.frame(route=c(97),
                                    county_code=c("SIS"),
                                    start=c(0),
                                    end=c(54.364)))
df98=as.data.frame(cbind.data.frame(route=c(98),
                                    county_code=c("IMP"),
                                    start=c(0),
                                    end=c(57.295)))
df35=as.data.frame(cbind.data.frame(route=c(35, 35, 35, 35, 35, 35, 35, 35, 35),
                                    county_code=c("SCL", "SCR", "SCL", "SM", "SCL", "SM", "SCL", "SM", "SF"),
                                    start=c(0, 0.23, 7.706, 17.147, 18.387, 18.695, 18.797, 19.401, 50.030),
                                    end=c(0.23, 7.706, 17.147, 18.387, 18.695, 18.797, 19.401, 50.20, 53.180)))
df39=as.data.frame(cbind.data.frame(route=c(39, 39, 39, 39),
                                    county_code=c("ORA", "LA", "ORA", "LA"),
                                    start=c(0, 17.27, 17.755, 22.66),
                                    end=c(17.27, 17.755, 22.66, 50.016)))
df62=as.data.frame(cbind.data.frame(route=c(62, 62, 62, 62),
                                    county_code=c("RIV", "SBD", "RIV", "SBD"),
                                    start=c(0, 8.780, 88.15, 98.20),
                                    end=c(8.788, 88.15, 98.20, 151.43)))
df120=as.data.frame(cbind.data.frame(route=c(120, 120, 120, 120, 120, 120),
                                     county_code=c("SJ", "STA", "TUO", "MPA", "TUO", "MNO"),
                                     start=c(0, 21.380, 39.5, 80.470, 82.74, 94.9),
                                     end=c(21.370, 39.5, 80.47, 82.74, 94.95, 152.72)))
df150=as.data.frame(cbind.data.frame(route=c(150, 150, 150),
                                     county_code=c("SB", "VEN", "SB"),
                                     start=c(0, 1.110, 2.120),
                                     end=c(1.110, 2.120, 36.38)))
df166=as.data.frame(cbind.data.frame(route=c(166, 166, 166,
                                             166, 166, 166,
                                             166, 166, 166,
                                             166, 166, 166,
                                             166, 166, 166,
                                             166, 166, 166,
                                             166, 166, 166,
                                             166, 166),
                                     county_code=c("SB", "SLO", "SB",
                                                   "SLO", "SB", "SLO",
                                                   "SB", "SLO", "SB",
                                                   "SLO", "SB", "SLO",
                                                   "SB", "SLO", "SB",
                                                   "SLO", "SB", "SLO",
                                                   "SB", "SLO", "SB",
                                                   "SLO", "KER"),
                                     start=c(0, 8.930, 22.895, 30.118, 30.17, 30.400, 30.719, 30.92, 31.008,
                                             31.25, 31.382, 31.815, 32.027, 32.424, 32.578, 33.743, 33.836,
                                             33.836, 34.193, 34.987, 47.648, 66.606, 71.190),
                                     end=c(8.930, 22.895, 30.118, 30.17, 30.400, 30.719, 30.92, 31.005, 31.25,
                                           31.382, 31.815, 32.027, 32.424, 32.578, 33.743, 33.836, 34.037, 34.193,
                                           34.987, 47.648, 66.606, 71.181, 95.792)))
df180=as.data.frame(cbind.data.frame(route=c(180, 180, 180),
                                     county_code=c("FRE", "TUL", "FRE"),
                                     start=c(0, 85.872, 87.176),
                                     end=c(85.872, 86.176, 113.015)))
df210=as.data.frame(cbind.data.frame(route=c(210, 210),
                                     county_code=c("LA", "SBD"),
                                     start=c(0, 52.434),
                                     end=c(52.434, 85.599)))
df605=as.data.frame(cbind.data.frame(route=c(605, 605),
                                     county_code=c("ORA", "LA"),
                                     start=c(0, 2.050),
                                     end=c(2.050, 28.050)))
df78=as.data.frame(cbind.data.frame(route=c(78, 78, 78),
                                    county_code=c("SD", "IMP", "RIV"),
                                    start=c(0, 95.746, 181.473),
                                    end=c(95.746, 181.473, 197.885)))

RouteMarker.df=rbind.data.frame(RouteMarker.df, df120, df150, df166, df180, df210, df35, df39, df605, 
                                                df78, df79, df8, df80, df805, df82, df83, df84, df85, df86,
                                                df87, df88, df880, df89, df9, df90, df905, df91, df92,
                                                df94, df95, df96, df97, df98, df980, df99)
RouteMarker.df=setDT(RouteMarker.df)[order(route, start)]
fwrite(RouteMarker.df, file="RouteMarker.odometer.csv", sep=",", append = FALSE, row.names = TRUE)
#there still may be cases that need to be amended