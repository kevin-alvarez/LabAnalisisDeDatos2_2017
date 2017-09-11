#Nostros guardamos las tablas en un archivo txt donde se separan las columnas por
#comas, luego usamos read.table.
#file.choose() nos permite buscar el archivo en vez de escribir su ruta
#sep = "," es necesario para que R entienda que las columnas están separadas por
#comas
library(cluster)
library(ggplot2)
library(magrittr)
library(Rtsne)
library(dplyr) # for data cleaning
library(ISLR) # for college dataset

Estudio<-read.table(file.choose(),header = T,sep = ",")

# Se realiza una limpieza de datos.
keeps <- c("age","sex","on.thyroxine","on.antithyroid.medication","pregnant","thyroid.surgery","I131.treatment","lithium","goitre","tumor","hypopituitary","psych","TSH","T3","TT4","T4U")
EstudioLimpiado <- Estudio[ , keeps, drop = FALSE]
columnas <- colnames(EstudioLimpiado)

# Permite calcular los indices a eliminar, pero por alguna razón no pude usar el vector de indices, por lo que
# tuvimos que construirlos manualmente.
for(row in 1:nrow(EstudioLimpiado)){
  for (col in 1:length(EstudioLimpiado)){
    if( EstudioLimpiado[row, col] == "?"){
      myvector = append(myvector, row)
      print(row)
      break
    }
  }
}

indicesAEliminar <- c(2,3,4,6,7,12,16,17,21,24,28,30,36,39,40,48,55,57,62,65,67,70,71,72,73,74,75,77,81,82,84,86,87,93,96,97,99,104,105,107,108,111,116,119,120,122,127,131,133,137,138,141,151,155,156,157,158,170,179,185,189,195,197,198,205,208,209,211,212,215,218,219,220,221,222,226,230,232,240,249,251,255,256,257,261,262,263,269,275,277,281,286,292,298,300,304,308,315,321,328,330,337,340,342,343,355,365,368,369,370,371,377,380,385,386,388,396,397,401,403,412,424,425,432,439,443,444,446,448,457,459,464,465,476,478,481,485,488,491,492,496,497,500,505,506,513,518,519,521,523,524,527,529,531,535,539,541,545,552,555,566,567,570,576,577,581,591,593,596,599,601,604,607,609,612,613,622,623,626,629,632,633,634,636,637,650,655,658,659,662,667,671,672,673,674,675,676,678,681,686,688,691,694,697,701,703,709,711,716,718,727,733,735,736,738,739,746,756,760,764,765,766,772,774,777,788,789,792,793,795,796,810,812,813,820,821,823,825,826,827,829,831,847,849,855,856,857,865,868,874,875,877,884,895,896,899,905,911,913,926,927,930,934,936,939,940,942,943,944,947,948,949,950,952,953,956,957,958,961,963,965,966,968,969,978,979,981,986,999,1000,1001,1007,1012,1013,1016,1017,1018,1019,1020,1024,1026,1027,1030,1033,1034,1043,1046,1048,1051,1052,1055,1057,1067,1071,1075,1078,1088,1091,1092,1095,1105,1108,1113,1120,1130,1131,1136,1137,1138,1139,1140,1143,1144,1146,1147,1156,1157,1158,1159,1164,1165,1166,1173,1175,1176,1180,1182,1183,1189,1199,1203,1207,1209,1210,1213,1218,1219,1221,1222,1228,1231,1232,1233,1236,1238,1240,1241,1242,1246,1247,1250,1251,1261,1262,1267,1274,1286,1296,1298,1299,1301,1304,1306,1309,1310,1312,1315,1320,1329,1334,1336,1339,1340,1344,1350,1351,1357,1362,1363,1367,1370,1374,1380,1386,1387,1393,1395,1397,1398,1400,1403,1410,1414,1416,1419,1421,1428,1429,1431,1434,1436,1438,1442,1444,1453,1455,1461,1462,1467,1468,1471,1475,1482,1485,1488,1491,1497,1501,1505,1506,1509,1512,1514,1517,1519,1522,1525,1528,1532,1533,1535,1539,1542,1544,1548,1555,1556,1557,1578,1580,1586,1589,1592,1593,1596,1600,1602,1603,1604,1607,1610,1614,1618,1620,1624,1625,1626,1629,1631,1632,1633,1635,1645,1649,1650,1654,1655,1660,1662,1664,1666,1679,1680,1681,1683,1686,1688,1692,1693,1698,1699,1700,1704,1705,1707,1708,1712,1716,1718,1719,1721,1729,1730,1731,1732,1734,1736,1744,1745,1746,1747,1749,1756,1763,1764,1766,1768,1769,1779,1781,1782,1785,1786,1790,1792,1795,1796,1797,1807,1813,1823,1826,1827,1829,1835,1837,1839,1842,1843,1844,1848,1852,1857,1858,1859,1862,1864,1867,1868,1869,1873,1876,1878,1879,1883,1886,1891,1893,1897,1898,1904,1905,1907,1912,1920,1924,1930,1931,1935,1941,1942,1943,1949,1951,1956,1959,1963,1965,1972,1976,1979,1981,1982,1986,1988,1991,1997,2002,2007,2008,2009,2010,2011,2013,2015,2018,2021,2025,2027,2031,2033,2034,2038,2042,2043,2044,2048,2060,2063,2069,2072,2074,2082,2088,2090,2094,2095,2099,2100,2102,2103,2104,2115,2122,2125,2130,2137,2138,2142,2143,2144,2146,2156,2159,2165,2166,2170,2171,2172,2174,2175,2180,2181,2187,2189,2190,2192,2201,2202,2208,2211,2214,2215,2223,2227,2229,2233,2234,2236,2244,2247,2248,2253,2254,2255,2259,2261,2262,2264,2273,2274,2278,2282,2284,2285,2287,2288,2289,2290,2293,2301,2302,2304,2306,2316,2325,2326,2331,2332,2333,2335,2336,2337,2339,2340,2341,2344,2348,2352,2353,2356,2359,2360,2372,2373,2374,2378,2380,2381,2385,2396,2399,2400,2401,2404,2407,2409,2410,2411,2420,2421,2424,2428,2430,2435,2438,2443,2446,2447,2449,2452,2466,2468,2470,2471,2475,2477,2485,2488,2490,2492,2498,2500,2501,2503,2508,2509,2511,2512,2516,2527,2528,2531,2536,2541,2546,2547,2551,2552,2559,2566,2571,2575,2577,2579,2583,2597,2601,2602,2606,2607,2613,2616,2620,2621,2624,2626,2628,2630,2641,2643,2645,2647,2648,2649,2650,2651,2652,2655,2662,2663,2666,2667,2668,2670,2673,2681,2682,2683,2689,2691,2696,2704,2707,2712,2721,2726,2729,2734,2740,2741,2743,2744,2746,2747,2752,2753,2754,2758,2761,2763,2765,2766,2767,2771,2776,2778,2780,2781,2782,2784,2785,2787,2788,2790,2792,2793,2796,2797,2798,2799)
EstudioLimpiado <- EstudioLimpiado[-indicesAEliminar, ,drop=F]

TSHMAX <- 4.70 + abs(0.5 - 4.70) * 3
T3MAX <-  2.8 + abs(0.9 - 2.8) * 3
TT4MAX <- 161 + abs(58-161) * 3
T4UMAX <- 1.3 + abs(0.8 - 1.3) * 3

EstudioLimpiado <- subset(EstudioLimpiado, (as.numeric(as.character(TSH)) <= TSHMAX) & (as.numeric(as.character(T3)) <= T3MAX) & (as.numeric(as.character(TT4)) <= TT4MAX) & (as.numeric(as.character(T4U)) <= T4UMAX))
Estudio2 <- EstudioLimpiado
Estudio2$age <- as.numeric(as.character(Estudio2$age))
Estudio2$on.thyroxine <- (Estudio2$on.thyroxine == "t")
Estudio2$on.antithyroid.medication <- (Estudio2$on.antithyroid.medication == "t")
Estudio2$pregnant <- (Estudio2$pregnant == "t")
Estudio2$thyroid.surgery <- (Estudio2$thyroid.surgery == "t")
Estudio2$I131.treatment <- (Estudio2$I131.treatment == "t")
Estudio2$lithium <- (Estudio2$lithium == "t")
Estudio2$goitre <- (Estudio2$goitre == "t")
Estudio2$tumor <- (Estudio2$tumor == "t")
Estudio2$hypopituitary <- (Estudio2$hypopituitary == "t")
Estudio2$psych <- (Estudio2$psych == "t")
Estudio2$TSH <- as.numeric(as.character(Estudio2$TSH))
Estudio2$T3 <- as.numeric(as.character(Estudio2$T3))
Estudio2$TT4 <- as.numeric(as.character(Estudio2$TT4))
Estudio2$T4U <- as.numeric(as.character(Estudio2$T4U))

# Matriz de distancia gower
gower_dist <- daisy(Estudio2[, -1],
                    metric = "gower",
                    type = list(logratio = 3))


sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}


# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

##########

clust4 <- pam(gower_dist, diss = TRUE, k = 4)
clust5 <- pam(gower_dist, diss = TRUE, k = 5)

#clustPlot(clust4, c(1,2,3,4), diss=FALSE)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clust4$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



nrow(EstudioLimpiado)
nrow(subset(EstudioLimpiado, diagnostico == "hyperthyroid."))
