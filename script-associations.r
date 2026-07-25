#---- Basic pre-processing ----

# Load environment: 
load("results_TFM.RData")

# Upload tables:
tabproc <- read.table("./tabla_procesada.csv", 
                      header = T, sep = "\t",
                      row.names = 1)

# tabdif <- read.table("./dif_universidad.txt", 
#                       header = T, sep = "\t")
# tabdif2 <- read.table("./dif_HM", 
#                      header = T, sep = "\t")
# tabdif3 <- read.table("./dif_universidadHM.txt", 
#                      header = T, sep = "\t")

# Reproducibility seed:
set.seed(1000)

# Add frequencies to rows:
tabproc$Freq <- rep(x = 1, 305)

# Fix levels of factors
tabproc$Pragmático <- factor(tabproc$Pragmático, 
                             levels = c("Muy alta", "Alta", "Moderada",
                                        "Baja", "Muy baja"))
tabproc$Reflexivo <- factor(tabproc$Reflexivo, 
                            levels = c("Muy alta", "Alta", "Moderada",
                                       "Baja", "Muy baja"))
tabproc$Activo <- factor(tabproc$Activo, 
                         levels = c("Muy alta", "Alta", "Moderada",
                                    "Baja", "Muy baja"))
tabproc$Teórico <- factor(tabproc$Teórico, 
                          levels = c("Muy alta", "Alta", "Moderada",
                                     "Baja", "Muy baja"))

#---- Contingency tables for Honey-Mumford learning styles ----

# Pragmatist's learning style and diversity variables:
ctg.0 <- table(tabproc$Universidad, tabproc$Pragmático)
ctg.1 <- table(tabproc$área_estudio, tabproc$Pragmático)
ctg.2 <- table(tabproc$nivel_académico, tabproc$Pragmático)# bajo*, pero sino queda 1x5
ctg.3 <- table(tabproc$etapa_estudio, tabproc$Pragmático)
ctg.4 <- table(tabproc$institución_escolar, tabproc$Pragmático)
ctg.5 <- table(tabproc$preuniversitario, tabproc$Pragmático)
ctg.6 <- table(tabproc$multi_carrera, tabproc$Pragmático)[-1, ]
ctg.7 <- table(tabproc$desempeño_anterior, tabproc$Pragmático)[-1, ]
ctg.8 <- table(tabproc$trabaja, tabproc$Pragmático)
ctg.9 <- table(tabproc$experiencia_laboral, tabproc$Pragmático)
ctg.10 <- table(tabproc$edad, tabproc$Pragmático)#rm-bajo* [-3, ]
ctg.11 <- table(tabproc$tecnológico, tabproc$Pragmático)
ctg.12 <- table(tabproc$origen_geo, tabproc$Pragmático)#rm-bajo* [-1, ]
ctg.13 <- table(tabproc$territorio, tabproc$Pragmático)
ctg.14 <- table(tabproc$movilidad, tabproc$Pragmático)
ctg.15 <- table(tabproc$lengua, tabproc$Pragmático)#rm-bajo* [-1, ]
ctg.16 <- table(tabproc$idiomas, tabproc$Pragmático)[-3, ]
ctg.17 <- table(tabproc$etnia, tabproc$Pragmático)#rm-bajo* [-c(1,3,6), ]
ctg.18 <- table(tabproc$religión, tabproc$Pragmático)[-c(1), ]#rm-bajo* :3,6,10:15,17,18
ctg.19 <- table(tabproc$nivel_socioeconómico, tabproc$Pragmático)[-c(1), ]#rm-bajo* ,2
ctg.20 <- table(tabproc$clase_social, tabproc$Pragmático)[-c(1), ]#rm-bajo* ,2
ctg.21 <- table(tabproc$func_física, tabproc$Pragmático)[-c(1), ]#rm-bajo* ,3
ctg.22 <- table(tabproc$func_cognitiva, tabproc$Pragmático)[-1, ]
ctg.23 <- table(tabproc$género, tabproc$Pragmático)#rm-bajo* [-3, ]
ctg.24 <- table(tabproc$sexo, tabproc$Pragmático)
ctg.25 <- table(tabproc$orientación_sexual, tabproc$Pragmático)[-c(1), ]#rm-bajo* ,2,7
ctg.26 <- table(tabproc$motivación, tabproc$Pragmático)[-1, ]
ctg.27 <- table(tabproc$objetivo, tabproc$Pragmático)
ctg.28 <- table(tabproc$proyección, tabproc$Pragmático)[-1, ]

# Reflector's learning style and diversity variables:
ctg.r0 <- table(tabproc$Universidad, tabproc$Reflexivo)
ctg.r1 <- table(tabproc$área_estudio, tabproc$Reflexivo)
ctg.r2 <- table(tabproc$nivel_académico, tabproc$Reflexivo)# bajo*, pero sino queda 1x5
ctg.r3 <- table(tabproc$etapa_estudio, tabproc$Reflexivo)
ctg.r4 <- table(tabproc$institución_escolar, tabproc$Reflexivo)
ctg.r5 <- table(tabproc$preuniversitario, tabproc$Reflexivo)
ctg.r6 <- table(tabproc$multi_carrera, tabproc$Reflexivo)[-1, ]
ctg.r7 <- table(tabproc$desempeño_anterior, tabproc$Reflexivo)[-1, ]
ctg.r8 <- table(tabproc$trabaja, tabproc$Reflexivo)
ctg.r9 <- table(tabproc$experiencia_laboral, tabproc$Reflexivo)
ctg.r10 <- table(tabproc$edad, tabproc$Reflexivo)#rm-bajo* [-3, ]
ctg.r11 <- table(tabproc$tecnológico, tabproc$Reflexivo)
ctg.r12 <- table(tabproc$origen_geo, tabproc$Reflexivo)#rm-bajo* [-1, ]
ctg.r13 <- table(tabproc$territorio, tabproc$Reflexivo)
ctg.r14 <- table(tabproc$movilidad, tabproc$Reflexivo)
ctg.r15 <- table(tabproc$lengua, tabproc$Reflexivo)#rm-bajo* [-1, ]
ctg.r16 <- table(tabproc$idiomas, tabproc$Reflexivo)
ctg.r17 <- table(tabproc$etnia, tabproc$Reflexivo)#rm-bajo* [-c(1,3,6), ]
ctg.r18 <- table(tabproc$religión, tabproc$Reflexivo)[-c(1), ]#rm-bajo* :3,6,10:15,17,18
ctg.r19 <- table(tabproc$nivel_socioeconómico, tabproc$Reflexivo)[-c(1), ]#rm-bajo* ,2
ctg.r20 <- table(tabproc$clase_social, tabproc$Reflexivo)[-c(1), ]#rm-bajo* ,2
ctg.r21 <- table(tabproc$func_física, tabproc$Reflexivo)[-c(1), ]#rm-bajo* ,3
ctg.r22 <- table(tabproc$func_cognitiva, tabproc$Reflexivo)[-1, ]
ctg.r23 <- table(tabproc$género, tabproc$Reflexivo)#rm-bajo* [-3, ]
ctg.r24 <- table(tabproc$sexo, tabproc$Reflexivo)
ctg.r25 <- table(tabproc$orientación_sexual, tabproc$Reflexivo)[-c(1), ]#rm-bajo* ,2,7
ctg.r26 <- table(tabproc$motivación, tabproc$Reflexivo)[-1, ]
ctg.r27 <- table(tabproc$objetivo, tabproc$Reflexivo)
ctg.r28 <- table(tabproc$proyección, tabproc$Reflexivo)[-1, ]

# Activist's learning style and diversity variables:
ctg.a0 <- table(tabproc$Universidad, tabproc$Activo)
ctg.a1 <- table(tabproc$área_estudio, tabproc$Activo)
ctg.a2 <- table(tabproc$nivel_académico, tabproc$Activo)# bajo*, pero sino queda 1x5
ctg.a3 <- table(tabproc$etapa_estudio, tabproc$Activo)
ctg.a4 <- table(tabproc$institución_escolar, tabproc$Activo)
ctg.a5 <- table(tabproc$preuniversitario, tabproc$Activo)
ctg.a6 <- table(tabproc$multi_carrera, tabproc$Activo)[-1, ]
ctg.a7 <- table(tabproc$desempeño_anterior, tabproc$Activo)[-c(1,2), ]
ctg.a8 <- table(tabproc$trabaja, tabproc$Activo)
ctg.a9 <- table(tabproc$experiencia_laboral, tabproc$Activo)
ctg.a10 <- table(tabproc$edad, tabproc$Activo)#rm-bajo* [-3, ]
ctg.a11 <- table(tabproc$tecnológico, tabproc$Activo)
ctg.a12 <- table(tabproc$origen_geo, tabproc$Activo)#rm-bajo* [-1, ]
ctg.a13 <- table(tabproc$territorio, tabproc$Activo)
ctg.a14 <- table(tabproc$movilidad, tabproc$Activo)
ctg.a15 <- table(tabproc$lengua, tabproc$Activo)#rm-bajo* [-1, ]
ctg.a16 <- table(tabproc$idiomas, tabproc$Activo)
ctg.a17 <- table(tabproc$etnia, tabproc$Activo)#rm-bajo* [-c(1,3,6), ]
ctg.a18 <- table(tabproc$religión, tabproc$Activo)[-c(1), ]#rm-bajo* :3,6,10:15,17,18
ctg.a19 <- table(tabproc$nivel_socioeconómico, tabproc$Activo)[-c(1,2), ]#rm-bajo* ,2
ctg.a20 <- table(tabproc$clase_social, tabproc$Activo)[-c(1), ]#rm-bajo* ,2
ctg.a21 <- table(tabproc$func_física, tabproc$Activo)[-c(1), ]#rm-bajo* ,3
ctg.a22 <- table(tabproc$func_cognitiva, tabproc$Activo)[-1, ]
ctg.a23 <- table(tabproc$género, tabproc$Activo)#rm-bajo* [-3, ]
ctg.a24 <- table(tabproc$sexo, tabproc$Activo)
ctg.a25 <- table(tabproc$orientación_sexual, tabproc$Activo)[-c(1), ]#rm-bajo* ,2,7
ctg.a26 <- table(tabproc$motivación, tabproc$Activo)[-1, ]
ctg.a27 <- table(tabproc$objetivo, tabproc$Activo)
ctg.a28 <- table(tabproc$proyección, tabproc$Activo)[-1, ]

# Theorist 's learning style and diversity variables:ctg.t0 <- table(tabproc$Universidad, tabproc$Teórico)
ctg.t1 <- table(tabproc$área_estudio, tabproc$Teórico)
ctg.t2 <- table(tabproc$nivel_académico, tabproc$Teórico)# bajo*, pero sino queda 1x5
ctg.t3 <- table(tabproc$etapa_estudio, tabproc$Teórico)
ctg.t4 <- table(tabproc$institución_escolar, tabproc$Teórico)
ctg.t5 <- table(tabproc$preuniversitario, tabproc$Teórico)
ctg.t6 <- table(tabproc$multi_carrera, tabproc$Teórico)[-1, ]
ctg.t7 <- table(tabproc$desempeño_anterior, tabproc$Teórico)[-1, ]
ctg.t8 <- table(tabproc$trabaja, tabproc$Teórico)
ctg.t9 <- table(tabproc$experiencia_laboral, tabproc$Teórico)
ctg.t10 <- table(tabproc$edad, tabproc$Teórico)[-3, ]#rm-bajo* [-3, ]
ctg.t11 <- table(tabproc$tecnológico, tabproc$Teórico)
ctg.t12 <- table(tabproc$origen_geo, tabproc$Teórico)#rm-bajo* [-1, ]
ctg.t13 <- table(tabproc$territorio, tabproc$Teórico)
ctg.t14 <- table(tabproc$movilidad, tabproc$Teórico)
ctg.t15 <- table(tabproc$lengua, tabproc$Teórico)#rm-bajo* [-1, ]
ctg.t16 <- table(tabproc$idiomas, tabproc$Teórico)
ctg.t17 <- table(tabproc$etnia, tabproc$Teórico)#rm-bajo* [-c(1,3,6), ]
ctg.t18 <- table(tabproc$religión, tabproc$Teórico)[-c(1), ]#rm-bajo* :3,6,10:15,17,18
ctg.t19 <- table(tabproc$nivel_socioeconómico, tabproc$Teórico)[-c(1), ]#rm-bajo* ,2
ctg.t20 <- table(tabproc$clase_social, tabproc$Teórico)[-c(1), ]#rm-bajo* ,2
ctg.t21 <- table(tabproc$func_física, tabproc$Teórico)[-c(1), ]#rm-bajo* ,3
ctg.t22 <- table(tabproc$func_cognitiva, tabproc$Teórico)[-1, ]
ctg.t23 <- table(tabproc$género, tabproc$Teórico)#rm-bajo* [-3, ]
ctg.t24 <- table(tabproc$sexo, tabproc$Teórico)
ctg.t25 <- table(tabproc$orientación_sexual, tabproc$Teórico)[-c(1), ]#rm-bajo* ,2,7
ctg.t26 <- table(tabproc$motivación, tabproc$Teórico)[-1, ]
ctg.t27 <- table(tabproc$objetivo, tabproc$Teórico)
ctg.t28 <- table(tabproc$proyección, tabproc$Teórico)[-1, ]

# Import table:
write.table(ctg.0,
            file = "./ctg.0.txt", sep = "\t", row.names = T, 
            col.names = NA)

#---- Contingency tables for VARK learning styles  ----

ctg.vark0 <- table(tabproc$Universidad, tabproc$estilo_VARK)[, -1]
ctg.vark1 <- table(tabproc$área_estudio, tabproc$estilo_VARK)[, -1]
ctg.vark2 <- table(tabproc$nivel_académico, tabproc$estilo_VARK)[, -1]# bajo*, pero sino queda 1x5
ctg.vark3 <- table(tabproc$etapa_estudio, tabproc$estilo_VARK)[, -1]
ctg.vark4 <- table(tabproc$institución_escolar, tabproc$estilo_VARK)[, -1]
ctg.vark5 <- table(tabproc$preuniversitario, tabproc$estilo_VARK)[, -1]
ctg.vark6 <- table(tabproc$multi_carrera, tabproc$estilo_VARK)[-1, -1]
ctg.vark7 <- table(tabproc$desempeño_anterior, tabproc$estilo_VARK)[-1, -1]
ctg.vark8 <- table(tabproc$trabaja, tabproc$estilo_VARK)[, -1]
ctg.vark9 <- table(tabproc$experiencia_laboral, tabproc$estilo_VARK)[, -1]
ctg.vark10 <- table(tabproc$edad, tabproc$estilo_VARK)[, -1]#rm-bajo* [-3, ]
ctg.vark11 <- table(tabproc$tecnológico, tabproc$estilo_VARK)[, -1]
ctg.vark12 <- table(tabproc$origen_geo, tabproc$estilo_VARK)[, -1]#rm-bajo* [-1, ]
ctg.vark13 <- table(tabproc$territorio, tabproc$estilo_VARK)[, -1]
ctg.vark14 <- table(tabproc$movilidad, tabproc$estilo_VARK)[, -1]
ctg.vark15 <- table(tabproc$lengua, tabproc$estilo_VARK)[, -1]#rm-bajo* [-1, ]
ctg.vark16 <- table(tabproc$idiomas, tabproc$estilo_VARK)[, -1]
ctg.vark17 <- table(tabproc$etnia, tabproc$estilo_VARK)[, -1]#rm-bajo* [-c(1,3,6), ]
ctg.vark18 <- table(tabproc$religión, tabproc$estilo_VARK)[-c(1), -1]#rm-bajo* :3,6,10:15,17,18
ctg.vark19 <- table(tabproc$nivel_socioeconómico, tabproc$estilo_VARK)[-c(1), -1]#rm-bajo* ,2
ctg.vark20 <- table(tabproc$clase_social, tabproc$estilo_VARK)[-c(1), -1]#rm-bajo* ,2
ctg.vark21 <- table(tabproc$func_física, tabproc$estilo_VARK)[-c(1), -1]#rm-bajo* ,3
ctg.vark22 <- table(tabproc$func_cognitiva, tabproc$estilo_VARK)[-1, -1]
ctg.vark23 <- table(tabproc$género, tabproc$estilo_VARK)[, -1]#rm-bajo* [-3, ]
ctg.vark24 <- table(tabproc$sexo, tabproc$estilo_VARK)[, -1]
ctg.vark25 <- table(tabproc$orientación_sexual, tabproc$estilo_VARK)[-c(1), -1]#rm-bajo* ,2,7
ctg.vark26 <- table(tabproc$motivación, tabproc$estilo_VARK)[-1, -1]
ctg.vark27 <- table(tabproc$objetivo, tabproc$estilo_VARK)[, -1]
ctg.vark28 <- table(tabproc$proyección, tabproc$estilo_VARK)[-1, -1]
#---- Fisher's tests ----

# For Pragmatist's contingency tables:
fisher.test(ctg.0, workspace = 2e9)
fisher.test(ctg.1, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.2, workspace = 2e9)
fisher.test(ctg.3, workspace = 2e9)
fisher.test(ctg.4, workspace = 2e9)
fisher.test(ctg.5, workspace = 2e9)
fisher.test(ctg.6, workspace = 2e9)
fisher.test(ctg.7, workspace = 2e9)
fisher.test(ctg.8, workspace = 2e9)
fisher.test(ctg.9, workspace = 2e9)
fisher.test(ctg.10, workspace = 2e9)
fisher.test(ctg.11, workspace = 2e9)
fisher.test(ctg.12, workspace = 2e9)
fisher.test(ctg.13, workspace = 2e9)
fisher.test(ctg.14, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.15, workspace = 2e9)
fisher.test(ctg.16, workspace = 2e9)
fisher.test(ctg.17, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.18, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.19, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.20, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.21, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.22, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.23, workspace = 2e9)
fisher.test(ctg.24, workspace = 2e9)
fisher.test(ctg.25, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.26, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.27, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.28, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda

# For Reflector's contingency tables:
fisher.test(ctg.r0, workspace = 2e9)
fisher.test(ctg.r1, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r2, workspace = 2e9)
fisher.test(ctg.r3, workspace = 2e9)
fisher.test(ctg.r4, workspace = 2e9)
fisher.test(ctg.r5, workspace = 2e9)
fisher.test(ctg.r6, workspace = 2e9)
fisher.test(ctg.r7, workspace = 2e9)
fisher.test(ctg.r8, workspace = 2e9)
fisher.test(ctg.r9, workspace = 2e9)
fisher.test(ctg.r10, workspace = 2e9)
fisher.test(ctg.r11, workspace = 2e9)
fisher.test(ctg.r12, workspace = 2e9)
fisher.test(ctg.r13, workspace = 2e9)
fisher.test(ctg.r14, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.r15, workspace = 2e9)
fisher.test(ctg.r16, workspace = 2e9)
fisher.test(ctg.r17, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r18, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r19, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r20, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.r21, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.r22, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r23, workspace = 2e9)
fisher.test(ctg.r24, workspace = 2e9)
fisher.test(ctg.r25, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r26, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r27, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.r28, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda

# For Activist's contingency tables:
fisher.test(ctg.a0, workspace = 2e9)
fisher.test(ctg.a1, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a2, workspace = 2e9)
fisher.test(ctg.a3, workspace = 2e9)
fisher.test(ctg.a4, workspace = 2e9)
fisher.test(ctg.a5, workspace = 2e9)
fisher.test(ctg.a6, workspace = 2e9)
fisher.test(ctg.a7, workspace = 2e9)
fisher.test(ctg.a8, workspace = 2e9)
fisher.test(ctg.a9, workspace = 2e9)
fisher.test(ctg.a10, workspace = 2e9)
fisher.test(ctg.a11, workspace = 2e9)
fisher.test(ctg.a12, workspace = 2e9)
fisher.test(ctg.a13, workspace = 2e9)
fisher.test(ctg.a14, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.a15, workspace = 2e9)
fisher.test(ctg.a16, workspace = 2e9)
fisher.test(ctg.a17, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a18, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a19, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a20, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.a21, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.a22, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a23, workspace = 2e9)
fisher.test(ctg.a24, workspace = 2e9)
fisher.test(ctg.a25, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a26, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a27, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.a28, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda

# For Theorist's contingency tables:
fisher.test(ctg.t0, workspace = 2e9)
fisher.test(ctg.t1, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t2, workspace = 2e9)
fisher.test(ctg.t3, workspace = 2e9)
fisher.test(ctg.t4, workspace = 2e9)
fisher.test(ctg.t5, workspace = 2e9)
fisher.test(ctg.t6, workspace = 2e9)
fisher.test(ctg.t7, workspace = 2e9)
fisher.test(ctg.t8, workspace = 2e9)
fisher.test(ctg.t9, workspace = 2e9)
fisher.test(ctg.t10, workspace = 2e9)
fisher.test(ctg.t11, workspace = 2e9)
fisher.test(ctg.t12, workspace = 2e9)
fisher.test(ctg.t13, workspace = 2e9)
fisher.test(ctg.t14, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.t15, workspace = 2e9)
fisher.test(ctg.t16, workspace = 2e9)
fisher.test(ctg.t17, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t18, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t19, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t20, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.t21, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#probar con tiempo
fisher.test(ctg.t22, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t23, workspace = 2e9)
fisher.test(ctg.t24, workspace = 2e9)
fisher.test(ctg.t25, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t26, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t27, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda
fisher.test(ctg.t28, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)#queda

# For VARK contingency tables:
fisher.test(ctg.vark0, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark1, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark2, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark3, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark4, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark5, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark6, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark7, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark8, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark9, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark10, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark11, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark12, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark13, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark14, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark15, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark16, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark17, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark18, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark19, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark20, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark21, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark22, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark23, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark24, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark25, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark26, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark27, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)
fisher.test(ctg.vark28, workspace = 2e9, simulate.p.value = TRUE, B = 1e5)

#---- Chi-Square tests ----

# For Pragmatist's contingency tables:
chisq.test(ctg.0, simulate.p.value = TRUE, B = 1e5)$stdres#no
chisq.test(ctg.1, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.2, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.3, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.4)
chisq.test(ctg.5)
chisq.test(ctg.6, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.7, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.8, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.9)
chisq.test(ctg.10, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.11, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.12, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.13, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.14, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.15, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.16, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.17, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.18, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.19, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.20, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.21, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.22, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.23, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.24)
chisq.test(ctg.25, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.26, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.27, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.28, simulate.p.value = TRUE, B = 1e5)#no

# For Reflector's contingency tables:
chisq.test(ctg.r0, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r1, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r2, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r3, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r4)
chisq.test(ctg.r5)
chisq.test(ctg.r6, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r7, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r8, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r9)
chisq.test(ctg.r10, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r11, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r12, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r13, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r14, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r15, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r16, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r17, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r18, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r19, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r20, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r21, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r22, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r23, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r24)
chisq.test(ctg.r25, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r26, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r27, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.r28, simulate.p.value = TRUE, B = 1e5)#no

# For Activist's contingency tables:
chisq.test(ctg.a0, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a1, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a2, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a3, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a4)
chisq.test(ctg.a5)
chisq.test(ctg.a6, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a7, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a8, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a9)
chisq.test(ctg.a10, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a11, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a12, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a13, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a14, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a15, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a16, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a17, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a18, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a19, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a20, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a21, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a22, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a23, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a24)
chisq.test(ctg.a25, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a26, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a27, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.a28, simulate.p.value = TRUE, B = 1e5)#no

# For Theorist's contingency tables:
chisq.test(ctg.t0, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t1, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t2, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t3, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t4)
chisq.test(ctg.t5)
chisq.test(ctg.t6, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t7, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t8)
chisq.test(ctg.t9)
chisq.test(ctg.t10, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t11, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t12, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t13, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t14, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t15, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t16, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t17, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t18, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t19, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t20, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t21, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t22, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t23, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t24)
chisq.test(ctg.t25, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t26, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t27, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.t28, simulate.p.value = TRUE, B = 1e5)#no

# For VARK contingency tables:
chisq.test(ctg.vark0, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark1, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark2, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark3, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark4, simulate.p.value = TRUE, B = 1e5)
chisq.test(ctg.vark5, simulate.p.value = TRUE, B = 1e5)
chisq.test(ctg.vark6, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark7, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark8, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark9, simulate.p.value = TRUE, B = 1e5)
chisq.test(ctg.vark10, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark11, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark12, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark13, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark14, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark15, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark16, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark17, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark18, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark19, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark20, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark21, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark22, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark23, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark24, simulate.p.value = TRUE, B = 1e5)
chisq.test(ctg.vark25, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark26, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark27, simulate.p.value = TRUE, B = 1e5)#no
chisq.test(ctg.vark28, simulate.p.value = TRUE, B = 1e5)#no

#---- Post hoc Fisher tests (only for previous significant events) ----

# For Honey-Mumford learning styles:
# pairwise_fisher_test(t(ctg.a0)[2:3,], p.adjust.method = "holm")
fisher.multcomp(t(ctg.0), p.method = "holm")
fisher.multcomp(t(ctg.2), p.method = "holm")
fisher.multcomp(t(ctg.16), p.method = "holm")
fisher.multcomp(t(ctg.28), p.method = "holm")
fisher.multcomp(t(ctg.r2), p.method = "holm")
fisher.multcomp(t(ctg.a0), p.method = "holm")
fisher.multcomp(t(ctg.a1), p.method = "holm")
fisher.multcomp(t(ctg.a2), p.method = "holm")#, simulate.p.value = TRUE, B = 1e5)
fisher.multcomp(t(ctg.a7), p.method = "holm")
fisher.multcomp(t(ctg.a8), p.method = "holm")
fisher.multcomp(t(ctg.a19), p.method = "holm")#, simulate.p.value = TRUE, B = 1e5)
fisher.multcomp(t(ctg.a22), p.method = "holm")#, simulate.p.value = TRUE, B = 1e5)
fisher.multcomp(t(ctg.a26), p.method = "holm")#, simulate.p.value = TRUE, B = 1e5)
fisher.multcomp(t(ctg.t10), p.method = "holm")

# For VARK lerning styles:
fisher.multcomp(ctg.vark0, p.method = "holm")
fisher.multcomp(ctg.vark5, p.method = "holm")
fisher.multcomp(ctg.vark7, p.method = "holm")
fisher.multcomp(ctg.vark11, p.method = "holm")
fisher.multcomp(ctg.vark20, p.method = "holm")
fisher.multcomp(ctg.vark21, p.method = "holm")

#---- Post hoc Chi-Square tests (only for previous significant events) ----

# For Honey-Mumford learning styles:
chisq.posthoc.test(ctg.0, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.2, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.16, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.28, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.r2, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a0, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a1, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a2, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a7, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a8, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a19, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a22, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.a26, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.t10, method = "holm", simulate.p.value = TRUE, B = 1e5)
#---
chisq.posthoc.test(ctg.19, method = "holm", simulate.p.value = TRUE, B = 1e5)

# For VARK learning styles:
chisq.posthoc.test(ctg.vark0, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.vark5, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.vark7, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.vark11, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.vark20, method = "holm", simulate.p.value = TRUE, B = 1e5)
chisq.posthoc.test(ctg.vark21, method = "holm", simulate.p.value = TRUE, B = 1e5)

#---- V de Cramer (only for previous significant events) ----

# For Honey-Mumford learning styles:
assocstats(ctg.0)
assocstats(ctg.2)
assocstats(ctg.16)
assocstats(ctg.28)
assocstats(ctg.r2)
assocstats(ctg.a0)
assocstats(ctg.a1)
assocstats(ctg.a2)
assocstats(ctg.a7)
assocstats(ctg.a8)
assocstats(ctg.a19)
assocstats(ctg.a22)
assocstats(ctg.a26)
assocstats(ctg.t10)

# For VARK learning styles:
assocstats(ctg.vark0)
assocstats(ctg.vark5)
assocstats(ctg.vark7)
assocstats(ctg.vark11)
assocstats(ctg.vark20)
assocstats(ctg.vark21)

#---- Plots for Honey-Mumford associations (Figure 6) ----

# Plot A:
tabproc.rel0 <- tabproc %>%
  group_by(Universidad) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(Activo %in% c("Baja", "Moderada", "Alta"))

png("./new_figs/bar.a0.png", units = "in", width = 3, height = 4, res = 300)
ggplot(tabproc.rel0, aes(fill=Activo, y=Porcentaje, x=Universidad)) + #Freq
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[2:4])) +
  labs(y="Porcentaje estudiantes (%)", x= "Universidad",
       fill="Preferencia estilo Activo", title = "Estilo Activo") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot B:
tabproc.rel7 <- tabproc %>%
  filter(desempeño_anterior != "", desempeño_anterior != "Tercio medio") %>%
  group_by(desempeño_anterior) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(Activo %in% c("Alta"))

png("./new_figs/bar.a7.png", units = "in", width = 3, height = 4, res = 300)
ggplot(tabproc.rel7, #filter(tabproc[tabproc$desempeño_anterior != "", ],
       #desempeño_anterior != "Tercio medio", Activo %in% c("Alta")),
       aes(fill=Activo, y=Porcentaje, x=desempeño_anterior)) + #Freq
  # geom_bar(position="fill", stat="identity") +
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[4])) +
  labs(y="Porcentaje estudiantes (%)", x= "Desempeño académico previo",
       title="Estilo Activo") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot C:
tabproc.rel8 <- tabproc %>%
  group_by(trabaja) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(Activo %in% c("Muy alta"))

png("./new_figs/bar.a8.png", units = "in", width = 3, height = 4, res = 300)
ggplot(tabproc.rel8, aes(fill=Activo, y=Porcentaje, x=trabaja)) + #Freq
  # geom_bar(position="fill", stat="identity") +
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[5])) +
  labs(y="Porcentaje estudiantes (%)", x= "Estudia y trabaja",
       title="Estilo Activo") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot D:
tabproc.rel16 <- tabproc %>%
  filter(idiomas != "Multilingüe") %>%
  group_by(idiomas) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(Pragmático %in% c("Baja", "Alta"))

png("./new_figs/bar.16.png", units = "in", width = 3, height = 4, res = 300)
# ggplot(tabproc %>% filter(idiomas != "Multilingüe"), aes(fill=Pragmático, y=Freq, x=idiomas)) +
ggplot(tabproc.rel16, 
       aes(fill=Pragmático, y=Porcentaje, x=idiomas)) + # Freq,
  geom_bar(stat="identity") +
  # geom_bar(position = position_dodge()) +
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[c(2,4)])) +
  labs(y="Porcentaje estudiantes (%)", x= "Manejo de idiomas",
       title="Estilo Pragmático") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot E:
tabproc.rel10 <- tabproc %>%
  filter(edad2 != "40-65 años") %>%
  group_by(edad2) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(Teórico %in% c("Alta"))

png("./new_figs/bar.t10.png", units = "in", width = 3, height = 4, res = 300)
ggplot(tabproc.rel10, #%>% filter(edad2 != "40-65 años"), 
       aes(fill=Teórico, y=Porcentaje, x=edad2)) + #Freq
  geom_bar(stat="identity") + #position="fill", 
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[4])) +
  labs(y="Porcentaje estudiantes (%)", x= "Edad",
       title="Estilo Teórico") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  scale_x_discrete() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Isolate legend:
legend.bar <- cowplot::get_legend(ggplot(tabproc %>% filter(Activo != "Muy baja"),
                                         aes(fill=Activo, y=Freq, x=Universidad)) + #Freq
                                    geom_bar(stat="identity") +
                                    scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[1:5])) +
                                    labs(y="Porcentaje estudiantes", x= "Universidad",
                                         fill="Nivel de preferencia del estilo", title = "Preferencia Estilo Activo") +
                                    theme_bw() +
                                    # scale_y_continuous(labels = label_percent()) +
                                    theme(legend.position = "right",
                                          plot.title = element_text(hjust = 0.5),
                                          panel.grid = element_blank(),
                                          text = element_text(size = 11),
                                          axis.text = element_text(size = 11),
                                          legend.text = element_text(size = 11)))
legend.bar <- as_ggplot(legend.bar)
png("./new_figs/legend_barplots.png", units = "in", width = 3, height = 4, res = 300)
legend.bar
dev.off()


#---- Plots for VARK associations (Figure 7) ----

# Plot A:
tabproc.rel11 <- tabproc %>%
  group_by(tecnológico2) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario. %in% "Multimodal (RK)")

png("./new_figs/vark11.png", units = "in", width = 4.5, height = 4, res = 300)
ggplot(tabproc.rel11,
       aes(fill=X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario., 
           y=Porcentaje, x=tecnológico2)) + #Freq
  geom_bar(stat="identity") + #position="fill", 
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[4])) +
  labs(y="Porcentaje estudiantes (%)", x= "Nivel tecnológico",
       title="Estilo Multimodal (RK)") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  scale_x_discrete() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot B:
tabproc.rel5 <- tabproc %>%
  # filter(tecnológico != "40-65 años") %>%
  group_by(preuniversitario) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario. %in% c("Lectura/Escritura, media"))

png("./new_figs/vark15.png", units = "in", width = 4.5, height = 4, res = 300)
ggplot(tabproc.rel5, #%>% filter(edad2 != "40-65 años"), 
       aes(fill=X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario., 
           y=Porcentaje, x=preuniversitario)) + #Freq
  geom_bar(stat="identity") + #position="fill", 
  scale_fill_manual(values = rev(brewer.pal(5, "YlGnBu")[4])) +
  labs(y="Porcentaje estudiantes (%)", x= "Estudios preuniversitarios",
       title="Estilo Lectura/Escritura (preferencia media)") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  scale_x_discrete() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()

# Plot C:
tabproc.rel21 <- tabproc %>%
  # filter(tecnológico != "40-65 años") %>%
  group_by(func_física) %>%
  mutate(Porcentaje = Freq / sum(Freq) * 100) %>%
  ungroup() %>%
  filter(X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario. %in% c("Auditivo, media", "Kinestésico, media"))

png("./new_figs/vark21.png", units = "in", width = 9, height = 4, res = 300)
ggplot(tabproc.rel21, #%>% filter(edad2 != "40-65 años"), 
       aes(fill=X1...Cuál.es.el.resultado.de.tu.preferencia.de.aprendizaje.según.el.cuestionario., 
           y=Porcentaje, x=func_física)) + #Freq
  geom_bar(stat="identity") + #position="fill", 
  scale_fill_manual(values = c(rev(brewer.pal(5, "YlGnBu")[4]), "#2772a5")) +
  labs(y="Porcentaje estudiantes (%)", x= "Funcionalidad física",
       title="Estilos Auditivo y Kinestésico (preferencias medias)") +
  theme_bw() +
  # scale_y_continuous(labels = label_percent()) +
  scale_x_discrete() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))
dev.off()


#---- Save environment ----
save.image("./results_TFM.RData")
