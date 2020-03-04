# Random Assignment: This macro function divides itself in 3 functions:
# 1. Create Treatment status var: treatment_status_var
# 2. Create strata variable: rct_strata
# 3. Create cuartiles, ntile with label: rct_ntile


# 1. Create Treatment status var: treatment_status_var
# Description: This function creates a variable that indicates the treatment status
# Arguments: treat_var, share_control, n treatment groups, strata, missfits, seed, equal
# miss fits: NA,
#           strata (missfits allocated to strata randomly),
#           global (assigning missfits to treatment),



# Equal fractions, los misfits son el modulo de n_strata / n_trats.
# Unequal fractions,
treatment_status_var <- function(data,
                                 share_control,
                                 n_t = 2,
                                 strata_varlist,
                                 missfits = "global",
                                 seed = 1990,
                                 share_ti = 1/n_t - share_control/n_t,
                                 key) {


    # Arrange by key
    data <- data %>% dplyr::arrange(key)

    # Creating a random variable
    set.seed(seed)
    data$random <- runif(nrow(data))

    #Strata size
    data <-
        data %>%
        group_by({strata_varlist}) %>%
        mutate(n_strata = n())

    # Strata id
    data$strata<-dplyr::group_indices(data, data$n_strata)

    # Row number on each strata
    data <- data %>%
        dplyr::group_by(strata) %>%
        dplyr::arrange(strata, random) %>%
        dplyr::mutate(strata_index = dplyr::row_number())

    # Strata summary
    resumen_strata <-
        data %>%
        group_by(strata, {strata_varlist}) %>%
        summarise(n_strata = mean(n_strata))


    # As data table para q corra
    data <- as.data.table(data)

    # Treatment sequence, if equal
    seq_treat<-seq(1, n_t)
    group_sequence  <- c(0,
                         share_control,
                         share_control + seq_treat * (1-share_control)/n_t)
    for (i in 1:57) {
        data <- data[strata_index/n_strata <= group_sequence[i + 1] &
                     strata_index/n_strata2 > group_sequence[i], `:=`(treat, i - 1)]
    }



    }




# Reasignando los estratos pequeños globally
universo_por_asignar <- universo %>% filter(n_strata < 57)

universo <- universo %>% filter(n_strata >= 57)


universo_por_asignar <- universo_por_asignar %>% ungroup()

universo_por_asignar <- universo_por_asignar %>% arrange(random) %>% mutate(index = row_number())

summary(universo_por_asignar$index)










############################### Balances: Balance por trat (#57 grupos) Balance por Msj (#8 grupos) Balance por periodicidad (#3
############################### grupos) Balance por descanso vs sin descanso (#3 grupos) Balance por mismo vs distinto msj


################## Balance por trat
universo <- universo %>% ungroup()

balance_trat <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:EGRESOS_RASTREABLES, share_gastos_rastreables,
    banco_principal, trat) %>% gather(key = variables, value = value, -trat) %>% group_by(trat, variables) %>%
    summarise(value = list(value)) %>% spread(key = trat, value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(`0`),
    unlist(`1`), )$estimate[1], Mean_trat1 = t.test(unlist(`0`), unlist(`1`))$estimate[2], Mean_trat2 = t.test(unlist(`0`),
    unlist(`2`))$estimate[2], Mean_trat3 = t.test(unlist(`0`), unlist(`3`))$estimate[2], Mean_trat4 = t.test(unlist(`0`),
    unlist(`4`))$estimate[2], Mean_trat5 = t.test(unlist(`0`), unlist(`5`))$estimate[2], Mean_trat6 = t.test(unlist(`0`),
    unlist(`6`))$estimate[2], Mean_trat7 = t.test(unlist(`0`), unlist(`7`))$estimate[2], Mean_trat8 = t.test(unlist(`0`),
    unlist(`8`))$estimate[2], Mean_trat9 = t.test(unlist(`0`), unlist(`9`))$estimate[2], Mean_trat10 = t.test(unlist(`0`),
    unlist(`10`))$estimate[2], Mean_trat11 = t.test(unlist(`0`), unlist(`11`))$estimate[2], Mean_trat12 = t.test(unlist(`0`),
    unlist(`12`))$estimate[2], Mean_trat13 = t.test(unlist(`0`), unlist(`13`))$estimate[2], Mean_trat14 = t.test(unlist(`0`),
    unlist(`14`))$estimate[2], Mean_trat15 = t.test(unlist(`0`), unlist(`15`))$estimate[2], Mean_trat16 = t.test(unlist(`0`),
    unlist(`16`))$estimate[2], Mean_trat17 = t.test(unlist(`0`), unlist(`17`))$estimate[2], Mean_trat18 = t.test(unlist(`0`),
    unlist(`18`))$estimate[2], Mean_trat19 = t.test(unlist(`0`), unlist(`19`))$estimate[2], Mean_trat20 = t.test(unlist(`0`),
    unlist(`20`))$estimate[2], Mean_trat21 = t.test(unlist(`0`), unlist(`21`))$estimate[2], Mean_trat22 = t.test(unlist(`0`),
    unlist(`22`))$estimate[2], Mean_trat23 = t.test(unlist(`0`), unlist(`23`))$estimate[2], Mean_trat24 = t.test(unlist(`0`),
    unlist(`24`))$estimate[2], Mean_trat25 = t.test(unlist(`0`), unlist(`25`))$estimate[2], Mean_trat26 = t.test(unlist(`0`),
    unlist(`26`))$estimate[2], Mean_trat27 = t.test(unlist(`0`), unlist(`27`))$estimate[2], Mean_trat28 = t.test(unlist(`0`),
    unlist(`28`))$estimate[2], Mean_trat29 = t.test(unlist(`0`), unlist(`29`))$estimate[2], Mean_trat30 = t.test(unlist(`0`),
    unlist(`30`))$estimate[2], Mean_trat31 = t.test(unlist(`0`), unlist(`31`))$estimate[2], Mean_trat32 = t.test(unlist(`0`),
    unlist(`32`))$estimate[2], Mean_trat33 = t.test(unlist(`0`), unlist(`33`))$estimate[2], Mean_trat34 = t.test(unlist(`0`),
    unlist(`34`))$estimate[2], Mean_trat35 = t.test(unlist(`0`), unlist(`35`))$estimate[2], Mean_trat36 = t.test(unlist(`0`),
    unlist(`36`))$estimate[2], Mean_trat37 = t.test(unlist(`0`), unlist(`37`))$estimate[2], Mean_trat38 = t.test(unlist(`0`),
    unlist(`38`))$estimate[2], Mean_trat39 = t.test(unlist(`0`), unlist(`39`))$estimate[2], Mean_trat40 = t.test(unlist(`0`),
    unlist(`40`))$estimate[2], Mean_trat41 = t.test(unlist(`0`), unlist(`41`))$estimate[2], Mean_trat42 = t.test(unlist(`0`),
    unlist(`42`))$estimate[2], Mean_trat43 = t.test(unlist(`0`), unlist(`43`))$estimate[2], Mean_trat44 = t.test(unlist(`0`),
    unlist(`44`))$estimate[2], Mean_trat45 = t.test(unlist(`0`), unlist(`45`))$estimate[2], Mean_trat46 = t.test(unlist(`0`),
    unlist(`46`))$estimate[2], Mean_trat47 = t.test(unlist(`0`), unlist(`47`))$estimate[2], Mean_trat48 = t.test(unlist(`0`),
    unlist(`48`))$estimate[2], Mean_trat49 = t.test(unlist(`0`), unlist(`49`))$estimate[2], Mean_trat50 = t.test(unlist(`0`),
    unlist(`50`))$estimate[2], Mean_trat51 = t.test(unlist(`0`), unlist(`51`))$estimate[2], Mean_trat52 = t.test(unlist(`0`),
    unlist(`52`))$estimate[2], Mean_trat53 = t.test(unlist(`0`), unlist(`53`))$estimate[2], Mean_trat54 = t.test(unlist(`0`),
    unlist(`54`))$estimate[2], Mean_trat55 = t.test(unlist(`0`), unlist(`55`))$estimate[2], Mean_trat56 = t.test(unlist(`0`),
    unlist(`56`))$estimate[2], p_value1 = t.test(unlist(`0`), unlist(`1`))$p.value, p_value2 = t.test(unlist(`0`),
    unlist(`2`))$p.value, p_value3 = t.test(unlist(`0`), unlist(`3`))$p.value, p_value4 = t.test(unlist(`0`),
    unlist(`4`))$p.value, p_value5 = t.test(unlist(`0`), unlist(`5`))$p.value, p_value6 = t.test(unlist(`0`),
    unlist(`6`))$p.value, p_value7 = t.test(unlist(`0`), unlist(`7`))$p.value, p_value8 = t.test(unlist(`0`),
    unlist(`8`))$p.value, p_value9 = t.test(unlist(`0`), unlist(`9`))$p.value, p_value10 = t.test(unlist(`0`),
    unlist(`10`))$p.value, p_value11 = t.test(unlist(`0`), unlist(`11`))$p.value, p_value12 = t.test(unlist(`0`),
    unlist(`12`))$p.value, p_value13 = t.test(unlist(`0`), unlist(`13`))$p.value, p_value14 = t.test(unlist(`0`),
    unlist(`14`))$p.value, p_value15 = t.test(unlist(`0`), unlist(`15`))$p.value, p_value16 = t.test(unlist(`0`),
    unlist(`16`))$p.value, p_value17 = t.test(unlist(`0`), unlist(`17`))$p.value, p_value18 = t.test(unlist(`0`),
    unlist(`18`))$p.value, p_value19 = t.test(unlist(`0`), unlist(`19`))$p.value, p_value20 = t.test(unlist(`0`),
    unlist(`20`))$p.value, p_value21 = t.test(unlist(`0`), unlist(`21`))$p.value, p_value22 = t.test(unlist(`0`),
    unlist(`22`))$p.value, p_value23 = t.test(unlist(`0`), unlist(`23`))$p.value, p_value24 = t.test(unlist(`0`),
    unlist(`24`))$p.value, p_value25 = t.test(unlist(`0`), unlist(`25`))$p.value, p_value26 = t.test(unlist(`0`),
    unlist(`26`))$p.value, p_value27 = t.test(unlist(`0`), unlist(`27`))$p.value, p_value28 = t.test(unlist(`0`),
    unlist(`28`))$p.value, p_value29 = t.test(unlist(`0`), unlist(`29`))$p.value, p_value30 = t.test(unlist(`0`),
    unlist(`30`))$p.value, p_value31 = t.test(unlist(`0`), unlist(`31`))$p.value, p_value32 = t.test(unlist(`0`),
    unlist(`32`))$p.value, p_value33 = t.test(unlist(`0`), unlist(`33`))$p.value, p_value34 = t.test(unlist(`0`),
    unlist(`34`))$p.value, p_value35 = t.test(unlist(`0`), unlist(`35`))$p.value, p_value36 = t.test(unlist(`0`),
    unlist(`36`))$p.value, p_value37 = t.test(unlist(`0`), unlist(`37`))$p.value, p_value38 = t.test(unlist(`0`),
    unlist(`38`))$p.value, p_value39 = t.test(unlist(`0`), unlist(`39`))$p.value, p_value40 = t.test(unlist(`0`),
    unlist(`40`))$p.value, p_value41 = t.test(unlist(`0`), unlist(`41`))$p.value, p_value42 = t.test(unlist(`0`),
    unlist(`42`))$p.value, p_value43 = t.test(unlist(`0`), unlist(`43`))$p.value, p_value44 = t.test(unlist(`0`),
    unlist(`44`))$p.value, p_value45 = t.test(unlist(`0`), unlist(`45`))$p.value, p_value46 = t.test(unlist(`0`),
    unlist(`46`))$p.value, p_value47 = t.test(unlist(`0`), unlist(`47`))$p.value, p_value48 = t.test(unlist(`0`),
    unlist(`48`))$p.value, p_value49 = t.test(unlist(`0`), unlist(`49`))$p.value, p_value50 = t.test(unlist(`0`),
    unlist(`50`))$p.value, p_value51 = t.test(unlist(`0`), unlist(`51`))$p.value, p_value52 = t.test(unlist(`0`),
    unlist(`52`))$p.value, p_value53 = t.test(unlist(`0`), unlist(`53`))$p.value, p_value54 = t.test(unlist(`0`),
    unlist(`54`))$p.value, p_value55 = t.test(unlist(`0`), unlist(`55`))$p.value, p_value56 = t.test(unlist(`0`),
    unlist(`56`))$p.value)


################## Balance por msj
balance_msj <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:EGRESOS_RASTREABLES, share_gastos_rastreables,
    banco_principal, msj) %>% gather(key = variables, value = value, -msj) %>% group_by(msj, variables) %>%
    summarise(value = list(value)) %>% spread(key = msj, value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(`0`),
    unlist(`1`), )$estimate[1], Mean_trat1 = t.test(unlist(`0`), unlist(`1`))$estimate[2], Mean_trat2 = t.test(unlist(`0`),
    unlist(`2`))$estimate[2], Mean_trat3 = t.test(unlist(`0`), unlist(`3`))$estimate[2], Mean_trat4 = t.test(unlist(`0`),
    unlist(`4`))$estimate[2], Mean_trat5 = t.test(unlist(`0`), unlist(`5`))$estimate[2], Mean_trat6 = t.test(unlist(`0`),
    unlist(`6`))$estimate[2], Mean_trat7 = t.test(unlist(`0`), unlist(`7`))$estimate[2], p_value1 = t.test(unlist(`0`),
    unlist(`1`))$p.value, p_value2 = t.test(unlist(`0`), unlist(`2`))$p.value, p_value3 = t.test(unlist(`0`),
    unlist(`3`))$p.value, p_value4 = t.test(unlist(`0`), unlist(`4`))$p.value, p_value5 = t.test(unlist(`0`),
    unlist(`5`))$p.value, p_value6 = t.test(unlist(`0`), unlist(`6`))$p.value, p_value7 = t.test(unlist(`0`),
    unlist(`7`))$p.value)


######################### Balance por periodicidad
balance_periodicidad <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:EGRESOS_RASTREABLES,
    share_gastos_rastreables, banco_principal, periodicidad) %>% gather(key = variables, value = value,
    -periodicidad) %>% group_by(periodicidad, variables) %>% summarise(value = list(value)) %>% spread(key = periodicidad,
    value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(control), unlist(quincenal))$estimate[1],
    Mean_quincenal = t.test(unlist(control), unlist(quincenal))$estimate[2], Mean_semanal = t.test(unlist(control),
        unlist(semanal))$estimate[2], p_value_quincenal = t.test(unlist(control), unlist(quincenal))$p.value,
    p_value_semanal = t.test(unlist(control), unlist(semanal))$p.value)


#################################### Balance por descanso vs no descanso
balance_descanso <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:EGRESOS_RASTREABLES,
    share_gastos_rastreables, banco_principal, descanso) %>% gather(key = variables, value = value,
    -descanso) %>% group_by(descanso, variables) %>% summarise(value = list(value)) %>% spread(key = descanso,
    value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(Control), unlist(`0`))$estimate[1],
    Mean_sin_descanso = t.test(unlist(Control), unlist(`0`))$estimate[2], Mean_con_descanso = t.test(unlist(Control),
        unlist(`1`))$estimate[2], p_value_sin_descanso = t.test(unlist(Control), unlist(`0`))$p.value,
    p_value_con_descanso = t.test(unlist(Control), unlist(`1`))$p.value)



#################################### Balance por mismo vs diferente
balance_rpt_msj <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:EGRESOS_RASTREABLES,
    share_gastos_rastreables, banco_principal, repeticion_msj) %>% gather(key = variables, value = value,
    -repeticion_msj) %>% group_by(repeticion_msj, variables) %>% summarise(value = list(value)) %>%
    spread(key = repeticion_msj, value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(Control),
    unlist(Diferente))$estimate[1], Mean_diferente = t.test(unlist(Control), unlist(Diferente))$estimate[2],
    Mean_mismo = t.test(unlist(Control), unlist(Mismo))$estimate[2], p_value_diferente = t.test(unlist(Control),
        unlist(Diferente))$p.value, p_value_mismo = t.test(unlist(Control), unlist(Mismo))$p.value)


balances <- list(por_trat = balance_trat, por_msj = balance_msj, por_periodicidad = balance_periodicidad,
    por_descanso = balance_descanso, por_rpt_msj = balance_rpt_msj)


write.xlsx(balances, file = "balances.xlsx")

rm(balance_descanso, balance_msj, balance_periodicidad, balance_rpt_msj, balance_trat)
rm(balances)


# Balance para banco principal
universo_banco_principal <- universo %>% filter(banco_principal == 1)


################## Balance por trat
universo <- universo %>% ungroup()

balance_trat <- universo_banco_principal %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:CELULAR_OK,
    ING_MENSUAL:SDOPROM_CBOLSA, TXN_ATM, TXN_SUC, TXN_MON_BXI, TXN_NOMON_BXI, TXN_CORRESPONSALES:BEM,
    SDO_VIG_CA:EGRESOS_RASTREABLES, -SDOTDC_OFFUS, trat) %>% gather(key = variables, value = value,
    -trat) %>% group_by(trat, variables) %>% summarise(value = list(value)) %>% spread(key = trat,
    value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(`0`), unlist(`1`), )$estimate[1],
    Mean_trat1 = t.test(unlist(`0`), unlist(`1`))$estimate[2], Mean_trat2 = t.test(unlist(`0`), unlist(`2`))$estimate[2],
    Mean_trat3 = t.test(unlist(`0`), unlist(`3`))$estimate[2], Mean_trat4 = t.test(unlist(`0`), unlist(`4`))$estimate[2],
    Mean_trat5 = t.test(unlist(`0`), unlist(`5`))$estimate[2], Mean_trat6 = t.test(unlist(`0`), unlist(`6`))$estimate[2],
    Mean_trat7 = t.test(unlist(`0`), unlist(`7`))$estimate[2], Mean_trat8 = t.test(unlist(`0`), unlist(`8`))$estimate[2],
    Mean_trat9 = t.test(unlist(`0`), unlist(`9`))$estimate[2], Mean_trat10 = t.test(unlist(`0`), unlist(`10`))$estimate[2],
    Mean_trat11 = t.test(unlist(`0`), unlist(`11`))$estimate[2], Mean_trat12 = t.test(unlist(`0`),
        unlist(`12`))$estimate[2], Mean_trat13 = t.test(unlist(`0`), unlist(`13`))$estimate[2], Mean_trat14 = t.test(unlist(`0`),
        unlist(`14`))$estimate[2], Mean_trat15 = t.test(unlist(`0`), unlist(`15`))$estimate[2], Mean_trat16 = t.test(unlist(`0`),
        unlist(`16`))$estimate[2], Mean_trat17 = t.test(unlist(`0`), unlist(`17`))$estimate[2], Mean_trat18 = t.test(unlist(`0`),
        unlist(`18`))$estimate[2], Mean_trat19 = t.test(unlist(`0`), unlist(`19`))$estimate[2], Mean_trat20 = t.test(unlist(`0`),
        unlist(`20`))$estimate[2], Mean_trat21 = t.test(unlist(`0`), unlist(`21`))$estimate[2], Mean_trat22 = t.test(unlist(`0`),
        unlist(`22`))$estimate[2], Mean_trat23 = t.test(unlist(`0`), unlist(`23`))$estimate[2], Mean_trat24 = t.test(unlist(`0`),
        unlist(`24`))$estimate[2], Mean_trat25 = t.test(unlist(`0`), unlist(`25`))$estimate[2], Mean_trat26 = t.test(unlist(`0`),
        unlist(`26`))$estimate[2], Mean_trat27 = t.test(unlist(`0`), unlist(`27`))$estimate[2], Mean_trat28 = t.test(unlist(`0`),
        unlist(`28`))$estimate[2], Mean_trat29 = t.test(unlist(`0`), unlist(`29`))$estimate[2], Mean_trat30 = t.test(unlist(`0`),
        unlist(`30`))$estimate[2], Mean_trat31 = t.test(unlist(`0`), unlist(`31`))$estimate[2], Mean_trat32 = t.test(unlist(`0`),
        unlist(`32`))$estimate[2], Mean_trat33 = t.test(unlist(`0`), unlist(`33`))$estimate[2], Mean_trat34 = t.test(unlist(`0`),
        unlist(`34`))$estimate[2], Mean_trat35 = t.test(unlist(`0`), unlist(`35`))$estimate[2], Mean_trat36 = t.test(unlist(`0`),
        unlist(`36`))$estimate[2], Mean_trat37 = t.test(unlist(`0`), unlist(`37`))$estimate[2], Mean_trat38 = t.test(unlist(`0`),
        unlist(`38`))$estimate[2], Mean_trat39 = t.test(unlist(`0`), unlist(`39`))$estimate[2], Mean_trat40 = t.test(unlist(`0`),
        unlist(`40`))$estimate[2], Mean_trat41 = t.test(unlist(`0`), unlist(`41`))$estimate[2], Mean_trat42 = t.test(unlist(`0`),
        unlist(`42`))$estimate[2], Mean_trat43 = t.test(unlist(`0`), unlist(`43`))$estimate[2], Mean_trat44 = t.test(unlist(`0`),
        unlist(`44`))$estimate[2], Mean_trat45 = t.test(unlist(`0`), unlist(`45`))$estimate[2], Mean_trat46 = t.test(unlist(`0`),
        unlist(`46`))$estimate[2], Mean_trat47 = t.test(unlist(`0`), unlist(`47`))$estimate[2], Mean_trat48 = t.test(unlist(`0`),
        unlist(`48`))$estimate[2], Mean_trat49 = t.test(unlist(`0`), unlist(`49`))$estimate[2], Mean_trat50 = t.test(unlist(`0`),
        unlist(`50`))$estimate[2], Mean_trat51 = t.test(unlist(`0`), unlist(`51`))$estimate[2], Mean_trat52 = t.test(unlist(`0`),
        unlist(`52`))$estimate[2], Mean_trat53 = t.test(unlist(`0`), unlist(`53`))$estimate[2], Mean_trat54 = t.test(unlist(`0`),
        unlist(`54`))$estimate[2], Mean_trat55 = t.test(unlist(`0`), unlist(`55`))$estimate[2], Mean_trat56 = t.test(unlist(`0`),
        unlist(`56`))$estimate[2], p_value1 = t.test(unlist(`0`), unlist(`1`))$p.value, p_value2 = t.test(unlist(`0`),
        unlist(`2`))$p.value, p_value3 = t.test(unlist(`0`), unlist(`3`))$p.value, p_value4 = t.test(unlist(`0`),
        unlist(`4`))$p.value, p_value5 = t.test(unlist(`0`), unlist(`5`))$p.value, p_value6 = t.test(unlist(`0`),
        unlist(`6`))$p.value, p_value7 = t.test(unlist(`0`), unlist(`7`))$p.value, p_value8 = t.test(unlist(`0`),
        unlist(`8`))$p.value, p_value9 = t.test(unlist(`0`), unlist(`9`))$p.value, p_value10 = t.test(unlist(`0`),
        unlist(`10`))$p.value, p_value11 = t.test(unlist(`0`), unlist(`11`))$p.value, p_value12 = t.test(unlist(`0`),
        unlist(`12`))$p.value, p_value13 = t.test(unlist(`0`), unlist(`13`))$p.value, p_value14 = t.test(unlist(`0`),
        unlist(`14`))$p.value, p_value15 = t.test(unlist(`0`), unlist(`15`))$p.value, p_value16 = t.test(unlist(`0`),
        unlist(`16`))$p.value, p_value17 = t.test(unlist(`0`), unlist(`17`))$p.value, p_value18 = t.test(unlist(`0`),
        unlist(`18`))$p.value, p_value19 = t.test(unlist(`0`), unlist(`19`))$p.value, p_value20 = t.test(unlist(`0`),
        unlist(`20`))$p.value, p_value21 = t.test(unlist(`0`), unlist(`21`))$p.value, p_value22 = t.test(unlist(`0`),
        unlist(`22`))$p.value, p_value23 = t.test(unlist(`0`), unlist(`23`))$p.value, p_value24 = t.test(unlist(`0`),
        unlist(`24`))$p.value, p_value25 = t.test(unlist(`0`), unlist(`25`))$p.value, p_value26 = t.test(unlist(`0`),
        unlist(`26`))$p.value, p_value27 = t.test(unlist(`0`), unlist(`27`))$p.value, p_value28 = t.test(unlist(`0`),
        unlist(`28`))$p.value, p_value29 = t.test(unlist(`0`), unlist(`29`))$p.value, p_value30 = t.test(unlist(`0`),
        unlist(`30`))$p.value, p_value31 = t.test(unlist(`0`), unlist(`31`))$p.value, p_value32 = t.test(unlist(`0`),
        unlist(`32`))$p.value, p_value33 = t.test(unlist(`0`), unlist(`33`))$p.value, p_value34 = t.test(unlist(`0`),
        unlist(`34`))$p.value, p_value35 = t.test(unlist(`0`), unlist(`35`))$p.value, p_value36 = t.test(unlist(`0`),
        unlist(`36`))$p.value, p_value37 = t.test(unlist(`0`), unlist(`37`))$p.value, p_value38 = t.test(unlist(`0`),
        unlist(`38`))$p.value, p_value39 = t.test(unlist(`0`), unlist(`39`))$p.value, p_value40 = t.test(unlist(`0`),
        unlist(`40`))$p.value, p_value41 = t.test(unlist(`0`), unlist(`41`))$p.value, p_value42 = t.test(unlist(`0`),
        unlist(`42`))$p.value, p_value43 = t.test(unlist(`0`), unlist(`43`))$p.value, p_value44 = t.test(unlist(`0`),
        unlist(`44`))$p.value, p_value45 = t.test(unlist(`0`), unlist(`45`))$p.value, p_value46 = t.test(unlist(`0`),
        unlist(`46`))$p.value, p_value47 = t.test(unlist(`0`), unlist(`47`))$p.value, p_value48 = t.test(unlist(`0`),
        unlist(`48`))$p.value, p_value49 = t.test(unlist(`0`), unlist(`49`))$p.value, p_value50 = t.test(unlist(`0`),
        unlist(`50`))$p.value, p_value51 = t.test(unlist(`0`), unlist(`51`))$p.value, p_value52 = t.test(unlist(`0`),
        unlist(`52`))$p.value, p_value53 = t.test(unlist(`0`), unlist(`53`))$p.value, p_value54 = t.test(unlist(`0`),
        unlist(`54`))$p.value, p_value55 = t.test(unlist(`0`), unlist(`55`))$p.value, p_value56 = t.test(unlist(`0`),
        unlist(`56`))$p.value)


################## Balance por msj
balance_msj <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:CELULAR_OK, ING_MENSUAL:SDOPROM_CBOLSA,
    TXN_ATM, TXN_SUC, TXN_MON_BXI, TXN_NOMON_BXI, TXN_CORRESPONSALES:BEM, SDO_VIG_CA:EGRESOS_RASTREABLES,
    -SDOTDC_OFFUS, msj) %>% gather(key = variables, value = value, -msj) %>% group_by(msj, variables) %>%
    summarise(value = list(value)) %>% spread(key = msj, value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(`0`),
    unlist(`1`), )$estimate[1], Mean_trat1 = t.test(unlist(`0`), unlist(`1`))$estimate[2], Mean_trat2 = t.test(unlist(`0`),
    unlist(`2`))$estimate[2], Mean_trat3 = t.test(unlist(`0`), unlist(`3`))$estimate[2], Mean_trat4 = t.test(unlist(`0`),
    unlist(`4`))$estimate[2], Mean_trat5 = t.test(unlist(`0`), unlist(`5`))$estimate[2], Mean_trat6 = t.test(unlist(`0`),
    unlist(`6`))$estimate[2], Mean_trat7 = t.test(unlist(`0`), unlist(`7`))$estimate[2], p_value1 = t.test(unlist(`0`),
    unlist(`1`))$p.value, p_value2 = t.test(unlist(`0`), unlist(`2`))$p.value, p_value3 = t.test(unlist(`0`),
    unlist(`3`))$p.value, p_value4 = t.test(unlist(`0`), unlist(`4`))$p.value, p_value5 = t.test(unlist(`0`),
    unlist(`5`))$p.value, p_value6 = t.test(unlist(`0`), unlist(`6`))$p.value, p_value7 = t.test(unlist(`0`),
    unlist(`7`))$p.value)


######################### Balance por periodicidad
balance_periodicidad <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:CELULAR_OK, ING_MENSUAL:SDOPROM_CBOLSA,
    TXN_ATM, TXN_SUC, TXN_MON_BXI, TXN_NOMON_BXI, TXN_CORRESPONSALES:BEM, SDO_VIG_CA:EGRESOS_RASTREABLES,
    -SDOTDC_OFFUS, periodicidad) %>% gather(key = variables, value = value, -periodicidad) %>% group_by(periodicidad,
    variables) %>% summarise(value = list(value)) %>% spread(key = periodicidad, value) %>% group_by(variables) %>%
    transmute(Mean_control = t.test(unlist(control), unlist(quincenal))$estimate[1], Mean_quincenal = t.test(unlist(control),
        unlist(quincenal))$estimate[2], Mean_semanal = t.test(unlist(control), unlist(semanal))$estimate[2],
        p_value_quincenal = t.test(unlist(control), unlist(quincenal))$p.value, p_value_semanal = t.test(unlist(control),
            unlist(semanal))$p.value)


#################################### Balance por descanso vs no descanso
balance_descanso <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:CELULAR_OK, ING_MENSUAL:SDOPROM_CBOLSA,
    TXN_ATM, TXN_SUC, TXN_MON_BXI, TXN_NOMON_BXI, TXN_CORRESPONSALES:BEM, SDO_VIG_CA:EGRESOS_RASTREABLES,
    -SDOTDC_OFFUS, descanso) %>% gather(key = variables, value = value, -descanso) %>% group_by(descanso,
    variables) %>% summarise(value = list(value)) %>% spread(key = descanso, value) %>% group_by(variables) %>%
    transmute(Mean_control = t.test(unlist(Control), unlist(`0`))$estimate[1], Mean_sin_descanso = t.test(unlist(Control),
        unlist(`0`))$estimate[2], Mean_con_descanso = t.test(unlist(Control), unlist(`1`))$estimate[2],
        p_value_sin_descanso = t.test(unlist(Control), unlist(`0`))$p.value, p_value_con_descanso = t.test(unlist(Control),
            unlist(`1`))$p.value)



#################################### Balance por mismo vs diferente
balance_rpt_msj <- universo %>% select(SALDODISVAL, EDAD, ANTIG_MESES, USO_BXI:CELULAR_OK, ING_MENSUAL:SDOPROM_CBOLSA,
    TXN_ATM, TXN_SUC, TXN_MON_BXI, TXN_NOMON_BXI, TXN_CORRESPONSALES:BEM, SDO_VIG_CA:EGRESOS_RASTREABLES,
    -SDOTDC_OFFUS, repeticion_msj) %>% gather(key = variables, value = value, -repeticion_msj) %>%
    group_by(repeticion_msj, variables) %>% summarise(value = list(value)) %>% spread(key = repeticion_msj,
    value) %>% group_by(variables) %>% transmute(Mean_control = t.test(unlist(Control), unlist(Diferente))$estimate[1],
    Mean_diferente = t.test(unlist(Control), unlist(Diferente))$estimate[2], Mean_mismo = t.test(unlist(Control),
        unlist(Mismo))$estimate[2], p_value_diferente = t.test(unlist(Control), unlist(Diferente))$p.value,
    p_value_mismo = t.test(unlist(Control), unlist(Mismo))$p.value)


balances <- list(por_trat = balance_trat, por_msj = balance_msj, por_periodicidad = balance_periodicidad,
    por_descanso = balance_descanso, por_rpt_msj = balance_rpt_msj)


write.xlsx(balances, file = "balances_banco_principal.xlsx")

rm(balance_descanso, balance_msj, balance_periodicidad, balance_rpt_msj, balance_trat)
rm(balances)
rm(universo_banco_principal)

# graficas de balances para las variables principales

# Por tratamiento
universo <- universo %>% mutate(log_saldo_vista = log(SALDODISVAL + 1))

universo <- universo %>% gather(log_saldo_vista, ANTIG_MESES, share_gastos_rastreables, EGRESOS_TOTALES,
    EGRESOS_RASTREABLES, TXN_ATM, key = "variable", value = "valor")


universo$msj <- as.factor(as.character(universo$msj))

ggplot(universo) + geom_boxplot(aes(msj, valor)) + facet_wrap(~variable, scales = "free") + theme_bw() +
    labs(title = "Distribucion de variables mas relevantes", subtitle = "Por grupo de tratamiento")

