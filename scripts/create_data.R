################################################################################
# DATOS MODULO CARRUSEL
#
# Autora: Gabriela Mathieu
# Fuente: https://github.com/calcita/compras_publicas
################################################################################
# CUANTO: BAR-------------------------------------------------------------------
data_bar_1 <- bar_data(compras_resumen_so, 2017)
readr::write_csv(data_bar_1, "data/data_bar_1.csv")

data_bar_2 <- bar_data(compras_resumen_so, 2018)
readr::write_csv(data_bar_2, "data/data_bar_2.csv")

data_bar_3 <- bar_data(compras_resumen_so, 2019)
readr::write_csv(data_bar_3, "data/data_bar_3.csv")

data_bar_4 <- bar_data(compras_resumen_so, 2020)
readr::write_csv(data_bar_4, "data/data_bar_4.csv")

data_bar_5 <- bar_data(compras_resumen_so, 2021)
readr::write_csv(data_bar_5, "data/data_bar_5.csv")

# COMO: TIPO DE COMPRA ---------------------------------------------------------
data_bartime <- bartime_data()
readr::write_csv(data_bartime, "data/data_bartime.csv")

# CUANDO: CALENDARIO
d1 <- calendar_data(year = 2017, category = "Administración Central")
d2 <- calendar_data(year = 2018, category = "Administración Central")
d3 <- calendar_data(year = 2019, category = "Administración Central")
d4 <- calendar_data(year = 2020, category = "Administración Central")
d5 <- calendar_data(year = 2021, category = "Administración Central")
data_heatmap_1 <- bind_rows(d1, d2, d3, d4, d5)
readr::write_csv(data_heatmap_1, "data/data_heatmap_1.csv")

d1 <- calendar_data(year = 2017, category = "Gobiernos Departamentales")
d2 <- calendar_data(year = 2018, category = "Gobiernos Departamentales")
d3 <- calendar_data(year = 2019, category = "Gobiernos Departamentales")
d4 <- calendar_data(year = 2020, category = "Gobiernos Departamentales")
d5 <- calendar_data(year = 2021, category = "Gobiernos Departamentales")
data_heatmap_2 <- bind_rows(d1, d2, d3, d4, d5)
readr::write_csv(data_heatmap_2, "data/data_heatmap_2.csv")

d1 <- calendar_data(year = 2017, category = "Entes autónomos")
d2 <- calendar_data(year = 2018, category = "Entes autónomos")
d3 <- calendar_data(year = 2019, category = "Entes autónomos")
d4 <- calendar_data(year = 2020, category = "Entes autónomos")
d5 <- calendar_data(year = 2021, category = "Entes autónomos")
data_heatmap_3 <- bind_rows(d1, d2, d3, d4, d5)
readr::write_csv(data_heatmap_3, "data/data_heatmap_3.csv")

d1 <- calendar_data(year = 2017, category = "Servicios descentralizados")
d2 <- calendar_data(year = 2018, category = "Servicios descentralizados")
d3 <- calendar_data(year = 2019, category = "Servicios descentralizados")
d4 <- calendar_data(year = 2020, category = "Servicios descentralizados")
d5 <- calendar_data(year = 2021, category = "Servicios descentralizados")
data_heatmap_4 <- bind_rows(d1, d2, d3, d4, d5)
readr::write_csv(data_heatmap_4, "data/data_heatmap_4.csv")

d1 <- calendar_data(year = 2017, category = "Otros")
d2 <- calendar_data(year = 2018, category = "Otros")
d3 <- calendar_data(year = 2019, category = "Otros")
d4 <- calendar_data(year = 2020, category = "Otros")
d5 <- calendar_data(year = 2021, category = "Otros")
data_heatmap_5 <- bind_rows(d1, d2, d3, d4, d5)
readr::write_csv(data_heatmap_5, "data/data_heatmap_5.csv")

# A QUIEN COMPRA: SANKEY -------------------------------------------------------
# plot_sankey_1
data_sankey_1 <- sankey_data(organismo = "Presidencia", year = 2017, top = 5)
readr::write_csv(data_sankey_1, "data/data_sankey_1.csv")

# plot_sankey_2
data_sankey_2 <- sankey_data(organismo = "Presidencia", year = 2018, top = 5)
readr::write_csv(data_sankey_2, "data/data_sankey_2.csv")

# plot_sankey_3
data_sankey_3 <- sankey_data(organismo = "Presidencia", year = 2019, top = 5)
readr::write_csv(data_sankey_3, "data/data_sankey_3.csv")

# plot_sankey_4
data_sankey_4 <- sankey_data(organismo = "Presidencia", year = 2020, top = 5)
readr::write_csv(data_sankey_4, "data/data_sankey_4.csv")

# plot_sankey_5
data_sankey_5 <- sankey_data(organismo = "Presidencia", year = 2021, top = 5)
readr::write_csv(data_sankey_5, "data/data_sankey_5.csv")

# A QUIEN VENDE: SANKEY --------------------------------------------------------
# plot_sankey_v1
data_sankey_v1 <- sankey_data_prov(year = 2017, top = 10) %>%
  top_n(total, n = 10) %>%
  arrange(desc(total))
readr::write_csv(data_sankey_v1, "data/data_sankey_v1.csv")

# plot_sankey_v2
data_sankey_v2 <- sankey_data_prov(year = 2018, top = 10) %>%
  top_n(total, n = 10) %>%
  arrange(desc(total))
readr::write_csv(data_sankey_v2, "data/data_sankey_v2.csv")

# plot_sankey_v3
data_sankey_v3 <- sankey_data_prov(year = 2019, top = 10) %>%
  top_n(total, n = 10) %>%
  arrange(desc(total))
readr::write_csv(data_sankey_v3, "data/data_sankey_v3.csv")

# plot_sankey_v4
data_sankey_v4 <- sankey_data_prov(year = 2020, top = 10) %>%
  top_n(total, n = 10) %>%
  arrange(desc(total))
readr::write_csv(data_sankey_v4, "data/data_sankey_v4.csv")

# plot_sankey_v5
data_sankey_v5 <- sankey_data_prov(year = 2021, top = 10) %>%
  top_n(total, n = 10) %>%
  arrange(desc(total))
readr::write_csv(data_sankey_v5, "data/data_sankey_v5.csv")

# QUE COMPRA: TREEMAPS ---------------------------------------------------------
# t1
data_treemap_1 <- treemap_data_arts(year = 2017) %>% 
  treemap_data(var1 = 'articulo', var2 = 'organismo_sigla', counts = 'Total') 
readr::write_csv(data_treemap_1, "data/data_treemap_1.csv")

# t2
data_treemap_2 <- treemap_data_arts(year = 2018) %>% 
  treemap_data(var1 = 'articulo', var2 = 'organismo_sigla', counts = 'Total') 
readr::write_csv(data_treemap_2, "data/data_treemap_2.csv")

# t3
data_treemap_3 <- treemap_data_arts(year = 2019) %>% 
  treemap_data(var1 = 'articulo', var2 = 'organismo_sigla', counts = 'Total') 
readr::write_csv(data_treemap_3, "data/data_treemap_3.csv")

# t4
data_treemap_4 <- treemap_data_arts(year = 2020) %>% 
  treemap_data(var1 = 'articulo', var2 = 'organismo_sigla', counts = 'Total') 
readr::write_csv(data_treemap_4, "data/data_treemap_4.csv")

# t5
data_treemap_5 <- treemap_data_arts(year = 2021) %>% 
  treemap_data(var1 = 'articulo', var2 = 'organismo_sigla', counts = 'Total') 
readr::write_csv(data_treemap_5, "data/data_treemap_5.csv")

# A CUANTO COMPRA: VIOLIN ------------------------------------------------------
# plot_violin_1
data_violin_1 <- violin_data(item = "NOTEBOOK")
readr::write_csv(data_violin_1, "data/data_violin_1.csv")

# plot_violin_2
data_violin_2 <- violin_data(item = "TAPABOCA")
readr::write_csv(data_violin_2, "data/data_violin_2.csv")

# plot_violin_3
data_violin_3 <- violin_data(item = "AGUA MINERAL")
readr::write_csv(data_violin_3, "data/data_violin_3.csv")

# plot_violin_4
data_violin_4 <- violin_data(item = "CAFE")
readr::write_csv(data_violin_4, "data/data_violin_4.csv")

# plot_violin_5
data_violin_5 <- violin_data(item = "TONER PARA FOTOCOPIADORA")
readr::write_csv(data_violin_5, "data/data_violin_5.csv")
