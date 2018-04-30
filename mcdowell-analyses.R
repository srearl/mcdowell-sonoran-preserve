
# README ------------------------------------------------------------------


# setup: libraries and connection -----------------------------------------

library(RMySQL)
library(tidyverse)
library(vegan)
library(lubridate)

source('~/Documents/localSettings/mysql_rds_prod.R')
mcd <- mysql_rds_connect('mcdowell_arthropods')
core <- mysql_rds_connect('lter10_arthropods_production')


# McDowell total number of orgs by taxon ----------------------------------

# Use mcdTotals to get summary stats: total number of orgs and breakdown of
# numbers by taxonomic level.

mcdTotals <- dbGetQuery(mcd, "
SELECT trap_specimens.arthropod_taxon_id,
  sum(
    IF(isnull(lt2mm), 0, lt2mm) +
    IF(isnull(_2_5mm), 0, _2_5mm) +
    IF(isnull(_5_10mm), 0, _5_10mm) +
    IF(isnull(gt10mm), 0, gt10mm) +
    IF(isnull(unsized), 0, unsized)
  ) AS allsizes,
  # sum(trap_specimens.unsized),
  arthropod_taxonomies.arth_class,
  arthropod_taxonomies.arth_order,
  arthropod_taxonomies.arth_family,
  arthropod_taxonomies.arth_genus_subgenus,
  arthropod_taxonomies.display_name
FROM
trap_specimens
JOIN
  arthropod_taxonomies ON (arthropod_taxonomies.arthropod_taxon_id = trap_specimens.arthropod_taxon_id)
GROUP BY 
  trap_specimens.arthropod_taxon_id
ORDER BY 
  arth_class,
  arth_order,
  arth_family,
  arth_genus_subgenus;")


# McDowell & AD10 cumulative averages -----------------------------------

# Calculate cumulative number of organisms per cumulative number of traps for
# both the McDowell and PO10 sites. core_average_all query is restricted to
# sampling events > 2012-01-01 to more closely reflect the approximate start
# date (Feb 2012) of the McDowell sampling.

mcdowell_average_all <- dbGetQuery(mcd, "
SELECT
  se.site_id,
  s.sitename,
  sub.trapcount,
  SUM(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
    IF(isnull(_2_5mm), 0, _2_5mm) +
    IF(isnull(_5_10mm), 0, _5_10mm) +
    IF(isnull(gt10mm), 0, gt10mm) +
    IF(isnull(unsized), 0, unsized)
    )) as allsizes,
  SUM(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
    IF(isnull(_2_5mm), 0, _2_5mm) +
    IF(isnull(_5_10mm), 0, _5_10mm) +
    IF(isnull(gt10mm), 0, gt10mm) +
    IF(isnull(unsized), 0, unsized)
  ))/sub.trapcount as average
FROM trap_specimens ts
JOIN trap_sampling_events tse ON (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
JOIN sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN sites s ON (s.site_id = se.site_id)
JOIN
(
  SELECT
      se.site_id,
      count(tse.`trap_sampling_event_id`) as trapcount
  FROM trap_sampling_events tse
  JOIN sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
  JOIN sites s ON (s.site_id = se.site_id)
  WHERE coalesce(tse.flags, '') = ''
  GROUP BY se.site_id
) as sub ON (sub.site_id = se.site_id)
GROUP BY se.site_id;")


core_average_all <- dbGetQuery(core, "
SELECT
se.site_id,
s.sitename,
sub.trapcount,
SUM(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  )) as allsizes,
SUM(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  ))/sub.trapcount as average
FROM trap_specimens ts
JOIN trap_sampling_events tse ON (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
JOIN sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN sites s ON (s.site_id = se.site_id)
JOIN
(
  SELECT
    se.site_id,
    count(tse.`trap_sampling_event_id`) as trapcount
  FROM trap_sampling_events tse
  JOIN sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
  JOIN sites s ON (s.site_id = se.site_id)
  WHERE 
    coalesce(tse.flags, '') = '' AND
    se.sample_date > '2012-01-01'
  GROUP BY se.site_id
) AS sub ON (sub.site_id = se.site_id)
WHERE 
  se.sample_date > '2012-01-01'
GROUP BY se.site_id;") %>%
  filter(sitename == 'AD-10')
  
cumulative_average <- bind_rows(mcdowell_average_all, core_average_all) %>% 
  mutate(
    position = case_when(
      sitename %in% c('Paraiso', 'Bell', 'Dixileta', 'Mine', 'Rincon') ~ 'boundary',
      TRUE ~ 'interior'
    ),
    region = case_when(
      sitename %in% c('Dixileta', 'LoneMtn') ~ 'Browns',
      sitename %in% c('TomThumb', 'Paraiso') ~ 'Toms',
      sitename %in% c('Gateway', 'Bell') ~ 'Gateway',
      sitename %in% c('Sunrise', 'Rincon') ~ 'Lost Dog',
      sitename %in% c('Mine', 'Prospector') ~ 'Dixie Mine',
      sitename %in% c('AD-10') ~ 'MMRP'
    )
  )

# make region a factor with levels corresponding to the desired order of the
# plot facets
cumulative_average$region <- factor(cumulative_average$region,
                                    levels = c('Browns', 'Toms', 'Dixie Mine', 'Gateway', 'Lost Dog', 'MMRP'))  

ggplot(cumulative_average, aes(x = sitename, y = average, fill = position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4) +
  ggtitle('average number of organisms across sampling locations') + 
  xlab('sampling location') +
  ylab('cumulative organisms per\ncumulative traps') +
  scale_x_discrete(labels = abbreviate) + # abbreviate site names
  facet_grid(. ~ region, scales = 'free', space = 'free_x') +
  geom_segment(aes(x=0.5, y=10.97, xend=2.5, yend=10.97), lty='dotted') + # global mean value (not included)
  scale_fill_manual(values = c('green2', 'Grey')) +
  # guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  theme(
    axis.text.x = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
    #         axis.title.y = element_blank(),
    #         axis.ticks.y = element_line(size = 1, colour = 'black'),
    #         axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #         panel.background = element_blank(),
    #         # panel.border=element_blank(),
    #         #legend.text = element_blank(),
    #         legend.text = element_text(colour = 'black', size = 10),
    #         #legend.title = element_blank(),
    #         legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

ggsave("~/Desktop/cumulative_average.png")
unlink(cumulative_average)


# McDowell & AD10 annual averages ------------------------------

# Calculate cumulative number of organisms per cumulative number of traps for
# both the McDowell and PO10 sites by year. Date range is restricted to 2012 -
# 2017.


mcdowell_dataset <- dbGetQuery(mcd, "
SELECT  
  se.sampling_event_id,
  s.site_code,
  se.sample_date,
  count_data.trap_count,
  specimen_data.arth_class,
  specimen_data.arth_order,
  specimen_data.arth_family,
  specimen_data.arth_genus_subgenus,
  specimen_data.display_name,
  specimen_data.lt2mm,
  specimen_data._2_5mm,
  specimen_data._5_10mm,
  specimen_data.gt10mm,
  specimen_data.unsized
FROM mcdowell_arthropods.trap_sampling_events tse
JOIN mcdowell_arthropods.sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN mcdowell_arthropods.sites s ON (s.site_id = se.site_id)
JOIN mcdowell_arthropods.traps t ON (tse.trap_id = t.trap_id)
LEFT JOIN mcdowell_arthropods.people outer_join_people ON (outer_join_people.person_id = se.default_person_for_trap_samples)
LEFT JOIN
(
  SELECT
    tse.trap_sampling_event_id,
    p.observer,
    tax.arth_class,
    tax.arth_order,
    tax.arth_family,
    tax.arth_genus_subgenus,
    tax.display_name,
    ts.lt2mm,
    ts._2_5mm,
    ts._5_10mm,
    ts.gt10mm,
    ts.unsized
  FROM mcdowell_arthropods.trap_specimens ts
  JOIN mcdowell_arthropods.arthropod_taxonomies tax ON (tax.arthropod_taxon_id = ts.arthropod_taxon_id)
  JOIN mcdowell_arthropods.trap_sampling_events tse ON (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
  JOIN mcdowell_arthropods.people p ON (p.person_id = ts.person_id)
) AS specimen_data ON (specimen_data.trap_sampling_event_id = tse.trap_sampling_event_id)
JOIN
(
  SELECT
    trap_sampling_events.sampling_event_id,
    COUNT(DISTINCT trap_sampling_events.trap_sampling_event_id) AS trap_count
  FROM mcdowell_arthropods.trap_sampling_events
  WHERE NOT 
  (
    trap_sampling_events.flags LIKE 'trap not collected' OR 
    trap_sampling_events.flags LIKE 'NotCollected'
  ) OR 
  trap_sampling_events.flags IS NULL OR
  trap_sampling_events.flags = ''
  GROUP BY sampling_event_id
) AS count_data ON (count_data.sampling_event_id = tse.sampling_event_id)
ORDER BY se.sample_date, s.site_code;")


core_dataset <- dbGetQuery(core, "
SELECT  
  se.sampling_event_id,
  s.site_code, 
  se.sample_date,
  count_data.trap_count,
  specimens_data.arth_class,
  specimens_data.arth_order,
  specimens_data.arth_family,
  specimens_data.arth_genus_subgenus,
  specimens_data.display_name,
  specimens_data.lt2mm,
  specimens_data._2_5mm,
  specimens_data._5_10mm,
  specimens_data.gt10mm,
  specimens_data.unsized
FROM lter10_arthropods_production.trap_sampling_events tse
JOIN lter10_arthropods_production.sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN lter10_arthropods_production.sites s ON (s.site_id = se.site_id)
JOIN lter10_arthropods_production.traps t ON (tse.trap_id = t.trap_id)
LEFT JOIN lter10_arthropods_production.people outer_join_people ON (outer_join_people.person_id = se.default_person_for_trap_samples)
LEFT JOIN lter10_arthropods_production.specimens_data ON (specimens_data.trap_sampling_event_id = tse.trap_sampling_event_id)
JOIN
(
  SELECT
  trap_sampling_events.sampling_event_id,
  COUNT(DISTINCT trap_sampling_events.trap_sampling_event_id) AS trap_count
  FROM lter10_arthropods_production.trap_sampling_events
  WHERE NOT 
  (
    trap_sampling_events.flags LIKE 'trap not collected' OR 
    trap_sampling_events.flags LIKE 'NotCollected' OR
    trap_sampling_events.flags LIKE 'missing'
  ) OR 
  trap_sampling_events.flags IS NULL OR
  trap_sampling_events.flags = ''
  GROUP BY sampling_event_id
) AS count_data ON (count_data.sampling_event_id = tse.sampling_event_id)
WHERE 
  site_code LIKE 'AD-10'
ORDER BY se.sample_date, s.site_code;")

mcdowell_annual_avg <- mcdowell_dataset %>%
  rowwise() %>% 
  mutate(
    sample_date = as.Date(sample_date, format = "%Y-%m-%d"),
    allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)
  ) %>%
  ungroup() %>% 
  group_by(sampling_event_id) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    all_orgs = sum(allsizes, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  group_by(site_code, year(sample_date)) %>% 
  summarise(
    ann_avg = (sum(all_orgs)/sum(trap_count))
  )

core_annual_avg <- core_dataset %>% 
  rowwise() %>% 
  mutate(
    sample_date = as.Date(sample_date, format = "%Y-%m-%d"),
    allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)
  ) %>%
  ungroup() %>% 
  group_by(sampling_event_id) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    all_orgs = sum(allsizes, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  group_by(site_code, year(sample_date)) %>% 
  summarise(
    ann_avg = (sum(all_orgs)/sum(trap_count))
  )

annual_average <- rbind(core_annual_avg, mcdowell_annual_avg) %>% 
  rename(
    sitename = site_code,
    year = `year(sample_date)`
  ) %>% 
  mutate(
    position = case_when(
      sitename %in% c('Paraiso', 'Bell', 'Dixileta', 'Mine', 'Rincon') ~ 'boundary',
      TRUE ~ 'interior'
    ),
    region = case_when(
      sitename %in% c('Dixileta', 'LoneMtn') ~ 'Browns',
      sitename %in% c('TomThumb', 'Paraiso') ~ 'Toms',
      sitename %in% c('Gateway', 'Bell') ~ 'Gateway',
      sitename %in% c('Sunrise', 'Rincon') ~ 'Lost Dog',
      sitename %in% c('Mine', 'Prospector') ~ 'Dixie Mine',
      sitename %in% c('AD-10') ~ 'MMRP'
    )
  ) %>% 
  filter(year >= 2012 & year < 2017)

# make region a factor with levels corresponding to the desired order of the
# plot facets
annual_average$region <- factor(annual_average$region,
                                levels = c('Browns', 'Toms', 'Dixie Mine', 'Gateway', 'Lost Dog', 'MMRP'))  

ggplot(annual_average, aes(x = year, y = ann_avg)) +
  geom_point(aes(color = position), size = 2) +
  geom_line(aes(color = position)) +
  # scale_colour_manual(values = c('green2', 'Grey')) +
  scale_colour_manual(values = c('green2', 'grey47')) +
  ggtitle('average number of organisms: location + year') + 
  xlab('year') +
  ylab('cumulative organisms per\ncumulative traps') +
  facet_grid(region ~ ., switch = 'y') +
  # facet_grid(region ~ ., switch = 'y') +
  theme(
    axis.text.x = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_blank(),
    # axis.title.y = element_blank(),
    # axis.ticks.y = element_line(size = 1, colour = 'black'),
    # axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    # panel.border=element_blank(),
    # legend.text = element_blank(),
    # legend.text = element_text(colour = 'black', size = 10),
    # legend.title = element_blank(),
    # legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

ggsave("~/Desktop/ann_average.png")
unlink(annual_average)


# McDowell & AD10 annual diversity -----------------------------

mcdowell_wide <- mcdowell_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  group_by(site_code, year(sample_date), display_name) %>% 
  summarise(ann_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = ann_avg) %>% 
  rename(year = `year(sample_date)`)

core_wide <- core_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  group_by(site_code, year(sample_date), display_name) %>% 
  summarise(ann_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = ann_avg) %>% 
  rename(year = `year(sample_date)`)

all_wide <- bind_rows(mcdowell_wide, core_wide)

all_wide[is.na(all_wide)] <- 0 # NAs to 0
all_wide$shannon <- diversity(log1p(all_wide[,-c(1,2)])) # create index
all_wide <- all_wide %>% 
  select(site_code, year, shannon)

all_wide <- all_wide %>% 
  rename(sitename = site_code) %>% 
  mutate(
    position = case_when(
      sitename %in% c('Paraiso', 'Bell', 'Dixileta', 'Mine', 'Rincon') ~ 'boundary',
      TRUE ~ 'interior'
    ),
    region = case_when(
      sitename %in% c('Dixileta', 'LoneMtn') ~ 'Browns',
      sitename %in% c('TomThumb', 'Paraiso') ~ 'Toms',
      sitename %in% c('Gateway', 'Bell') ~ 'Gateway',
      sitename %in% c('Sunrise', 'Rincon') ~ 'Lost Dog',
      sitename %in% c('Mine', 'Prospector') ~ 'Dixie Mine',
      sitename %in% c('AD-10') ~ 'MMRP'
    )
  ) %>% 
  filter(year >= 2012 & year < 2017)

# make region a factor with levels corresponding to the desired order of the
# plot facets
all_wide$region <- factor(all_wide$region,
                          levels = c('Browns', 'Toms', 'Dixie Mine', 'Gateway', 'Lost Dog', 'MMRP'))  

ggplot(all_wide, aes(x = year, y = shannon)) +
  geom_point(aes(color = position), size = 2) +
  geom_line(aes(color = position)) +
  scale_y_continuous(limits = c(1.5,3.9), breaks = c(2,3,4)) +
  scale_colour_manual(values = c('green2', 'grey47')) +
  ggtitle('diversity: location + year') + 
  xlab('year') +
  ylab('Shannon\'s diversity index (H)') +
  facet_grid(region ~ ., switch = 'y') +
  theme(
    axis.text.x = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_blank(),
    # axis.title.y = element_blank(),
    # axis.ticks.y = element_line(size = 1, colour = 'black'),
    # axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    # panel.border=element_blank(),
    # legend.text = element_blank(),
    # legend.text = element_text(colour = 'black', size = 10),
    # legend.title = element_blank(),
    # legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )


ggsave("~/Desktop/all_wide.png")
unlink(all_wide)



# McDowell & AD10 cumulative diversity ------------------------------------


mcdowell_wide <- mcdowell_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  group_by(site_code, display_name) %>% 
  summarise(cum_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = cum_avg)

core_wide <- core_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  filter(year(sample_date) >= 2012 & year(sample_date) <= 2016) %>% # contrain date range
  group_by(site_code, display_name) %>% 
  summarise(cum_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = cum_avg)

all_wide <- bind_rows(mcdowell_wide, core_wide)

all_wide[is.na(all_wide)] <- 0 # NAs to 0
all_wide$shannon <- diversity(log1p(all_wide[,-c(1,2)])) # create index
all_wide <- all_wide %>% 
  select(site_code, shannon)


all_wide <- all_wide %>% 
  rename(sitename = site_code) %>% 
  mutate(
    position = case_when(
      sitename %in% c('Paraiso', 'Bell', 'Dixileta', 'Mine', 'Rincon') ~ 'boundary',
      TRUE ~ 'interior'
    ),
    region = case_when(
      sitename %in% c('Dixileta', 'LoneMtn') ~ 'Browns',
      sitename %in% c('TomThumb', 'Paraiso') ~ 'Toms',
      sitename %in% c('Gateway', 'Bell') ~ 'Gateway',
      sitename %in% c('Sunrise', 'Rincon') ~ 'Lost Dog',
      sitename %in% c('Mine', 'Prospector') ~ 'Dixie Mine',
      sitename %in% c('AD-10') ~ 'MMRP'
    )
  )

# make region a factor with levels corresponding to the desired order of the
# plot facets
all_wide$region <- factor(all_wide$region,
                          levels = c('Browns', 'Toms', 'Dixie Mine', 'Gateway', 'Lost Dog', 'MMRP'))  

ggplot(all_wide, aes(x = sitename, y = shannon, fill = position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4) +
  # coord_cartesian(ylim=c(3.5,4.5)) +
  ggtitle('diversity of organisms across sampling locations') + 
  xlab('sampling location') +
  ylab('Shannon\'s diversity index (H)') +
  scale_x_discrete(labels = abbreviate) + # abbreviate site names
  facet_grid(. ~ region, scales = 'free', space = 'free_x') +
  geom_segment(aes(x=0.5, y=4.23, xend=2.5, yend=4.23), lty='dotted') + # global mean value (not included)
  scale_fill_manual(values = c('green2', 'Grey')) +
  # guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  theme(
    axis.text.x = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=8, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
    #         axis.title.y = element_blank(),
    #         axis.ticks.y = element_line(size = 1, colour = 'black'),
    #         axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #         panel.background = element_blank(),
    #         # panel.border=element_blank(),
    #         #legend.text = element_blank(),
    #         legend.text = element_text(colour = 'black', size = 10),
    #         #legend.title = element_blank(),
    #         legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

ggsave("~/Desktop/all_wide.png")
unlink(all_wide)


# McDowell & AD10 cumulative NMDS -----------------------------------------


mcdowell_wide <- mcdowell_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  group_by(site_code, display_name) %>% 
  summarise(cum_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = cum_avg)

core_wide <- core_dataset %>%
  filter(!is.na(display_name)) %>% 
  mutate(display_name = trimws(display_name, which = c("both"))) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sampling_event_id, display_name) %>% 
  summarise(
    site_code = first(site_code),
    sample_date = first(sample_date),
    trap_count = first(trap_count),
    allsizes = sum(allsizes)
  ) %>% 
  ungroup() %>% 
  mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>% 
  filter(year(sample_date) >= 2012 & year(sample_date) <= 2016) %>% # contrain date range
  group_by(site_code, display_name) %>% 
  summarise(cum_avg = (sum(allsizes)/sum(trap_count))) %>% 
  ungroup() %>% 
  spread(key = display_name, value = cum_avg)

all_wide <- bind_rows(mcdowell_wide, core_wide)

all_wide[is.na(all_wide)] <- 0 # NAs to 0
nmds <- metaMDS(all_wide[,-c(1,2)])
ordiplot(nmds, type = "text", display = "sites")

