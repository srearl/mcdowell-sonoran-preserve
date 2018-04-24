
# README ------------------------------------------------------------------


# setup: libraries and connection -----------------------------------------

library(RMySQL)
library(tidyverse)
library(vegan)

source('~/Documents/localSettings/mysql_rds_prod.R')
mcd <- mysql_rds_connect('mcdowell_arthropods')
core <- mysql_rds_connect('lter10_arthropods_production')


# McDowell total number of orgs by taxon ----------------------------------

# Use mcdTotals to get total number of orgs and breakdown by taxonomic level.

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


# McDowell and AD10 cumulative averages -----------------------------------

# Calculate cumulative number of organisms per cumulative number of traps for
# both the McDowell and PO10 sites. PO10 query is restricted to sampling events
# > 2012-01-01 to more closely reflect the approximate start date (Feb 2012) of
# the McDowell sampling.

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
      sitename %in% c('Dixileta', 'LoneMtn') ~ 'Brown\'s Ranch',
      sitename %in% c('TomThumb', 'Paraiso') ~ 'Tom\'s Thumb',
      sitename %in% c('Gateway', 'Bell') ~ 'Gateway',
      sitename %in% c('Sunrise', 'Rincon') ~ 'Lost Dog',
      sitename %in% c('Mine', 'Prospector') ~ 'Dixie Mine',
      sitename %in% c('AD-10') ~ 'MMRP'
    )
  )

# make region a factor with levels corresponding to the desired order of the
# plot facets
cumulative_average$region <- factor(cumulative_average$region,
                                    levels = c('Brown\'s Ranch', 'Tom\'s Thumb', 'Dixie Mine', 'Gateway', 'Lost Dog', 'MMRP'))  

ggplot(cumulative_average, aes(x = sitename, y = average, fill = position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4) +
  ggtitle('average number of organisms across sampling locations') + 
  xlab('region') +
  ylab('cumulative organisms per\ncumulative traps') +
  scale_x_discrete(labels = abbreviate) + # abbreviate site names
  facet_grid(. ~ region, scales = 'free', space = 'free_x') +
  # geom_segment(aes(x=0.5, y=10.72897, xend=2.5, yend=10.72897), lty='dotted') + # global mean value (not included)
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


# from dataset query ------------------------------------------------------


mcdowell_dataset <- dbGetQuery(mcd, "
SELECT  
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

# what the heck?

fromfile %>%
  filter(trapcount < 50) %>% 
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% View()

mcdowell_dataset %>%
  rowwise() %>% 
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>% 
  ungroup() %>% View()

mcdowell_dataset %>%
  mutate(sum = map())

# maybe this one from arthropodAnalyses.R

mcdAll <- dbGetQuery(mcd,
"select
se.sampling_event_id,
sub.sumoftaxa,
tc.trapcount,
(sub.sumoftaxa/tc.trapcount) as average,
se.site_id,
se.sample_date,
sub.display_name
from sampling_events as se
inner join(
select
sum(s.allsizes) as sumoftaxa,
s.arthropod_taxon_id,
atl.display_name,
se_2.sampling_event_id
FROM(
select
(
IF(isnull(lt2mm), 0, lt2mm) +
IF(isnull(_2_5mm), 0, _2_5mm) +
IF(isnull(_5_10mm), 0, _5_10mm) +
IF(isnull(gt10mm), 0, gt10mm) +
IF(isnull(unsized), 0, unsized)
) as allsizes,
arthropod_taxon_id,
trap_sampling_event_id
FROM trap_specimens
) as s
inner join trap_sampling_events as se_2 on s.trap_sampling_event_id = se_2.trap_sampling_event_id
inner join arthropod_taxonomies as atl on s.arthropod_taxon_id = atl.arthropod_taxon_id
group by s.arthropod_taxon_id, se_2.sampling_event_id
) as sub on (se.sampling_event_id = sub.sampling_event_id)
inner join (
select
`trap_sampling_events`.`sampling_event_id`,
count(distinct `trap_sampling_events`.`trap_sampling_event_id`) as trapcount
from `trap_sampling_events`
where coalesce(`trap_sampling_events`.flags, '') = ''
group by sampling_event_id
) as tc on (tc.sampling_event_id = sub.sampling_event_id)
order by se.site_id, se.sample_date
;")