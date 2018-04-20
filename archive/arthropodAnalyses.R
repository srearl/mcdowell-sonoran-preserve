# CREATE VIEW `arthropod_taxonomies` AS
# SELECT
# `mcdowell`.`arthropod_taxonomies`.`arthropod_taxon_id` AS `arthropod_taxon_id`,
# `mcdowell`.`arthropod_taxonomies`.`code` AS `code`,
# `mcdowell`.`arthropod_taxonomies`.`arth_class` AS `arth_class`,
# `mcdowell`.`arthropod_taxonomies`.`arth_order` AS `arth_order`,
# `mcdowell`.`arthropod_taxonomies`.`arth_family` AS `arth_family`,
# `mcdowell`.`arthropod_taxonomies`.`arth_genus_subgenus` AS `arth_genus_subgenus`,
# `mcdowell`.`arthropod_taxonomies`.`old_name` AS `old_name`,
# `mcdowell`.`arthropod_taxonomies`.`display_name` AS `display_name`
# FROM `mcdowell`.`arthropod_taxonomies`

#' huh <- dbHasCompleted(rs)
#' dbClearResult(rs)
#' dbDisconnect(con)

#' Alternatively, if you want all the results (and they will fit in memory) use dbGetQuery which sends, fetches and clears for you.
#' noticed that the dbSendQuery, fetch combo returned only a partial dataset when querying all orgs from McD (too big???), but
#' worked okay with dbGetQuery

#' # analysis goals
#' 1. sampling summary
#' 2. commong orgs
#' 3. Brachymercy, where, when?
#' 4. seasonal abundance/H
#' 5. site abundance/H
#' 6. in/out abundance/H

# libraries ----
library("RMySQL")
library("devtools")
library("lubridate")
library('tidyverse')
library('ggplot2')
library('vegan')

# DB connections ----

# the database name is not defined in these queries so we need to put it in the
# dbConnect statement. Best would be to reformat these queries so we could call
# a connection file from localSettings
mysql_research <- dbConnect(MySQL(),
                            user='srearl',
                            password=.rs.askForPassword("Enter password:"),
                            dbname='mcdowell_arthropods',
                            host='mysql.research.gios.asu.edu')
mcd <- mysql_research

# sampling summaries ----
rs <- dbGetQuery(mcd,
"SELECT
  se.site_id,
  se.sample_date,
  s.sitename
FROM
  sampling_events se
  JOIN sites s ON (se.site_id = s.site_id);")

po10SE <- dbSendQuery(mcd,
  "SELECT
    se.site_id,
    se.sample_date,
    s.sitename
  FROM
    sampling_events se
    JOIN sites s ON (se.site_id = s.site_id)
  WHERE se.site_id = 5;")

samplingEvents <- rbind(mcdSE, po10SE)

samplingEvents$sample_date <- as.POSIXct(samplingEvents$sample_date, format = "%Y-%m-%d")
samplingEvents['season'] <- NA
samplingEvents[month(samplingEvents$sample_date) %in% c(1, 2, 12), ]$season <- 'winter'
samplingEvents[month(samplingEvents$sample_date) %in% c(3, 4, 5), ]$season <- 'spring'
samplingEvents[month(samplingEvents$sample_date) %in% c(6:9), ]$season <- 'summer'
samplingEvents[month(samplingEvents$sample_date) %in% c(10, 11), ]$season <- 'fall'

samplingEvents$year <- NA
samplingEvents$year <- year(samplingEvents$sample_date)

samplingSummary <- cast(samplingEvents, year+season~sitename, value='sample_date')

# total orgs ----
mcdTotals <- dbGetQuery(mcd,
"select `trap_specimens`.`arthropod_taxon_id`,
sum(
  IF(isnull(lt2mm), 0, lt2mm) +
  IF(isnull(_2_5mm), 0, _2_5mm) +
  IF(isnull(_5_10mm), 0, _5_10mm) +
  IF(isnull(gt10mm), 0, gt10mm) +
  IF(isnull(unsized), 0, unsized) ) as allsizes,
# sum(trap_specimens.unsized),
`arthropod_taxonomies`.`arth_class`,
`arthropod_taxonomies`.`arth_order`,
`arthropod_taxonomies`.`arth_family`,
`arthropod_taxonomies`.`arth_genus_subgenus`,
`arthropod_taxonomies`.`display_name`
FROM trap_specimens
JOIN `arthropod_taxonomies`ON (`arthropod_taxonomies`.`arthropod_taxon_id` = `trap_specimens`.`arthropod_taxon_id`)
group by `trap_specimens`.`arthropod_taxon_id`
order by arth_class, arth_order, arth_family, arth_genus_subgenus
;")

sum(mcdTotals$allsizes) # sum of all orgs
head(mcdTotals[order(-mcdTotals$allsizes),], n=10L) # ten most abundant

# all orgs ----
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


ad10All <- dbGetQuery(po10,
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
where se.site_id = 5 and sample_date > '2012-01-01'
;")

# merge all orgs mcd and AD10 from above
allsites <- rbind(mcdAll, ad10All)
allsites <- subset(allsites, select = c('site_id', 'display_name', 'average')) # pare down to cols relevant to vegan
widesites <- cast(allsites, site_id ~ display_name) # cast wide for vegan
div <- diversity(log1p(widesites)) # calculte diversity, here using natural-log transformed data (log1p)
div <- as.data.frame(div) # make the vegan results a df
div$site_id <- NA # create a new col for merging with relevant info
div$site_id <- row.names(div)
imptinfo <- mcdAllAvg[mcdAllAvg$taxa == 'allOrgs',]
diversity <- merge(imptinfo, div, by = 'site_id')
mean(diversity$div)

# plot of H' across regions
ggplot(diversity, aes(x=region, y=div, fill=position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4, width = 0.5) +
  ggtitle('diversity H\' of organisms at sites across regions') +
  xlab('sampling region') +
  ylab('H\'') +
  geom_text(position = position_dodge(width = 1.0), aes(x=region, label=sitename, y=div+0.15)) +
  scale_x_discrete(limit=c('browns', 'toms', 'gateway', 'lostdog', 'dixie', 'mtnPark'), labels = c('BrownsRanch', 'TomsThumb', 'Gateway', 'LostDog', 'DixieMine', 'MtnPark')) +
  coord_cartesian(ylim=c(3, 5)) +
  geom_segment(aes(x=0.5, y=4.47677, xend=6.5, yend=4.47677), lty='dotted') +
  scale_fill_manual(name = 'relative position', values = c('green2', 'Grey'), labels = c('boundary', 'interior')) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  theme(
    #         axis.ticks.x = element_blank(),
    axis.text.x = element_text(size=12, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=12, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
    #         axis.title.y = element_blank(),
    #         axis.ticks.y = element_line(size = 1, colour = 'black'),
    #         axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(),
    #         panel.background = element_blank(),
    #         # panel.border=element_blank(),
    #         #legend.text = element_blank(),
    #         legend.text = element_text(colour = 'black', size = 10),
    #         #legend.title = element_blank(),
    #         legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

nmds <- metaMDS(widesites)
stressplot(nmds)
ordiplot(nmds, type = "text", display = "sites")

# look at differences between sites 15 & 16 (Rincon, Sunrise)
widesites <- widesites[widesites$site_id %in% c(15,16),] # pare down to 15 & 16
newrow <- colSums(widesites) # sum each column
widesites <- rbind(widesites, newrow) # bind sum of columns to DF
widesites <- subset(widesites, select = c(widesites[3,]!=0)) # subset columns (taxa) that are zero at both sites

# total orgs and trap count by site ----

# all orgs
mcdAllAvg <- dbGetQuery(mcd,
"select
se.site_id,
'allOrgs',
s.sitename,
sub.trapcount,
sum(
(
  IF(isnull(lt2mm), 0, lt2mm) +
  IF(isnull(_2_5mm), 0, _2_5mm) +
  IF(isnull(_5_10mm), 0, _5_10mm) +
  IF(isnull(gt10mm), 0, gt10mm) +
  IF(isnull(unsized), 0, unsized)
)) as allsizes,
  sum(
  (
  IF(isnull(lt2mm), 0, lt2mm) +
  IF(isnull(_2_5mm), 0, _2_5mm) +
  IF(isnull(_5_10mm), 0, _5_10mm) +
  IF(isnull(gt10mm), 0, gt10mm) +
  IF(isnull(unsized), 0, unsized)
  ))/sub.trapcount as average
  FROM trap_specimens ts
  join trap_sampling_events tse on (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
  join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
  join sites s on (s.site_id = se.site_id)
  join
  (
  select
  se.site_id,
  count(tse.`trap_sampling_event_id`) as trapcount
  from trap_sampling_events tse
  join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
  join sites s on (s.site_id = se.site_id)
  where coalesce(tse.flags, '') = ''
  group by se.site_id
  ) as sub on (sub.site_id = se.site_id)
  group by se.site_id;")

# add AD-10 data, easier to just manually do it
newrow <- data.frame(site_id = 5, allOrgs = 'allOrgs', sitename = 'mtnPark', trapcount = 138, allsizes = 1619, average = 11.73)
mcdAllAvg <- rbind(mcdAllAvg, newrow)

# sans ants
mcdSansAnts <- dbGetQuery(mcd,
"select
se.site_id,
'sansAnts',
s.sitename,
sub.trapcount,
sum(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  )) as allsizes,
sum(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  ))/sub.trapcount as average
FROM trap_specimens ts
join trap_sampling_events tse on (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
join sites s on (s.site_id = se.site_id)
join
(
select
se.site_id,
count(tse.`trap_sampling_event_id`) as trapcount
from trap_sampling_events tse
join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
join sites s on (s.site_id = se.site_id)
where coalesce(tse.flags, '') = ''
group by se.site_id
) as sub on (sub.site_id = se.site_id)
where ts.arthropod_taxon_id not in
(
  select arthropod_taxon_id from
  (
  select arthropod_taxon_id
  from arthropod_taxonomies
  where arth_family like 'formicidae'
  )
  as arbquery
)
group by se.site_id;")

# add AD-10 data, easier to just manually do it
newrow <- data.frame(site_id = 5, sansAnts = 'sansAnts', sitename = 'mtnPark', trapcount = 138, allsizes = 1234, average = 8.94)
mcdSansAnts <- rbind(mcdSansAnts, newrow)

# merge McD and AD10 data
names(mcdAllAvg)[2] <- 'taxa'
names(mcdSansAnts)[2] <- 'taxa'
mcdAllAvg <- rbind(mcdAllAvg, mcdSansAnts)

# add boundary / interior designation
mcdAllAvg$position <- NA
mcdAllAvg$position <- 'I'
mcdAllAvg[mcdAllAvg$sitename %in% c('Paraiso', 'Bell', 'Dixileta', 'Mine', 'Rincon'),]$position <- 'B'

# add region designation
mcdAllAvg$region <- NA
mcdAllAvg[mcdAllAvg$sitename %in% c('Dixileta', 'LoneMtn'),]$region <- 'browns'
mcdAllAvg[mcdAllAvg$sitename %in% c('TomThumb', 'Paraiso'),]$region <- 'toms'
mcdAllAvg[mcdAllAvg$sitename %in% c('Gateway', 'Bell'),]$region <- 'gateway'
mcdAllAvg[mcdAllAvg$sitename %in% c('Sunrise', 'Rincon'),]$region <- 'lostdog'
mcdAllAvg[mcdAllAvg$sitename %in% c('Mine', 'Prospector'),]$region <- 'dixie'
mcdAllAvg[mcdAllAvg$sitename %in% c('mtnPark'),]$region <- 'mtnPark'

# edit mtnPark
mcdAllAvg[mcdAllAvg$sitename == 'mtnPark',]$sitename <- 'MtnPark'

# mean of averages across sites
mean(mcdAllAvg[mcdAllAvg$taxa=='allOrgs',]$average) # all organisms
mean(mcdAllAvg[mcdAllAvg$taxa=='sansAnts',]$average) # without ants

# plot of cumulative orgs per cumulative traps across sites with and without ants (not used) ----
ggplot(mcdAllAvg, aes(x=sitename, y=average, fill = taxa)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4) +
  ggtitle('average number of organisms across sampling locations') +
  xlab('sampling location') +
  ylab('total number of organisms per trap') +
  scale_x_discrete(limit=c('LoneMtn', 'Dixileta', 'TomThumb', 'Paraiso', 'Gateway', 'Bell', 'Sunrise', 'Rincon', 'Prospector', 'Mine', 'MtnPark'), labels = c('LoneMtn', 'Dixileta', 'TomThumb', 'Paraiso', 'Gateway', 'Bell', 'Sunrise', 'Rincon', 'Prospector', 'Mine', 'MtnPark')) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  geom_segment(aes(x=0.5, y=12.3187, xend=11.5, yend=12.3187), lty='dotted', colour = '#333333') +
  geom_segment(aes(x=0.5, y=7.713782, xend=11.5, yend=7.713782), lty='dotted', colour = '#CC0000') +
  scale_fill_manual(values = c('green2', 'Grey')) +
  theme(
#         axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.text.y = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
        axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
        legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_line(size = 1, colour = 'black'),
#         axis.line = element_line(size = 1, colour = 'black'),
        panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         # panel.border=element_blank(),
#         #legend.text = element_blank(),
#         legend.text = element_text(colour = 'black', size = 10),
#         #legend.title = element_blank(),
#         legend.title = element_text(colour = 'black', size = 12),
        plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

# plot of cumulative orgs per cumulative traps across regions ----
ggplot(mcdAllAvg[mcdAllAvg$taxa == 'allOrgs',], aes(x=region, y=average, fill=position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4, width = 0.5) +
  ggtitle('average number of organisms across regions') +
  xlab('sampling region') +
  ylab('cumulative organisms per cumulative traps') +
  geom_text(position = position_dodge(width = 1), aes(x=region, label=sitename, y=average+1)) +
  scale_x_discrete(limit=c('browns', 'toms', 'gateway', 'lostdog', 'dixie', 'mtnPark'), labels = c('BrownsRanch', 'TomsThumb', 'Gateway', 'LostDog', 'DixieMine', 'MtnPark')) +
  geom_segment(aes(x=0.5, y=12.3187, xend=6.5, yend=12.3187), lty='dotted') +
  scale_fill_manual(values = c('green2', 'Grey')) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  theme(
    #         axis.ticks.x = element_blank(),
    axis.text.x = element_text(size=12, colour = '#333333', family = 'Arial'),
    axis.text.y = element_text(size=12, colour = '#333333', family = 'Arial'),
    axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
    axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
    legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
    #         axis.title.y = element_blank(),
    #         axis.ticks.y = element_line(size = 1, colour = 'black'),
    #         axis.line = element_line(size = 1, colour = 'black'),
    panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(),
    #         panel.background = element_blank(),
    #         # panel.border=element_blank(),
    #         #legend.text = element_blank(),
    #         legend.text = element_text(colour = 'black', size = 10),
    #         #legend.title = element_blank(),
    #         legend.title = element_text(colour = 'black', size = 12),
    plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

# plot of cumulative orgs SANS ANTS per cumulative traps across regions ----
ggplot(mcdAllAvg[mcdAllAvg$taxa == 'sansAnts',], aes(x=region, y=average, fill=position, labels = sitename)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4, width = 0.5) +
  ggtitle('average number of organisms sans the Ants across regions') +
  xlab('sampling region') +
  ylab('cumulative organisms per cumulative traps') +
  geom_text(position = position_dodge(width = 1), aes(x=region, label=sitename, y=average+1)) +
  scale_x_discrete(limit=c('browns', 'toms', 'gateway', 'lostdog', 'dixie', 'mtnPark'), labels = c('BrownsRanch', 'TomsThumb', 'Gateway', 'LostDog', 'DixieMine', 'MtnPark')) +
  geom_segment(aes(x=0.5, y=12.3187, xend=6.5, yend=12.3187), lty='dotted', colour = '#333333') +
  geom_segment(aes(x=0.5, y=7.713782, xend=6.5, yend=7.713782), lty='dotted', colour = '#CC0000') +
  scale_fill_manual(values = c('green2', 'Grey')) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  theme(
#         axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.text.y = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
        axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
        legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_line(size = 1, colour = 'black'),
#         axis.line = element_line(size = 1, colour = 'black'),
        panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         # panel.border=element_blank(),
#         #legend.text = element_blank(),
#         legend.text = element_text(colour = 'black', size = 10),
#         #legend.title = element_blank(),
#         legend.title = element_text(colour = 'black', size = 12),
        plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )

# total orgs and trap count over all years at AD-10 ----

ad10AllYears <- dbGetQuery(po10,
"select
se.site_id,
'allOrgs',
s.sitename,
year(se.sample_date) as year,
sub.trapcount,
sum(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  )) as allsizes,
sum(
  (
    IF(isnull(lt2mm), 0, lt2mm) +
      IF(isnull(_2_5mm), 0, _2_5mm) +
      IF(isnull(_5_10mm), 0, _5_10mm) +
      IF(isnull(gt10mm), 0, gt10mm) +
      IF(isnull(unsized), 0, unsized)
  ))/sub.trapcount as average
FROM trap_specimens ts
join trap_sampling_events tse on (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
join sites s on (s.site_id = se.site_id)
join
(
select
se.site_id,
year(se.sample_date) as year,
count(tse.`trap_sampling_event_id`) as trapcount
from trap_sampling_events tse
join sampling_events se on (se.sampling_event_id = tse.sampling_event_id)
join sites s on (s.site_id = se.site_id)
where coalesce(tse.flags, '') = ''
and se.site_id = 5
group by year(se.sample_date)
) as sub on (sub.year = year(se.sample_date))
where se.site_id = 5
group by year(se.sample_date)
;")

mean(ad10AllYears$average)

ggplot(ad10AllYears, aes(x=year, y=average)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black', size = 0.4, fill = 'green2') +
  ggtitle('average number of organisms at McDowell Mountain Park') +
  xlab('sampling location') +
  ylab('cumulative organsism per cumulative number of traps') +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) + # remove slashed lines from legend
  geom_segment(aes(x=2001.5, y=22.67534, xend=2015.5, yend=22.67534), lty='dotted', colour = '#333333') +
  scale_x_continuous('year', breaks = c(2002:2015)) +
  theme(
        axis.text.x = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.text.y = element_text(size=12, colour = '#333333', family = 'Arial'),
        axis.title.x = element_text(size=12, colour = 'black', family = 'Arial', vjust = 0.0, face = 'bold'),
        axis.title.y = element_text(size=12, colour = 'black', family = 'Arial', vjust = 1.0, face = 'bold'),
        legend.key = element_rect(colour = "black"), # add lines around legend boxes back after removing them with guides above
        panel.grid.major = element_blank(),
        plot.title = element_text(vjust = 1.2, hjust = 0.5, family = 'Arial', face = 'bold')
  )