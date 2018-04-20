##### trap count ####

select
`trap_sampling_events`.`sampling_event_id`,
count(distinct `trap_sampling_events`.`trap_sampling_event_id`) as trapcount
from `trap_sampling_events`
where NOT 
(
`trap_sampling_events`.`flags` LIKE 'trap not collected' OR 
`trap_sampling_events`.`flags` LIKE 'missing'
)  OR 
`trap_sampling_events`.`flags` is null OR
`trap_sampling_events`.`flags` = ""
group by sampling_event_id
limit 50000;

-- totals for each taxon

select `trap_specimens`.`arthropod_taxon_id`, 
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
;


##### McDowell query ####

select
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
where NOT 
(
`trap_sampling_events`.`flags` LIKE 'trap not collected' OR 
`trap_sampling_events`.`flags` LIKE 'NotCollected'
)  OR 
`trap_sampling_events`.`flags` is null OR
`trap_sampling_events`.`flags` = ""
group by sampling_event_id
) as tc on (tc.sampling_event_id = sub.sampling_event_id)
# where se.sampling_event_id = 4177
# where se.site_id = 5
order by se.site_id, se.sample_date
limit 50000;


##### po10 query ####

select
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
where NOT 
(
`trap_sampling_events`.`flags` LIKE 'trap not collected' OR 
`trap_sampling_events`.`flags` LIKE 'missing'
)  OR 
`trap_sampling_events`.`flags` is null OR
`trap_sampling_events`.`flags` = ""
group by sampling_event_id
) as tc on (tc.sampling_event_id = sub.sampling_event_id)
# where se.site_id = 5
order by se.sample_date

#################################################################
##### vegetation dwelling ####
# note that this query does not add the organisms that are sized.
# given how few there are, it is/was easier to change the single
# instance (as of 2014-06-04) of a sized organism (plant_specimen_id = 225)
# to unsized.
#################################################################


SELECT
# `plant_specimens`.`plant_specimen_id`, 
# `plant_specimens`.`plant_sampling_event_id`, 
# `plant_specimens`.`arthropod_taxon_id`, 
# `plant_specimens`.`lt2mm`, 
# `plant_specimens`.`_2_5mm`, 
# `plant_specimens`.`_5_10mm`, 
# `plant_specimens`.`gt10mm`, 
`sites`.`sitename`,
`sampling_events`.`sample_date`,
`plant_taxonomies`.`scientific_name` as plantTaxon,
`arthropod_taxonomies`.`display_name` as arthropodTaxon,
`plant_specimens`.`unsized` as number,
`plant_sampling_events`.`plant_sampling_event_id`, 
# `plant_sampling_events`.`sampling_event_id`, 
# `plant_sampling_events`.`plant_taxon_id`, 
`plant_sampling_events`.`comments`, 
`plant_sampling_events`.`flags`
# `sampling_events`.`sampling_event_id` 
# `sampling_events`.`site_id`, 
FROM mcdowell_arthropods.plant_specimens
JOIN `arthropod_taxonomies` ON (`arthropod_taxonomies`.`arthropod_taxon_id` = `plant_specimens`.`arthropod_taxon_id`)
JOIN `plant_sampling_events` ON (`plant_sampling_events`.`plant_sampling_event_id` = `plant_specimens`.`plant_sampling_event_id`)
JOIN `sampling_events` ON (`sampling_events`.`sampling_event_id` = `plant_sampling_events`.`sampling_event_id`)
JOIN `plant_taxonomies` ON (`plant_taxonomies`.`plant_taxon_id` = `plant_sampling_events`.`plant_taxon_id`)
JOIN `sites` ON (`sites`.`site_id` = `sampling_events`.`site_id`)
# where `sampling_events`.`sampling_event_id` = 50
#group by display_name, plant_sampling_event_id
order by sample_date, sitename
;


#################################################################
# McDowells by finer taxon, here order but change to desired
# level of resolution
#################################################################

select
  se.sampling_event_id,
  sub.sumoftaxa,
  tc.trapcount,
  (sub.sumoftaxa/tc.trapcount) as average,
  se.site_id,
  se.sample_date,
# sub.display_name
  sub.arth_order
from sampling_events as se
inner join(
  select
    sum(s.allsizes) as sumoftaxa,
    s.arthropod_taxon_id,
    atl.arth_order,
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
  group by atl.arth_order, se_2.sampling_event_id
) as sub on (se.sampling_event_id = sub.sampling_event_id)
inner join (
select
`trap_sampling_events`.`sampling_event_id`,
count(distinct `trap_sampling_events`.`trap_sampling_event_id`) as trapcount
from `trap_sampling_events`
where NOT 
(
`trap_sampling_events`.`flags` LIKE 'trap not collected' OR 
`trap_sampling_events`.`flags` LIKE 'NotCollected'
)  OR 
`trap_sampling_events`.`flags` is null OR
`trap_sampling_events`.`flags` = ""
group by sampling_event_id
) as tc on (tc.sampling_event_id = sub.sampling_event_id)
# where se.sampling_event_id = 4177
# where se.site_id = 5
order by se.site_id, se.sample_date
limit 50000;





#################################################################
# The following has been adjusted to the new arthropods schema
#################################################################
# This is from Dave - does not work; yields number of traps with
# particular taxon as trap count see above for working queries.
# Below this query that Dave generated are additional queries
# relevant only to the old DB structure, but maintained here
# as examples of past queries.
#################################################################

-- sums all insects of each TAXON (by display_name) at a given site and date (here site 5)
select
  sub.sumoftaxa,
  sub.trapcount,
  (sub.sumoftaxa/sub.trapcount) as average,
  se.site_id,
  se.sample_date,
  sub.display_name
from sampling_events as se
inner join(
  select
    sum(s.allsizes) as sumoftaxa,
    count(se_2.sampling_event_id) as trapcount,
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
where se.site_id = 5
order by se.sample_date;

#################################################################



#################################################################
# The following has NOT been adjusted to the new schema
# email me if you have questions.
#################################################################

-- sums all insect at a given site and date (here site 5)
SELECT sum(s.allsizes) AS allorgs,
se.site_id,
se.sample_date
FROM
(SELECT
(
IF(isnull(lt2mm), 0, lt2mm) +
IF(isnull(_2_5mm), 0, _2_5mm) +
IF(isnull(_5_10mm), 0, _5_10mm) +
IF(isnull(gt10mm), 0, gt10mm) +
IF(isnull(unsized), 0, unsized) ) as allsizes,
sampling_event_id
FROM specimens) AS s
INNER JOIN sampling_events AS se ON s.sampling_event_id = se.sampling_event_id
WHERE se.site_id = 5
group by se.sample_date
ORDER BY se.sample_date;


-- sums all insects of each ORDER at a given site and date (here site 5)
select
 sub.sumoftaxa,
 se.trapcount,
 (sub.sumoftaxa/se.trapcount) as average,
 sub.site_id,
 sub.sample_date,
-- sub.display_name
 sub.arth_order
FROM
 (
   SELECT
     count(sampling_event_id) as trapcount,
     site_id,
     sample_date
   FROM lter10arthropods_production.sampling_events
   where site_id = 5
   group by sample_date
 ) as se
inner join
 (
   select
     sum(s.allsizes) as sumoftaxa,
     s.taxon_id,
     tl.display_name,
   tl.arth_order,
     se_2.site_id,
     se_2.sample_date
   FROM
     (
       select
         (
           IF(isnull(lt2mm), 0, lt2mm) +
           IF(isnull(_2_5mm), 0, _2_5mm) +
           IF(isnull(_5_10mm), 0, _5_10mm) +
           IF(isnull(gt10mm), 0, gt10mm) +
           IF(isnull(unsized), 0, unsized) ) as allsizes,
         taxon_id,
         sampling_event_id
       FROM specimens
     ) as s
      inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
      inner join taxon_list as tl on s.taxon_id = tl.taxon_id
      where site_id = 5
    group by tl.arth_order, se_2.site_id, se_2.sample_date
      -- limit 500
      -- group by sample_date
 ) as sub on (se.site_id = sub.site_id and se.sample_date = sub.sample_date)
order by sub.sample_date;

-- sums all insects of each TAXON (by display_name) for all sites in McDowells, careful that trap count is misleading as there are always 10 traps due to notes or whathave you in database
select
 sub.sumoftaxa,
 se.trapcount,
 (sub.sumoftaxa/se.trapcount) as average,
 sub.site_id,
 sub.sample_date,
 sub.display_name
FROM
 (
   SELECT
     count(sampling_event_id) as trapcount,
     site_id,
     sample_date
   FROM McDowellArthropods.sampling_events
   -- include this where statement to omit cups that were not collected
   -- where (`sampling_events`.`flags` <> 'NotCollected' or `sampling_events`.`flags` IS NULL)
   group by site_id, sample_date
 ) as se
inner join
 (
   select
     sum(s.allsizes) as sumoftaxa,
     s.taxon_id,
     tl.display_name,
     se_2.site_id,
     se_2.sample_date
   FROM
     (
       select
         (
           IF(isnull(lt2mm), 0, lt2mm) +
           IF(isnull(_2_5mm), 0, _2_5mm) +
           IF(isnull(_5_10mm), 0, _5_10mm) +
           IF(isnull(gt10mm), 0, gt10mm) +
           IF(isnull(unsized), 0, unsized) ) as allsizes,
         taxon_id,
         sampling_event_id
       FROM specimens
     ) as s
      inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
      inner join taxon_list as tl on s.taxon_id = tl.taxon_id
      -- where site_id = 5
    group by s.taxon_id, se_2.site_id, se_2.sample_date
      -- limit 500
      -- group by sample_date
 ) as sub on (se.site_id = sub.site_id and se.sample_date = sub.sample_date)
order by sub.site_id, sub.sample_date;


-- sums all insects of each ORDER for all sites in the McDowells, careful that trap count is misleading as there are always 10 traps due to notes or whathave you in the database
select
 sub.sumoftaxa,
 se.trapcount,
 (sub.sumoftaxa/se.trapcount) as average,
 sub.site_id,
 sub.sample_date,
-- sub.display_name
 sub.arth_order
FROM
 (
   SELECT
     count(sampling_event_id) as trapcount,
     site_id,
     sample_date
   FROM McDowellArthropods.sampling_events
   -- include this where statement to omit cups that were not collected
   -- where (`sampling_events`.`flags` <> 'NotCollected' or `sampling_events`.`flags` IS NULL)
   group by site_id, sample_date
 ) as se
inner join
 (
   select
     sum(s.allsizes) as sumoftaxa,
     s.taxon_id,
     tl.display_name,
   tl.arth_order,
     se_2.site_id,
     se_2.sample_date
   FROM
     (
       select
         (
           IF(isnull(lt2mm), 0, lt2mm) +
           IF(isnull(_2_5mm), 0, _2_5mm) +
           IF(isnull(_5_10mm), 0, _5_10mm) +
           IF(isnull(gt10mm), 0, gt10mm) +
           IF(isnull(unsized), 0, unsized) ) as allsizes,
         taxon_id,
         sampling_event_id
       FROM specimens
     ) as s
      inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
      inner join taxon_list as tl on s.taxon_id = tl.taxon_id
      -- where site_id = 5
    group by tl.arth_order, se_2.site_id, se_2.sample_date
      -- limit 500
      -- group by sample_date
 ) as sub on (se.site_id = sub.site_id and se.sample_date = sub.sample_date)
order by sub.site_id, sub.sample_date;


-- sums all insects at McDowell sites by date and site
SELECT sum(s.allsizes) AS allorgs,
se.site_id,
se.sample_date
FROM
(SELECT
(
IF(isnull(lt2mm), 0, lt2mm) +
IF(isnull(_2_5mm), 0, _2_5mm) +
IF(isnull(_5_10mm), 0, _5_10mm) +
IF(isnull(gt10mm), 0, gt10mm) +
IF(isnull(unsized), 0, unsized) ) as allsizes,
sampling_event_id
FROM specimens) AS s
INNER JOIN sampling_events AS se ON s.sampling_event_id = se.sampling_event_id
-- WHERE se.site_id = 5
group by se.sample_date, se.site_id
ORDER BY se.site_id, se.sample_date;


-- simple summary stats for investigating common taxa
select s.taxon_id, 
sum(s.unsized), 
tl.arth_Class, 
tl.arth_order, 
tl.arth_family, 
tl.arth_genus_subgenus, 
tl.display_name
from McDowellArthropods.specimens as s
inner join taxon_list as tl on s.taxon_id = tl.taxon_id
group by tl.arth_genus_subgenus
order by s.unsized;


-- table of all taxa collected at McDowell sites
select
`taxon_list`.`taxon_id` as t_id,
`specimens`.`taxon_id` as s_id,
`specimens`.`sampling_event_id`,
(
  IF(isnull(lt2mm), 0, lt2mm) +
  IF(isnull(_2_5mm), 0, _2_5mm) +
  IF(isnull(_5_10mm), 0, _5_10mm) +
  IF(isnull(gt10mm), 0, gt10mm) +
  IF(isnull(unsized), 0, unsized) ) as allsizes,
`taxon_list`.`arth_Class`,
`taxon_list`.`arth_order`,
`taxon_list`.`arth_family`,
`taxon_list`.`arth_genus_subgenus`,
`taxon_list`.`old_name`,
`taxon_list`.`display_name`
FROM specimens
JOIN taxon_list ON `taxon_list`.`taxon_id` = `specimens`.`taxon_id`
GROUP BY t_id
LIMIT 50000

-- sums all insects of each TAXON (by display_name) at a given core site (here site 5) for a given year (here 2012)
select
 sub.sumoftaxa,
 se.trapcount,
 (sub.sumoftaxa/se.trapcount) as average,
 sub.site_id,
 year(sub.sample_date) as year,
 sub.display_name
FROM
 (
   SELECT
     count(sampling_event_id) as trapcount,
     site_id,
     sample_date
   FROM lter10arthropods_production.sampling_events
   where site_id = 5
   and (flags <> 'missing' or flags IS NULL)
   and year(`sampling_events`.`sample_date`) = 2012
 ) as se
inner join
(
select
sum(s.allsizes) as sumoftaxa,
s.taxon_id,
tl.display_name,
se_2.site_id,
se_2.sample_date
FROM
(
 select
  (
   IF(isnull(lt2mm), 0, lt2mm) +
   IF(isnull(_2_5mm), 0, _2_5mm) +
   IF(isnull(_5_10mm), 0, _5_10mm) +
   IF(isnull(gt10mm), 0, gt10mm) +
   IF(isnull(unsized), 0, unsized) 
  ) as allsizes,
   taxon_id,
   sampling_event_id
   FROM specimens
) as s
inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
inner join taxon_list as tl on s.taxon_id = tl.taxon_id
group by 
s.taxon_id,
se_2.site_id, 
year(se_2.sample_date)
) as sub on (se.site_id = sub.site_id and year(se.sample_date) = year(sub.sample_date))
order by display_name
;

-- sums all insects of each TAXON (by display_name) at all McDowell sites for a given year (here 2012); make appropriate changes for specific taxonomic levels
select
 sub.sumoftaxa,
 se.trapcount,
 (sub.sumoftaxa/se.trapcount) as average,
 sub.site_id,
 year(sub.sample_date),
 sub.display_name /*change to sub.arth_order for order */
FROM
 (
   SELECT
     count(sampling_event_id) as trapcount,
     site_id,
     sample_date
   FROM McDowellArthropods.sampling_events
   -- include this where statement to omit cups that were not collected
   where (`sampling_events`.`flags` <> 'NotCollected' or `sampling_events`.`flags` IS NULL)
   and year(`sampling_events`.`sample_date`) = 2012
   group by site_id#, sample_date
 ) as se
inner join
 (
   select
     sum(s.allsizes) as sumoftaxa,
     s.taxon_id,
     tl.display_name,
     /*add tl.arth_order for order*/
     se_2.site_id,
     se_2.sample_date
   FROM
     (
       select
         (
           IF(isnull(lt2mm), 0, lt2mm) +
           IF(isnull(_2_5mm), 0, _2_5mm) +
           IF(isnull(_5_10mm), 0, _5_10mm) +
           IF(isnull(gt10mm), 0, gt10mm) +
           IF(isnull(unsized), 0, unsized) ) as allsizes,
         taxon_id,
         sampling_event_id
       FROM specimens
     ) as s
inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
inner join taxon_list as tl on s.taxon_id = tl.taxon_id
group by 
s.taxon_id, /* change to tl.arth_order for order */
se_2.site_id, 
year(se_2.sample_date)
) as sub on (se.site_id = sub.site_id and year(se.sample_date) = year(sub.sample_date))
order by site_id, display_name
;

-- sums all insects at McDowell sites by site and provides average number per trap
select
sub.sumoftaxa,
se.trapcount,
(sub.sumoftaxa/se.trapcount) as average,
sub.site_id
FROM
 (
   SELECT
      count(sampling_event_id) as trapcount,
  site_id,
  sample_date
    FROM McDowellArthropods.sampling_events
    # include this where statement to omit cups that were not collected
    where (`sampling_events`.`flags` <> 'NotCollected' or `sampling_events`.`flags` IS NULL)
    group by site_id
 ) as se
inner join
 (
   select
      sum(s.allsizes) as sumoftaxa,
      s.taxon_id,
      tl.display_name,
      se_2.site_id,
      se_2.sample_date
   FROM
  (
  select
         (
           IF(isnull(lt2mm), 0, lt2mm) +
           IF(isnull(_2_5mm), 0, _2_5mm) +
           IF(isnull(_5_10mm), 0, _5_10mm) +
           IF(isnull(gt10mm), 0, gt10mm) +
           IF(isnull(unsized), 0, unsized) ) as allsizes,
  taxon_id,
  sampling_event_id
        FROM specimens
      ) as s
        inner join sampling_events as se_2 on s.sampling_event_id = se_2.sampling_event_id
        inner join taxon_list as tl on s.taxon_id = tl.taxon_id
  group by 
  se_2.site_id
  ) 
  as sub on (se.site_id = sub.site_id )
  order by 
  sub.site_id, 
  sub.sample_date
;
