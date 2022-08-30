/*
Schema: https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml
Camtrap DP terms and whether they are included in DwC (Y) or not (N):

deployments.deploymentID                Y
deployments.locationID                  Y
deployments.locationName                Y
deployments.longitude                   Y
deployments.latitude                    Y
deployments.coordinateUncertainty       Y
deployments.start                       Y
deployments.end                         Y
deployments.setupBy                     N
deployments.cameraID                    N
deployments.cameraModel                 Y: in dwc_multimedia
deployments.cameraInterval              N
deployments.cameraHeight                N
deployments.cameraTilt                  N
deployments.cameraHeading               N
deployments.timestampIssues             N
deployments.baitUse                     Y
deployments.session                     N
deployments.array                       N
deployments.featureType                 Y
deployments.habitat                     Y
deployments.tags                        Y
deployments.comments                    Y
deployments._id                         N
observations.observationID              Y
observations.deploymentID               Y
observations.sequenceID                 Y
observations.mediaID                    N: in dwc_multimedia
observations.timestamp                  Y
observations.observationType            Y: as filter
observations.cameraSetup                N
observations.taxonID                    Y
observations.scientificName             Y
observations.count                      Y
observations.countNew                   N
observations.lifeStage                  Y
observations.sex                        Y
observations.behaviour                  Y
observations.individualID               Y
observations.classificationMethod       Y
observations.classifiedBy               Y
observations.classificationTimestamp    Y
observations.classificationConfidence   Y
observations.comments                   Y
observations._id                        N
*/

SELECT
-- RECORD-LEVEL
  'Image'                               AS type,
  {license}                             AS license,
  {rights_holder}                       AS rightsHolder,
-- bibliographicCitation: how *record* should be cited, so not package bibliographicCitation
  {dataset_id}                          AS datasetID,
-- institutionCode: org managing the platform/collection, but that info is not available
  {collection_code}                     AS collectionCode,
  {dataset_name}                        AS datasetName,
  'MachineObservation'                  AS basisOfRecord,
-- OCCURRENCE
  obs.observationID                     AS occurrenceID,
  obs.count                             AS individualCount,
  obs.sex                               AS sex,
  obs.lifeStage                         AS lifeStage,
  obs.behaviour                         AS behavior,
  'present'                             AS occurrenceStatus,
  obs.comments                          AS occurrenceRemarks,
-- ORGANISM
  obs.individualID                      AS organismID,
-- EVENT
  obs.sequenceID                        AS eventID,
  obs.deploymentID                      AS parentEventID,
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(obs.timestamp, 'unixepoch')) AS eventDate,
  dep.habitat                           AS habitat,
  'camera trap' ||
  CASE
    WHEN dep.baitUse IS 'none' THEN ' without bait'
    WHEN dep.baitUse IS NOT NULL THEN ' with bait'
    ELSE ''
  END                                   AS samplingProtocol,
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(dep.start, 'unixepoch')) ||
  '/' ||
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(dep.end, 'unixepoch')) AS samplingEffort, -- Duration of deployment
  COALESCE(
    dep.comments || ' | tags: ' || dep.tags,
    'tags: ' || dep.tags,
    dep.comments
  )                                     AS eventRemarks,
-- LOCATION
  dep.locationID                        AS locationID,
  dep.locationName                      AS locality,
  dep.featureType                       AS locationRemarks,
  dep.latitude                          AS decimalLatitude,
  dep.longitude                         AS decimalLongitude,
  'WGS84'                               AS geodeticDatum,
  dep.coordinateUncertainty             AS coordinateUncertaintyInMeters,
-- IDENTIFICATION
  obs.classifiedBy                      AS identifiedBy,
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(obs.classificationTimestamp, 'unixepoch')) AS dateIdentified,
  COALESCE(
    'classified by ' || obs.classificationMethod || ' with ' || obs.classificationConfidence || ' confidence',
    'classified by ' || obs.classificationMethod
  )                                     AS identificationRemarks,
-- TAXON
  obs.taxonID                           AS taxonID,
  obs.scientificName                    AS scientificName,
  'Animalia'                            AS kingdom

FROM
  observations AS obs
  LEFT JOIN deployments AS dep
    ON obs.deploymentID = dep.deploymentID

WHERE
  -- Select biological observations only (excluding observations marked as human, blank, vehicle)
  -- Same filter should be used in dwc_multimedia.sql
  obs.observationType = 'animal'

ORDER BY
  obs.timestamp
