/*
Created by Peter Desmet (INBO)
Mapping from Camtrap DP: https://tdwg.github.io/camtrap-dp
Mapping to Darwin Core Occurrence: https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml
Y = included in DwC, N = not included in DwC

CAMTRAP DP DEPLOYMENTS

deploymentID                    Y
locationID                      Y
locationName                    Y
longitude                       Y
latitude                        Y
coordinateUncertainty           Y
start                           Y
end                             Y
setupBy                         N
cameraID                        N
cameraModel                     N
cameraInterval                  N
cameraHeight                    N
cameraTilt                      N
cameraHeading                   N
timestampIssues                 N
baitUse                         Y
session                         N
array                           N
featureType                     Y
habitat                         Y
tags                            Y
comments                        Y
_id                             N

CAMTRAP DP OBSERVATIONS

observationID                   Y
deploymentID                    Y
sequenceID                      Y
mediaID                         N: see dwc_multimedia
timestamp                       Y
observationType                 Y: as filter
cameraSetup                     N
taxonID                         Y
scientificName                  Y
count                           Y
countNew                        N
lifeStage                       Y
sex                             Y
behaviour                       Y
individualID                    Y
classificationMethod            Y
classifiedBy                    Y
classificationTimestamp         Y
classificationConfidence        Y
comments                        Y
_id                             N

*/

SELECT
-- RECORD-LEVEL
-- type
  -- Static value
  'Event' AS type,
-- license
  {metadata$dataLicense} AS license,
-- rightsHolder
  {metadata$rightsHolder} AS rightsHolder,
-- bibliographicCitation
  -- How *record* should be cited, don't use dataset-wide metadata$bibliographicCitation
-- datasetID
  {metadata$id} AS datasetID,
-- institutionCode
  {metadata$organization} AS institutionCode,
-- collectionCode
  {metadata$source} AS collectionCode,
-- datasetName
  {metadata$projectTitle} AS datasetName,
-- basisOfRecord
  -- Static value
  'MachineObservation' AS basisOfRecord,
-- informationWithheld
-- dataGeneralizations
-- dynamicProperties

-- OCCURRENCE
-- occurrenceID
  obs.observationID AS occurrenceID,
-- individualCount
  obs.count AS individualCount,
-- sex
  obs.sex AS sex,
-- lifeStage
  obs.lifeStage AS lifeStage,
-- behavior
  obs.behaviour AS behavior,
-- occurrenceStatus
  -- Static value
  'present' AS occurrenceStatus,
-- occurrenceRemarks
  obs.comments AS occurrenceRemarks,

-- ORGANISM
-- organismID
  obs.individualID AS organismID,

-- MATERIALSAMPLE
  -- Not applicable

-- EVENT
-- eventID
  obs.sequenceID AS eventID,
-- parentEventID
  obs.deploymentID AS parentEventID,
-- eventDate
  -- ISO-8601 in UTC
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(obs.timestamp, 'unixepoch')) AS eventDate,
-- eventTime
  -- Included in eventDate
-- habitat
  dep.habitat AS habitat,
-- samplingProtocol
  'camera trap' ||
  CASE
    WHEN dep.baitUse IS 'none' THEN ' without bait'
    WHEN dep.baitUse IS NOT NULL THEN ' with bait'
    ELSE ''
  END AS samplingProtocol,
-- samplingEffort
  -- Duration of deployment
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(dep.start, 'unixepoch')) ||
  '/' ||
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(dep.end, 'unixepoch')) AS samplingEffort,
-- eventRemarks
  CASE
    WHEN dep.comments IS NOT NULL THEN dep.comments
    ELSE ''
  END ||
  CASE
    WHEN dep.comments IS NOT NULL AND dep.tags IS NOT NULL THEN ' | '
    ELSE ''
  END ||
  CASE
    WHEN dep.tags IS NOT NULL THEN 'tags: ' || dep.tags
    ELSE ''
  END AS eventRemarks,

-- LOCATION
-- locationID
  dep.locationID AS locationID,
-- countryCode
  -- Single value might not apply to whole dataset, assumes coordinates are provided.
-- locality
  dep.locationName AS locality,
-- locationRemarks
  dep.featureType AS locationRemarks,
-- decimalLatitude
  dep.latitude AS decimalLatitude,
-- decimalLongitude
  dep.longitude AS decimalLongitude,
-- geodeticDatum
  -- Static value
  'WGS84' AS geodeticDatum,
-- coordinateUncertaintyInMeters
  dep.coordinateUncertainty AS coordinateUncertaintyInMeters,

-- GEOLOGICAL CONTEXT
  -- Not applicable

-- IDENTIFICATION
-- identifiedBy
  obs.classifiedBy AS identifiedBy,
-- identifiedByID
-- dateIdentified
  -- ISO-8601 in UTC
  strftime('%Y-%m-%dT%H:%M:%SZ', datetime(obs.classificationTimestamp, 'unixepoch')) AS dateIdentified,
-- identificationRemarks
  CASE
    WHEN obs.classificationMethod IS NOT NULL THEN 'classificationMethod: ' || obs.classificationMethod
    ELSE ''
  END ||
  CASE
    WHEN obs.classificationMethod IS NOT NULL AND obs.classificationConfidence IS NOT NULL THEN ' | '
    ELSE ''
  END ||
  CASE
    WHEN obs.classificationConfidence IS NOT NULL THEN 'classificationConfidence: ' || obs.classificationConfidence
    ELSE ''
  END AS identificationRemarks,

-- TAXON
-- taxonID
  obs.taxonID AS taxonID,
-- scientificName
  obs.scientificName AS scientificName,
-- kingdom
  -- Static value: records are filtered on animals in WHERE clause
  'Animalia' AS kingdom
-- taxonRank
-- vernacularName

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
