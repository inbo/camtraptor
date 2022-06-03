/*
Created by Peter Desmet (INBO)
Mapping from Camtrap DP: https://tdwg.github.io/camtrap-dp
Mapping to Audubon Media Description: https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml
Y = included in DwC, N = not included in DwC

CAMTRAP DP MEDIA

mediaID                         Y: as link to observation
deploymentID                    N: included at observation level
sequenceID                      Y: as link to observation
captureMethod                   ?
timestamp                       Y
filePath                        Y
fileName                        Y: to sort data
fileMediatype                   Y
exifData                        N
favourite                       N
comments                        N
_id                             N

*/

-- Observations can be based on sequences (sequenceID) or individual files (mediaID)
-- Make two joins and union to capture both cases without overlap
WITH observations_media AS (
-- Sequence based observations
  SELECT obs.observationID, obs.timestamp AS observationTimestamp, med.*
  FROM observations AS obs
    LEFT JOIN media AS med ON obs.sequenceID = med.sequenceID
  WHERE obs.observationType = 'animal' AND obs.mediaID IS NULL
  UNION
-- File based observations
  SELECT obs.observationID, obs.timestamp AS observationTimestamp, med.*
  FROM observations AS obs
    LEFT JOIN media AS med ON obs.mediaID = med.mediaID
  WHERE obs.observationType = 'animal' AND obs.mediaID IS NOT NULL
)

SELECT
-- occurrenceID
  obs_med.observationID AS occurrenceID,
-- creator
-- providerLiteral
-- provider
-- rights
  {metadata$mediaLicense} AS rights,
-- owner
-- identifier
  obs_med.mediaID AS identifier,
-- type
  CASE
    WHEN obs_med.fileMediatype LIKE '%video%' THEN 'MovingImage'
    ELSE 'StillImage'
  END AS type,
-- providerManagedID
  obs_med._id AS providerManagedID,
-- captureDevice
--  dep.cameraModel AS captureDevice,
-- resourceCreationTechnique
  obs_med.captureMethod AS resourceCreationTechnique,
-- accessURI
  obs_med.filePath AS accessURI,
-- format
  obs_med.fileMediatype AS format,
-- CreateDate
  STRFTIME('%Y-%m-%dT%H:%M:%SZ', datetime(obs_med.timestamp, 'unixepoch')) AS createDate

FROM
  observations_media AS obs_med
  LEFT JOIN deployments AS dep
    ON obs_med.deploymentID = dep.deploymentID

ORDER BY
-- Order is not retained in observations_media, so important to sort
  obs_med.observationTimestamp,
  obs_med.timestamp,
  obs_med.fileName
