-- migrate:up
CREATE TABLE if not exists queueLogJob
( jobId uuid NOT NULL
) inherits (queueLog);
CREATE INDEX if not exists ix_queueLogJob_jobId ON queueLogJob (jobId);


CREATE TABLE if not exists queueLogSample
( jobId uuid NOT NULL,
  sampleId uuid NOT NULL
) inherits (queueLog);
CREATE INDEX if not exists ix_queueLogSample_jobId ON queueLogSample (jobId);
CREATE INDEX if not exists ix_queueLogSample_sampleId ON queueLogSample (sampleId);

-- migrate:down
drop table queueLogJob;
drop table queueLogSample;
