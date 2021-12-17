-- migrate:up
CREATE TABLE if not exists queue_log_job
( jobId uuid NOT NULL
) inherits (queue_log);
CREATE INDEX if not exists ix_queue_log_job_jobId ON queue_log_job (jobId);


CREATE TABLE if not exists queue_log_sample
( jobId uuid NOT NULL,
  sampleId uuid NOT NULL
) inherits (queue_log);
CREATE INDEX if not exists ix_queue_log_sample_job_id ON queue_log_sample (jobId);
CREATE INDEX if not exists ix_queue_log_sample_sample_id ON queue_log_sample (sampleId);

-- migrate:down
drop table queue_log_Job;
drop table queue_log_Sample;
