-- migrate:up
CREATE TABLE if not exists bq_system_config
(
  system_id uuid NOT NULL,
  requires_global_lock boolean not null,
  poll_period_seconds int NOT NULL,
  locked_until timestamp without time zone null,
  locked_by text COLLATE pg_catalog."default" null,

  CONSTRAINT bq_queue_config_pkey PRIMARY KEY (system_id)
);


CREATE TABLE if not exists bq_work_type
(
  wtId uuid NOT NULL,
  system_id uuid NOT NULL references bq_system_config(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  default_retries int NOT NULL,
  default_backoff_seconds int[] NOT NULL,
  default_heartbeat_check_period int NULL,
  default_exec_environment text COLLATE pg_catalog."default" NOT NULL,
  dequeue_lock_period_seconds int NOT NULL,

  CONSTRAINT bq_work_type_pkey PRIMARY KEY (wtId)
);
CREATE INDEX if not exists ix_bq_work_type_system_id ON bq_work_type (system_id);


CREATE TABLE if not exists bq_work_item
(
  wiid uuid NOT NULL,
  system_id uuid NOT NULL references bq_system_config(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  wtid uuid NOT NULL references bq_work_type(wtId),
  ignore_until timestamp without time zone null,
  retries_left int NOT NULL,
  created_at timestamp without time zone not null,
  group_id uuid NULL,
  depends_on_groups uuid[] NULL,
  depends_on_work_item uuid[] NULL,
  backoff_count int NOT NULL,
  attempts int NOT NULL,

  CONSTRAINT bq_work_item_pkey PRIMARY KEY (wiId)
);
CREATE INDEX if not exists ix_bq_work_item_wtId ON bq_work_item (wtId);
CREATE INDEX if not exists ix_bq_work_item_system_id ON bq_work_item (system_id);
CREATE INDEX if not exists ix_bq_work_item_ignore_until ON bq_work_item (ignore_until);
CREATE INDEX if not exists ix_bq_work_item_group_id ON bq_work_item (group_id);


CREATE SEQUENCE if not exists bq_pending_work_item_seq;
CREATE TABLE if not exists bq_pending_work_item
(
  piId bigint NOT NULL DEFAULT nextval('bq_pending_work_item_seq'::regclass),
  wiId uuid NOT NULL references bq_work_item(wiId),
  created_at timestamp without time zone not null,
  parent_pending_worker_item bigint NULL references bq_pending_work_item(piId),

  CONSTRAINT bq_pending_work_item_pkey PRIMARY KEY (piId)
);
CREATE INDEX if not exists ix_bq_pending_work_item_wi_id ON bq_pending_work_item (wiId);


CREATE SEQUENCE if not exists bq_queue_seq;
CREATE TABLE if not exists bq_queue
(
  qId bigint NOT NULL DEFAULT nextval('bq_queue_seq'::regclass),
  piId bigint NOT NULL references bq_pending_work_item(piId),
  locked_until timestamp without time zone null,
  created_at timestamp without time zone not null,
  started_at timestamp without time zone null,
  heartbeat_at timestamp without time zone null,

  CONSTRAINT bq_queue_pkey PRIMARY KEY (qId)
);
CREATE INDEX if not exists ix_bq_queue_qid ON bq_queue (qid);
CREATE unique INDEX if not exists ix_bq_queue_piid ON bq_queue (piid);
CREATE INDEX if not exists ix_bq_queue_locked_until ON bq_queue (locked_until);
CREATE INDEX if not exists ix_bq_queue_heartbeat_at ON bq_queue (heartbeat_at);


-- migrate:down
drop table bq_queue;
drop sequence bq_queue_seq;
drop table bq_pending_work_item;
drop sequence bq_pending_work_item_seq;
drop table bq_work_item;
drop table bq_work_type;
drop table bq_system_config;

