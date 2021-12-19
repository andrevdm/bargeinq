-- migrate:up
CREATE TABLE if not exists system_config
(
  system_id uuid NOT NULL,
  requires_global_lock boolean not null,
  poll_period_seconds int NOT NULL,
  locked_until timestamp without time zone null,
  locked_by text COLLATE pg_catalog."default" null,

  CONSTRAINT queue_config_pkey PRIMARY KEY (system_id)
);


CREATE TABLE if not exists work_type
(
  wtId uuid NOT NULL,
  system_id uuid NOT NULL references system_config(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  default_retries int NOT NULL,
  default_backoff_seconds int[] NOT NULL,
  default_heartbeat_check_period int NULL,
  default_exec_environment text COLLATE pg_catalog."default" NOT NULL,
  dequeue_lock_period_seconds int NOT NULL,

  CONSTRAINT work_type_pkey PRIMARY KEY (wtId)
);
CREATE INDEX if not exists ix_work_type_system_id ON work_type (system_id);


CREATE TABLE if not exists work_item
(
  wiid uuid NOT NULL,
  system_id uuid NOT NULL references system_config(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  wtid uuid NOT NULL references work_type(wtId),
  ignore_until timestamp without time zone null,
  retries_left int NOT NULL,
  created_at timestamp without time zone not null,
  group_id uuid NULL,
  depends_on_groups uuid[] NULL,
  depends_on_work_item uuid[] NULL,
  backoff_count int NOT NULL,
  attempts int NOT NULL,

  CONSTRAINT work_item_pkey PRIMARY KEY (wiId)
);
CREATE INDEX if not exists ix_work_item_wtId ON work_item (wtId);
CREATE INDEX if not exists ix_work_item_system_id ON work_item (system_id);
CREATE INDEX if not exists ix_work_item_ignore_until ON work_item (ignore_until);
CREATE INDEX if not exists ix_work_item_group_id ON work_item (group_id);


CREATE SEQUENCE if not exists pending_work_item_seq;
CREATE TABLE if not exists pending_work_item
(
  piId bigint NOT NULL DEFAULT nextval('pending_work_item_seq'::regclass),
  wiId uuid NOT NULL references work_item(wiId),
  created_at timestamp without time zone not null,
  parent_pending_worker_item bigint NULL references pending_work_item(piId),

  CONSTRAINT pending_work_item_pkey PRIMARY KEY (piId)
);
CREATE INDEX if not exists ix_pending_work_item_wi_id ON pending_work_item (wiId);


CREATE SEQUENCE if not exists queue_seq;
CREATE TABLE if not exists queue
(
  qId bigint NOT NULL DEFAULT nextval('queue_seq'::regclass),
  piId bigint NOT NULL references pending_work_item(piId),
  locked_until timestamp without time zone null,
  created_at timestamp without time zone not null,
  started_at timestamp without time zone null,
  heartbeat_at timestamp without time zone null,

  CONSTRAINT queue_pkey PRIMARY KEY (qId)
);
CREATE INDEX if not exists ix_queue_qid ON queue (qid);
CREATE unique INDEX if not exists ix_queue_piid ON queue (piid);
CREATE INDEX if not exists ix_queue_locked_until ON queue (locked_until);
CREATE INDEX if not exists ix_queue_heartbeat_at ON queue (heartbeat_at);


-- migrate:down
drop table queue;
drop sequence queue_seq;
drop table pending_work_item;
drop sequence pending_work_item_seq;
drop table work_item;
drop table work_type;
drop table system_config;

