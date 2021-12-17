-- migrate:up
CREATE TABLE if not exists queue_config
(
  system_id uuid NOT NULL,
  requires_global_lock boolean not null,

  CONSTRAINT queue_config_pkey PRIMARY KEY (system_id)
);


CREATE TABLE if not exists queue_lock
(
  system_id uuid NOT NULL references queue_config(system_id),
  host text COLLATE pg_catalog."default" NOT NULL,
  locked_until timestamp without time zone null,

  CONSTRAINT queue_lock_pkey PRIMARY KEY (system_id)
);


CREATE TABLE if not exists work_type
(
  wtId uuid NOT NULL,
  system_id uuid NOT NULL references queue_config(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  default_retries int NOT NULL,
  default_backoff_seconds int[] NOT NULL,
  default_heartbeat_check_period int NULL,
  default_exec_environment text COLLATE pg_catalog."default" NOT NULL,
  dequeue_lock_period_seconds int NOT NULL,

  CONSTRAINT work_type_pkey PRIMARY KEY (wtId)
);
CREATE INDEX if not exists ix_work_type_system_id ON work_type (system_id);


CREATE TABLE if not exists work_item_status
( wsid int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT work_item_status_pkey PRIMARY KEY (wsId)
);

insert into work_item_status (wsid, name) values (900300, 'pending');


CREATE TABLE if not exists work_item
(
  wiid uuid NOT NULL,
  system_id uuid NOT NULL references queue_config(system_id),
  wtid uuid NOT NULL references work_type(wtId),
  ignore_until timestamp without time zone null,
  retries_left int NOT NULL,
  created_at timestamp without time zone not null,
  done_at timestamp without time zone null,
  active timestamp without time zone null,
  group_id uuid NULL,
  depends_on_groups uuid[] NULL,
  depends_on_work_item uuid[] NULL,
  status_id int NOT NULL references work_item_status(wsId),
  backoff_count int NOT NULL,
  attempts int NOT NULL,
  cleanup_done timestamp without time zone null,

  CONSTRAINT work_item_pkey PRIMARY KEY (wiId)
);
CREATE INDEX if not exists ix_work_item_wtId ON work_item (wtId);
CREATE INDEX if not exists ix_work_item_system_id ON work_item (system_id);
CREATE INDEX if not exists ix_work_item_ignore_until ON work_item (ignore_until);
CREATE INDEX if not exists ix_work_item_done_at ON work_item (done_at);
CREATE INDEX if not exists ix_work_item_group_id ON work_item (group_id);
CREATE INDEX if not exists ix_work_item_cleanup_done ON work_item (cleanup_done);
CREATE INDEX if not exists ix_work_item_active ON work_item (active);
CREATE INDEX if not exists ix_work_item_status_id ON work_item (status_id);


CREATE SEQUENCE if not exists pending_work_item_seq;
CREATE TABLE if not exists pending_work_item
(
  piId bigint NOT NULL DEFAULT nextval('pending_work_item_seq'::regclass),
  wiId uuid NOT NULL references work_item(wiId),
  created_at timestamp without time zone not null,
  active timestamp without time zone null,
  parent_pending_worker_item bigint NOT NULL references pending_work_item(piId),

  CONSTRAINT pending_work_item_pkey PRIMARY KEY (piId)
);
CREATE INDEX if not exists ix_pending_work_item_wi_id ON pending_work_item (wiId);
CREATE INDEX if not exists ix_pending_work_item_active ON pending_work_item (active);


CREATE TABLE if not exists queue_status
( qsId int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT queue_status_pkey PRIMARY KEY (qsId)
);
insert into queue_status (qsid, name) values (900200, 'pending');


CREATE SEQUENCE if not exists queue_seq;
CREATE TABLE if not exists queue
(
  qId bigint NOT NULL DEFAULT nextval('queue_seq'::regclass),
  piId bigint NOT NULL references pending_work_item(piId),
  locked_until timestamp without time zone null,
  created_at timestamp without time zone not null,
  started_at timestamp without time zone null,
  done_at timestamp without time zone null,
  active timestamp without time zone null,
  heartbeat_at timestamp without time zone null,
  worker_name text COLLATE pg_catalog."default" NOT NULL,
  worker_info text COLLATE pg_catalog."default" NOT NULL,
  cleanup_done timestamp without time zone null,
  status_id int NOT NULL references queue_status(qsId),

  CONSTRAINT queue_pkey PRIMARY KEY (qId)
);
CREATE INDEX if not exists ix_queue_qid ON queue (qid);
CREATE INDEX if not exists ix_queue_locked_until ON queue (locked_until);
CREATE INDEX if not exists ix_queue_active ON queue (active);
CREATE INDEX if not exists ix_queue_heartbeat_at ON queue (heartbeat_at);
CREATE INDEX if not exists ix_queue_cleanup_done ON queue (cleanup_done);
CREATE INDEX if not exists ix_queue_status_id ON queue (status_id);


CREATE TABLE if not exists queue_log_level
( llId int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT queue_log_level_pkey PRIMARY KEY (llId)
);
insert into queue_log_level (llid, name) values (900100, 'trace');
insert into queue_log_level (llid, name) values (900101, 'debug');
insert into queue_log_level (llid, name) values (900102, 'info');
insert into queue_log_level (llid, name) values (900103, 'warn');
insert into queue_log_level (llid, name) values (900104, 'error');



CREATE SEQUENCE if not exists queue_log_seq;
CREATE TABLE if not exists queue_log
(
  qlId bigint NOT NULL DEFAULT nextval('queue_log_seq'::regclass),
  qId bigint NOT NULL references queue(qId),
  level_id int not null references queue_log_level(llId),
  log_title text COLLATE pg_catalog."default" NOT NULL,
  log_type text COLLATE pg_catalog."default" NOT NULL,
  log_detail text COLLATE pg_catalog."default" NULL,
  log_data jsonb NULL,
  log_at timestamp without time zone not null,

  CONSTRAINT queue_log_pkey PRIMARY KEY (qlId)
);
CREATE INDEX if not exists ix_queue_qid ON queue (qid);


-- migrate:down
drop table queue_log;
drop sequence queue_log_seq;
drop table queue_log_level;
drop table queue;
drop sequence queue_seq;
drop table queue_status;
drop table pending_work_item;
drop sequence pending_work_item_seq;
drop table work_item;
drop table work_item_status;
drop table work_type;
drop table queue_lock;
drop table queue_config;

