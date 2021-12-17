-- migrate:up
CREATE TABLE if not exists queueConfig
(
  systemId uuid NOT NULL,
  requiresGlobalLock boolean not null,

  CONSTRAINT queueConfig_pkey PRIMARY KEY (systemId)
);


CREATE TABLE if not exists queueLock
(
  systemId uuid NOT NULL references queueConfig(systemId),
  host text COLLATE pg_catalog."default" NOT NULL,
  lockedUntil timestamp without time zone null,

  CONSTRAINT queueLock_pkey PRIMARY KEY (systemId)
);


CREATE SEQUENCE if not exists workType_seq;
CREATE TABLE if not exists workType
(
  wtId bigint NOT NULL DEFAULT nextval('workType_seq'::regclass),
  systemId uuid NOT NULL references queueConfig(systemId),
  name text COLLATE pg_catalog."default" NOT NULL,
  defaultRetries int NOT NULL,
  defaultBackoffSeconds int[] NOT NULL,
  defaultHeartBeatCheckPeriod int NULL,
  defaultExecEnvironment text COLLATE pg_catalog."default" NOT NULL,
  dequeueLockPeriodSeconds int NOT NULL,

  CONSTRAINT workType_pkey PRIMARY KEY (wtId)
);
CREATE INDEX if not exists ix_workType_systemId ON workType (systemId);


CREATE TABLE if not exists workItemStatus
( wsId int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT workItemStatus_pkey PRIMARY KEY (wsId)
);

insert into workItemStatus (wsid, name) values (900300, 'pending');


CREATE SEQUENCE if not exists workItem_seq;
CREATE TABLE if not exists workItem
(
  wiId bigint NOT NULL DEFAULT nextval('workItem_seq'::regclass),
  wtId bigint NOT NULL references workType(wtId),
  systemId uuid NOT NULL references queueConfig(systemId),
  workTypeId bigint NOT NULL references workType(wtId),
  ignoreUntil timestamp without time zone null,
  retriesLeft int NOT NULL,
  createdAt timestamp without time zone not null,
  doneAt timestamp without time zone null,
  active timestamp without time zone null,
  groupId uuid NULL,
  dependsOnGroups uuid[] NULL,
  dependsOnWorkItem uuid[] NULL,
  statusId int NOT NULL, --TODO
  backoffCount int NOT NULL,
  attempts int NOT NULL,
  cleanupDone timestamp without time zone null,

  CONSTRAINT workItem_pkey PRIMARY KEY (wiId)
);
CREATE INDEX if not exists ix_workItem_wtId ON workItem (wtId);
CREATE INDEX if not exists ix_workItem_systemId ON workItem (systemId);
CREATE INDEX if not exists ix_workItem_ignoreUntil ON workItem (ignoreUntil);
CREATE INDEX if not exists ix_workItem_doneAt ON workItem (doneAt);
CREATE INDEX if not exists ix_workItem_groupId ON workItem (groupId);
CREATE INDEX if not exists ix_workItem_cleanupDone ON workItem (cleanupDone);
CREATE INDEX if not exists ix_workItem_active ON workItem (active);
CREATE INDEX if not exists ix_workItem_statusId ON workItem (statusId);


CREATE SEQUENCE if not exists pendingWorkItem_seq;
CREATE TABLE if not exists pendingWorkItem
(
  piId bigint NOT NULL DEFAULT nextval('pendingWorkItem_seq'::regclass),
  wiId bigint NOT NULL references workItem(wiId),
  createdAt timestamp without time zone not null,
  active timestamp without time zone null,
  parentPendingWorkerItem bigint NOT NULL references pendingWorkItem(piId),

  CONSTRAINT pendingWorkItem_pkey PRIMARY KEY (piId)
);
CREATE INDEX if not exists ix_pendingWorkItem_wiId ON pendingWorkItem (wiId);
CREATE INDEX if not exists ix_pendingWorkItem_active ON pendingWorkItem (active);


CREATE TABLE if not exists queueStatus
( qsId int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT queueStatus_pkey PRIMARY KEY (qsId)
);
insert into queueStatus (qsid, name) values (900200, 'pending');


CREATE SEQUENCE if not exists queue_seq;
CREATE TABLE if not exists queue
(
  qId bigint NOT NULL DEFAULT nextval('queue_seq'::regclass),
  wiId bigint NOT NULL references pendingWorkItem(piId),
  lockedUntil timestamp without time zone null,
  createdAt timestamp without time zone not null,
  startedAt timestamp without time zone null,
  doneAt timestamp without time zone null,
  active timestamp without time zone null,
  heartbeatAt timestamp without time zone null,
  workerName text COLLATE pg_catalog."default" NOT NULL,
  workerInfo text COLLATE pg_catalog."default" NOT NULL,
  cleanupDone timestamp without time zone null,
  statusId int NOT NULL references queueStatus(qsId),

  CONSTRAINT queue_pkey PRIMARY KEY (qId)
);
CREATE INDEX if not exists ix_queue_qid ON queue (qid);
CREATE INDEX if not exists ix_queue_lockedUntil ON queue (lockedUntil);
CREATE INDEX if not exists ix_queue_active ON queue (active);
CREATE INDEX if not exists ix_queue_heartbeatAt ON queue (heartbeatAt);
CREATE INDEX if not exists ix_queue_cleanupDone ON queue (cleanupDone);
CREATE INDEX if not exists ix_queue_statusId ON queue (statusId);


CREATE TABLE if not exists queueLoglevel
( llId int NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT queueLogLevel_pkey PRIMARY KEY (llId)
);
insert into queueloglevel (llid, name) values (900100, 'trace');
insert into queueloglevel (llid, name) values (900101, 'debug');
insert into queueloglevel (llid, name) values (900102, 'info');
insert into queueloglevel (llid, name) values (900103, 'warn');
insert into queueloglevel (llid, name) values (900104, 'error');



CREATE SEQUENCE if not exists queueLog_seq;
CREATE TABLE if not exists queueLog
(
  qlId bigint NOT NULL DEFAULT nextval('queueLog_seq'::regclass),
  qId bigint NOT NULL references queue(qId),
  levelId int not null references queueLogLevel(llId),
  logTitle text COLLATE pg_catalog."default" NOT NULL,
  logType text COLLATE pg_catalog."default" NOT NULL,
  logDetail text COLLATE pg_catalog."default" NULL,
  logData jsonb NULL,
  logAt timestamp without time zone not null,

  CONSTRAINT queueLog_pkey PRIMARY KEY (qlId)
);
CREATE INDEX if not exists ix_queue_qid ON queue (qid);


-- migrate:down
drop table queueLog;
drop table queueLogLevel;
drop table queue;
drop table queueStatus;
drop table pendingWorkItem;
drop table workItem;
drop table workItemStatus;
drop table workType;
drop table queueLock;
drop table queueConfig;

