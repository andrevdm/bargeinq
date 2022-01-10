{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BargeInQueue.Migrations
    ( getMigrations
    ) where

import           Verset
import           Text.RawString.QQ (r)


getMigrations :: [(FilePath, ByteString)]
getMigrations = [ ("20211217121058_create", sql_20211217121058_create)
                , ("20211217140058_testData", sql_20211217140058_testData)
                , ("20211218143535_fn_listen", sql_20211218143535_fn_listen)
                , ("20211218155829_tg_queue_notify", sql_20211218155829_tg_queue_notify)
                , ("20211219102340_fetch_queue", sql_20211219102340_fetch_queue)
                , ("20211221065337_vw_unblocked_unqueued", sql_20211221065337_vw_unblocked_unqueued)
                , ("20211221072601_fn_unblocked", sql_20211221072601_fn_unblocked)
                ]


sql_20211217121058_create :: ByteString
sql_20211217121058_create = [r|
-- migrate:up
CREATE TABLE if not exists bq_system
(
  system_id uuid NOT NULL,
  name text COLLATE pg_catalog."default" NOT NULL,
  poll_period_seconds int NOT NULL,
  locked_until timestamp with time zone null,
  locked_by text COLLATE pg_catalog."default" null,
  max_active_items int null,
  auto_queue_unblocked bool not null,
  heartbeat_check_period_seconds int NULL,

  CONSTRAINT bq_system_pkey PRIMARY KEY (system_id)
);


CREATE TABLE if not exists bq_work_type
(
  wtId uuid NOT NULL,
  system_id uuid NOT NULL references bq_system(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  default_retries int NOT NULL,
  default_backoff_seconds int[] NOT NULL,
  default_exec_environment text COLLATE pg_catalog."default" NOT NULL,
  dequeue_lock_period_seconds int NOT NULL,
  heartbeat_expected_every_seconds int NULL,
  heartbeat_num_missed_for_error int NULL,

  CONSTRAINT bq_work_type_pkey PRIMARY KEY (wtId)
);
CREATE INDEX if not exists ix_bq_work_type_system_id ON bq_work_type (system_id);
alter table bq_work_type ADD CONSTRAINT chk_work_type_heartbeat_nulls CHECK
               ( ((heartbeat_expected_every_seconds is null) and (heartbeat_num_missed_for_error is null))
                or
                 ((heartbeat_expected_every_seconds is not null) and (heartbeat_num_missed_for_error is not null))
               );


CREATE TABLE if not exists bq_work_item
(
  wiid uuid NOT NULL,
  system_id uuid NOT NULL references bq_system(system_id),
  name text COLLATE pg_catalog."default" NOT NULL,
  wtid uuid NOT NULL references bq_work_type(wtId),
  ignore_until timestamp with time zone null,
  retries_left int NOT NULL,
  created_at timestamp with time zone not null,
  group_id uuid NULL,
  backoff_count int NOT NULL,
  attempts int NOT NULL,
  work_data text NULL,
  priority int not null,
  backoff_seconds_override int[] NULL,
  exec_environment_override text COLLATE pg_catalog."default" NULL,

  CONSTRAINT bq_work_item_pkey PRIMARY KEY (wiId)
);
CREATE INDEX if not exists ix_bq_work_item_wtId ON bq_work_item (wtId);
CREATE INDEX if not exists ix_bq_work_item_system_id ON bq_work_item (system_id);
CREATE INDEX if not exists ix_bq_work_item_ignore_until ON bq_work_item (ignore_until);
--CREATE INDEX if not exists ix_bq_work_item_group_id ON bq_work_item (group_id);
CREATE INDEX if not exists ix_bq_work_item_priority ON bq_work_item (priority);

CREATE TABLE if not exists bq_work_item_blockers
(
  wiid_blocked uuid NOT NULL references bq_work_item(wiId) ON DELETE CASCADE,
  wiId_blocker uuid NOT NULL references bq_work_item(wiId) ON DELETE CASCADE,

  CONSTRAINT bq_work_item_blockers_pkey PRIMARY KEY (wiid_blocked, wiId_blocker)
);
CREATE INDEX if not exists ix_bq_work_item_blockers_blocked ON bq_work_item_blockers (wiid_blocked);
CREATE INDEX if not exists ix_bq_work_item_blockers_blocker ON bq_work_item_blockers (wiid_blocker);
alter table bq_work_item_blockers ADD CONSTRAINT chk_work_item_blockers_no_self_ref CHECK (wiid_blocked <> wiid_blocker);


CREATE TABLE if not exists bq_fail_reason
(
  frId int NOT NULL,
  frName text COLLATE pg_catalog."default" NOT NULL,

  CONSTRAINT bq_pk_fail_reason PRIMARY KEY (frId)
);

insert into bq_fail_reason (frId, frName) values (543000, 'error');
insert into bq_fail_reason (frId, frName) values (543001, 'heartbeat timeout');
insert into bq_fail_reason (frId, frName) values (543002, 'manual fail');
insert into bq_fail_reason (frId, frName) values (543003, 'manual expire');
insert into bq_fail_reason (frId, frName) values (543004, 'timeout');
insert into bq_fail_reason (frId, frName) values (543005, 'user abort');


CREATE SEQUENCE if not exists bq_queue_seq;
CREATE TABLE if not exists bq_queue
(
  qId bigint NOT NULL DEFAULT nextval('bq_queue_seq'::regclass),
  wiId uuid NOT NULL references bq_work_item(wiid) ON DELETE CASCADE,
  locked_until timestamp with time zone null,
  created_at timestamp with time zone not null,
  heartbeat_at timestamp with time zone null,
  dequeued_at timestamp with time zone null,
  frId int null references bq_fail_reason (frId),
  host text COLLATE pg_catalog."default" NULL,

  CONSTRAINT bq_queue_pkey PRIMARY KEY (qId)
);
CREATE INDEX if not exists ix_bq_queue_qid ON bq_queue (qid);
CREATE unique INDEX if not exists ix_bq_queue_wiid ON bq_queue (wiid);
CREATE INDEX if not exists ix_bq_queue_locked_until ON bq_queue (locked_until);
CREATE INDEX if not exists ix_bq_queue_heartbeat_at ON bq_queue (heartbeat_at);
CREATE INDEX if not exists ix_bq_queue_fail_readon ON bq_queue (frId);
CREATE INDEX if not exists ix_bq_queue_host ON bq_queue (host);


-- migrate:down
drop table bq_queue;
drop sequence bq_queue_seq;
drop table bq_work_item;
drop table bq_work_type;
drop table bq_system;
drop table bq_fail_reason;
|]

sql_20211217140058_testData :: ByteString
sql_20211217140058_testData = [r|
-- migrate:up
insert into bq_system
  (system_id, name, poll_period_seconds, auto_queue_unblocked, max_active_items, heartbeat_check_period_seconds)
values
('36bad147-4370-4940-a0eb-bc08c3edf212', 'testSys', 20, true, null, 10);

insert into bq_work_type
   ( wtId
   , name
   , system_id
   , default_retries
   , default_backoff_seconds
   , default_exec_environment
   , dequeue_lock_period_seconds
   , heartbeat_expected_every_seconds
   , heartbeat_num_missed_for_error
   )
values
   ( 'd20ae0bc-c8c5-476c-9486-2ab4aaf9af09'
   , 'testType'
   , '36bad147-4370-4940-a0eb-bc08c3edf212'
   , 1
   , '{1,2,4}'::int[]
   , 'execTest'
   , 5
   , 8
   , 2
   );

-- migrate:down
delete from bq_work_type where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
delete from bq_system where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
|]


sql_20211218143535_fn_listen :: ByteString
sql_20211218143535_fn_listen = [r|
-- migrate:up
CREATE OR REPLACE FUNCTION fn_bq_listen(channel_ TEXT) RETURNS VOID AS $$
BEGIN
    EXECUTE format('LISTEN %I', channel_);
END
$$ LANGUAGE PLPGSQL;

-- migrate:down
drop FUNCTION if exists fn_bq_listen(channel_ TEXT);
|]


sql_20211218155829_tg_queue_notify :: ByteString
sql_20211218155829_tg_queue_notify = [r|
-- migrate:up
CREATE OR REPLACE FUNCTION fn_bq_queue_notify() RETURNS trigger AS $$
DECLARE
  r1 text;
BEGIN
  FOR r1 IN select i.system_id::text as system_id from bq_work_item i where i.wiid = NEW.wiid
    LOOP
      PERFORM pg_notify('c' || REPLACE(r1, '-', ''), 'trigger');
    END loop;
  RETURN NEW;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER tg_bq_queue_update_notify
  AFTER UPDATE
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

CREATE TRIGGER tg_bq_queue_insert_notify
  AFTER INSERT
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

CREATE TRIGGER tg_bq_queue_delete_notify
  AFTER DELETE
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

-- migrate:down
drop TRIGGER tg_bq_queue_update_notify on bq_queue;
drop TRIGGER tg_bq_queue_insert_notify on bq_queue;
drop TRIGGER tg_bq_queue_insert_delete on bq_queue;
drop function fn_bq_queue_notify();
|]


sql_20211219102340_fetch_queue :: ByteString
sql_20211219102340_fetch_queue = [r|
-- migrate:up
CREATE OR REPLACE FUNCTION bq_fetch_queue(_sys_id uuid, _host_name text, _host_max_items int)
  RETURNS table ( r_qid bigint
                , r_wiid uuid
                , r_wtid uuid
                , r_wi_name text
                , r_dequeued_at timestamp with time zone
                , r_work_data text
                , r_frid int
                )
  LANGUAGE 'plpgsql'
AS $BODY$
DECLARE
  max_items integer := (select max_active_items from bq_system where system_id = _sys_id);
  total_active_items integer := (
    select
      count(1)
    from
      bq_queue q
    inner join bq_work_item wi
      on q.wiid = wi.wiid
    where
      wi.system_id = _sys_id
      and q.frId is null
      and (q.dequeued_at is not null and ( q.locked_until is not null
                                           or q.locked_until >= now()
                                         )
          )
  );
  host_active_items integer := (
    select
      count(1)
    from
      bq_queue q
    inner join bq_work_item wi
      on q.wiid = wi.wiid
    where
      wi.system_id = _sys_id
      and q.frId is null
      and q.host = _host_name
      and (q.dequeued_at is not null and ( q.locked_until is not null
                                           or q.locked_until >= now()
                                         )
          )
  );
BEGIN
  return query
  -- Fetch and lock a single item, if there are any available
  with
    cte_lock as (
      -- Get items that have failed first.
      -- These will not result in new work immediately, they may result in new items in the queue to to processed later
      -- Note that these items are returned even if active > max, since they wont result in more running jobs immediately
      select
        xq.qid
      from
        bq_queue xq
      inner join bq_work_item xwi
        on xwi.wiid = xq.wiid
      where exists
        (
          (select
             lq.qid, 0
           from
             bq_queue lq
           inner join bq_work_item lwi
             on lwi.wiid = lq.wiid
           where
             lq.qid = xq.qid
             and lwi.system_id = _sys_id
             and lq.frId is not null
           order by
             lwi.priority desc, lwi.created_at asc
           limit 1
          )
          union
          -- Now get items that have not failed
          (
           select
             lq.qid, 1
           from
             bq_queue lq
           inner join bq_work_item lwi
             on lwi.wiid = lq.wiid
           where
             lq.qid = xq.qid
             and lwi.system_id = _sys_id
             and lq.frId is null
             and (lq.locked_until is null or lq.locked_until < now())
             and ((max_items is null) or (total_active_items < max_items)) -- Only get if no limit, or not above max items
             and ((_host_name is null) or (_host_max_items is null) or (host_active_items < _host_max_items))
           order by
             lwi.priority desc, lwi.created_at asc
           limit 1
          )
        )
      order by
        xq.frId NULLS LAST, xwi.priority desc, xq.created_at asc -- errors before non-error
      limit 1
      for update skip locked
    ),
    cte_data as (
      select
          rq.qid
        , rq.locked_until
        , rwi.wiid
        , rwi.wtid
        , rwi.name as wi_name
        , rq.dequeued_at
        , rwi.work_data
        , rwt.dequeue_lock_period_seconds
        , rq.frId
      from
        cte_lock
      inner join bq_queue rq
        on cte_lock.qid = rq.qid
      inner join bq_work_item rwi
        on rwi.wiid = rq.wiid
      inner join bq_work_type rwt
        on rwi.wtid = rwt.wtid
    )
  update
    bq_queue q
  set
      locked_until = now() + (interval '1 second' * cte_data.dequeue_lock_period_seconds)
    , dequeued_at = COALESCE(q.dequeued_at, now())
    , host = _host_name
  from
    cte_lock
  inner join cte_data
    on cte_data.qid = cte_lock.qid
  where
	  cte_lock.qid = q.qid
  returning
    q.qid, cte_data.wiid, cte_data.wtid, cte_data.wi_name, cte_data.dequeued_at, cte_data.work_data, cte_data.frId;

END
$BODY$;


-- migrate:down
drop FUNCTION bq_fetch_queue(uuid, text, int);
|]


sql_20211221065337_vw_unblocked_unqueued :: ByteString
sql_20211221065337_vw_unblocked_unqueued = [r|
-- migrate:up
create view vw_bq_unblocked_unqueued as
    select
        wi.wiid
      , wi.name
      , wi.wtid
      , wi.system_id
      , wi.ignore_until
      , wi.retries_left
      , wi.created_at
      , wi.group_id
      , wi.backoff_count
      , wi.attempts
      , wi.work_data
    from
      bq_work_item wi
    left outer join bq_queue q
      on q.wiid = wi.wiid
    where
      q.qid is null
      and not exists (select wiid_blocked from bq_work_item_blockers where wiid_blocked = wi.wiid);


-- migrate:down
drop view vw_bq_unblocked_unqueued;
|]


sql_20211221072601_fn_unblocked :: ByteString
sql_20211221072601_fn_unblocked = [r|
-- migrate:up
CREATE OR REPLACE FUNCTION fn_bq_queue_all_unblocked(_sys_id uuid) RETURNS VOID AS $$
BEGIN
  insert into bq_queue
    (wiid, created_at)
    (select v.wiid, now() from vw_bq_unblocked_unqueued v where v.system_id = _sys_id and (v.ignore_until is null or v.ignore_until < now()))
    on conflict do nothing;
END
$$ LANGUAGE PLPGSQL;


-- migrate:down
drop FUNCTION fn_bq_queue_all_unblocked(uuid);


|]

