-- migrate:up
CREATE OR REPLACE FUNCTION bq_fetch_queue(_sys_id uuid)
  RETURNS table ( r_qid bigint
                , r_piid bigint
                , r_wiid uuid
                , r_wtid uuid
                , r_wi_name text
                , r_dequeued_at timestamp with time zone
                , r_work_data text
                )
  LANGUAGE 'plpgsql'
AS $BODY$
BEGIN
  return query

  -- Fetch and lock a single item, if there are any available
  with
    cte_lock as (
      select
          lq.qid
      from
        bq_queue lq
      inner join
        bq_pending_work_item lpi
      on
        lpi.piid = lq.piid
      inner join
        bq_work_item lwi
      on
        lwi.system_id = _sys_id
      where
        (lq.locked_until is null or lq.locked_until < now())
      limit 1
      for update skip locked
    ),
    cte_data as (
      select
          rq.qid
        , rq.locked_until
        , rpi.piid
        , rwi.wiid
        , rwi.wtid
        , rwi.name as wi_name
        , rq.dequeued_at
        , rwi.work_data
        , rwt.dequeue_lock_period_seconds
      from
        bq_queue rq
      inner join
        cte_lock
      on
        cte_lock.qid = rq.qid
      inner join
        bq_pending_work_item rpi
      on
        rpi.piid = rq.piid
      inner join
        bq_work_item rwi
      on
        rwi.wiid = rpi.wiid
      inner join
        bq_work_type rwt
      on
        rwi.wtid = rwt.wtid
      where
        rq.qid = cte_lock.qid
    )
  update
    bq_queue q
  set
      locked_until = now() + (interval '1 second' * cte_data.dequeue_lock_period_seconds)
    , dequeued_at = COALESCE(q.dequeued_at, now())
  from
    cte_lock
  inner join
    cte_data
  on
    cte_data.qid = cte_lock.qid
  returning
    q.qid, cte_data.piid, cte_data.wiid, cte_data.wtid, cte_data.wi_name, cte_data.dequeued_at, cte_data.work_data;

END
$BODY$;


-- migrate:down
drop FUNCTION bq_fetch_queue(uuid, int);
