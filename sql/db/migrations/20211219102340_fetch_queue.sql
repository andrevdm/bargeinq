-- migrate:up
CREATE OR REPLACE FUNCTION bq_fetch_queue(_lock_for_seconds int)
  RETURNS table ( r_qid bigint
                , r_piid bigint
                , r_wiid uuid
                , r_wtid uuid
                , r_wi_name text
                )
  LANGUAGE 'plpgsql'
AS $BODY$
BEGIN
  return query

  -- Fetch and lock a single item, if there are any available
  with
    cte_lock as (
      select
          qid
      from
        bq_queue
      where
        (locked_until is null or locked_until < now())
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
      where
        rq.qid = cte_lock.qid
    )
  update
    bq_queue q
  set
    locked_until = now() + (interval '1 second' * _lock_for_seconds)
  from
    cte_lock
  inner join
    cte_data
  on
    cte_data.qid = cte_lock.qid
  returning
    q.qid, cte_data.piid, cte_data.wiid, cte_data.wtid, cte_data.wi_name;

END
$BODY$;


-- migrate:down
drop FUNCTION bq_fetch_queue(interval);
