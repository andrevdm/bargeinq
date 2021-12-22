-- migrate:up
CREATE OR REPLACE FUNCTION bq_fetch_queue(_sys_id uuid)
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
  active_items integer := (
    select
      count(1)
    from
      bq_queue q
    inner join bq_work_item wi
      on q.wiid = q.wiid
    where
      wi.system_id = _sys_id
      and q.frId is null
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
             and ((max_items is null) or (active_items <= max_items)) -- Only get if no limit, or not above max items
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
drop FUNCTION bq_fetch_queue(uuid);
