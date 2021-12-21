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
      and not exists (select wiid_blocked from bq_work_item_blockers where wiid_blocked = wi.wiid)

-- migrate:down
drop view vw_bq_unblocked_unqueued
