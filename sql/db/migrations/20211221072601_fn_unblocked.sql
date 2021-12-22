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


