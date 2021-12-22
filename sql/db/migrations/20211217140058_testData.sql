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
