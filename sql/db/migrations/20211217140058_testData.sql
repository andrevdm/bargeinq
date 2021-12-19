-- migrate:up
insert into bq_system (system_id, requires_global_lock, poll_period_seconds) values ('36bad147-4370-4940-a0eb-bc08c3edf212', true, 20);

insert into bq_work_type
   ( wtId
   , name
   , system_id
   , default_retries
   , default_backoff_seconds
   , default_heartbeat_check_period
   , default_exec_environment
   , dequeue_lock_period_seconds
   )
values
   ( 'd20ae0bc-c8c5-476c-9486-2ab4aaf9af09'
   , 'testType'
   , '36bad147-4370-4940-a0eb-bc08c3edf212'
   , 1
   , '{1,2,4}'::int[]
   , 10
   , 'execTest'
   , 5
   );

-- migrate:down
delete from bq_work_type where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
delete from bq_system where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
