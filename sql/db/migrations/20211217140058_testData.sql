-- migrate:up
insert into queue_config (system_id, requires_global_lock) values ('36bad147-4370-4940-a0eb-bc08c3edf212', true);

insert into work_type
   ( wtId
   , name
   , system_id
   , default_retries
   , default_backoff_seconds
   , default_heartbeat_check_period
   , default_exec_environment
   , dequeue_lock_period_seconds
   , poll_period_seconds
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
   , 60
   );

-- migrate:down
delete from work_type where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
delete from queue_config where system_id = '36bad147-4370-4940-a0eb-bc08c3edf212';
