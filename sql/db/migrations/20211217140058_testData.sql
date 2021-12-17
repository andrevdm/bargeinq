-- migrate:up
insert into queueConfig (systemId, requiresGlobalLock) values ('36bad147-4370-4940-a0eb-bc08c3edf212', true);

insert into workType
   ( wtId
   , name
   , systemId
   , defaultRetries
   , defaultBackoffSeconds
   , defaultHeartbeatCheckPeriod
   , defaultExecEnvironment
   , dequeueLockPeriodSeconds
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
delete from workType where systemId = '36bad147-4370-4940-a0eb-bc08c3edf212';
delete from queueConfig where systemId = '36bad147-4370-4940-a0eb-bc08c3edf212';
