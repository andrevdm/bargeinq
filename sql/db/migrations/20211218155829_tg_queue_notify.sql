-- migrate:up
CREATE OR REPLACE FUNCTION fn_queue_notify() RETURNS trigger AS $$
DECLARE
  r1 text;
BEGIN
  FOR r1 IN select i.system_id::text as system_id from pending_work_item p inner join work_item i on p.wiid = i.wiid where p.piid = NEW.piid
    LOOP
      PERFORM pg_notify('c' || REPLACE(r1, '-', ''), 'trigger');
    END loop;
  RETURN NEW;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER tg_queue_update_notify
  AFTER UPDATE
  ON queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_queue_notify();

CREATE TRIGGER tg_queue_insert_notify
  AFTER INSERT
  ON queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_queue_notify();

-- migrate:down
drop TRIGGER tg_queue_update_notify on queue;
drop TRIGGER tg_queue_insert_notify on queue;
drop function fn_queue_notify();
