-- migrate:up
CREATE OR REPLACE FUNCTION fn_bq_queue_notify() RETURNS trigger AS $$
DECLARE
  r1 text;
BEGIN
  FOR r1 IN select i.system_id::text as system_id from bq_work_item i where i.wiid = NEW.wiid
    LOOP
      PERFORM pg_notify('c' || REPLACE(r1, '-', ''), 'trigger');
    END loop;
  RETURN NEW;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER tg_bq_queue_update_notify
  AFTER UPDATE
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

CREATE TRIGGER tg_bq_queue_insert_notify
  AFTER INSERT
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

CREATE TRIGGER tg_bq_queue_delete_notify
  AFTER DELETE
  ON bq_queue
  FOR EACH ROW
  EXECUTE PROCEDURE fn_bq_queue_notify();

-- migrate:down
drop TRIGGER tg_bq_queue_update_notify on bq_queue;
drop TRIGGER tg_bq_queue_insert_notify on bq_queue;
drop TRIGGER tg_bq_queue_insert_delete on bq_queue;
drop function fn_bq_queue_notify();
