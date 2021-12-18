-- migrate:up
CREATE OR REPLACE FUNCTION fn_queue_notify() RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('c' || REPLACE(CAST('qId' AS text), '-', ''), 'trigger');
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
