-- migrate:up
CREATE OR REPLACE FUNCTION fn_listen(channel_ TEXT) RETURNS VOID AS $$
BEGIN
    EXECUTE format('LISTEN %I', channel_);
END
$$ LANGUAGE PLPGSQL;

-- migrate:down
drop FUNCTION if exists fn_listen(channel_ TEXT);