SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: bq_fetch_queue(uuid, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.bq_fetch_queue(_sys_id uuid, _lock_for_seconds integer) RETURNS TABLE(r_qid bigint, r_piid bigint, r_wiid uuid, r_wtid uuid, r_wi_name text, r_dequeued_at timestamp with time zone, r_work_data text)
    LANGUAGE plpgsql
    AS $$
BEGIN
  return query

  -- Fetch and lock a single item, if there are any available
  with
    cte_lock as (
      select
          lq.qid
      from
        bq_queue lq
      inner join
        bq_pending_work_item lpi
      on
        lpi.piid = lq.piid
      inner join
        bq_work_item lwi
      on
        lwi.system_id = _sys_id
      where
        (lq.locked_until is null or lq.locked_until < now())
      limit 1
      for update skip locked
    ),
    cte_data as (
      select
          rq.qid
        , rq.locked_until
        , rpi.piid
        , rwi.wiid
        , rwi.wtid
        , rwi.name as wi_name
        , rq.dequeued_at
        , rwi.work_data
      from
        bq_queue rq
      inner join
        cte_lock
      on
        cte_lock.qid = rq.qid
      inner join
        bq_pending_work_item rpi
      on
        rpi.piid = rq.piid
      inner join
        bq_work_item rwi
      on
        rwi.wiid = rpi.wiid
      where
        rq.qid = cte_lock.qid
    )
  update
    bq_queue q
  set
      locked_until = now() + (interval '1 second' * _lock_for_seconds)
    , dequeued_at = COALESCE(q.dequeued_at, now())
  from
    cte_lock
  inner join
    cte_data
  on
    cte_data.qid = cte_lock.qid
  returning
    q.qid, cte_data.piid, cte_data.wiid, cte_data.wtid, cte_data.wi_name, cte_data.dequeued_at, cte_data.work_data;

END
$$;


--
-- Name: fn_bq_listen(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_bq_listen(channel_ text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    EXECUTE format('LISTEN %I', channel_);
END
$$;


--
-- Name: fn_bq_queue_notify(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_bq_queue_notify() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  r1 text;
BEGIN
  FOR r1 IN select i.system_id::text as system_id from bq_pending_work_item p inner join bq_work_item i on p.wiid = i.wiid where p.piid = NEW.piid
    LOOP
      PERFORM pg_notify('c' || REPLACE(r1, '-', ''), 'trigger');
    END loop;
  RETURN NEW;

  RETURN NEW;
END;
$$;


--
-- Name: bq_pending_work_item_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.bq_pending_work_item_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


SET default_tablespace = '';

--
-- Name: bq_pending_work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_pending_work_item (
    piid bigint DEFAULT nextval('public.bq_pending_work_item_seq'::regclass) NOT NULL,
    wiid uuid NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: bq_queue_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.bq_queue_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: bq_queue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_queue (
    qid bigint DEFAULT nextval('public.bq_queue_seq'::regclass) NOT NULL,
    piid bigint NOT NULL,
    locked_until timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    started_at timestamp with time zone,
    heartbeat_at timestamp with time zone,
    dequeued_at timestamp with time zone
);


--
-- Name: bq_system; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_system (
    system_id uuid NOT NULL,
    requires_global_lock boolean NOT NULL,
    poll_period_seconds integer NOT NULL,
    locked_until timestamp with time zone,
    locked_by text
);


--
-- Name: bq_work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_work_item (
    wiid uuid NOT NULL,
    system_id uuid NOT NULL,
    name text NOT NULL,
    wtid uuid NOT NULL,
    ignore_until timestamp with time zone,
    retries_left integer NOT NULL,
    created_at timestamp with time zone NOT NULL,
    group_id uuid,
    depends_on_groups uuid[],
    depends_on_work_item uuid[],
    backoff_count integer NOT NULL,
    attempts integer NOT NULL,
    work_data text
);


--
-- Name: bq_work_type; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_work_type (
    wtid uuid NOT NULL,
    system_id uuid NOT NULL,
    name text NOT NULL,
    default_retries integer NOT NULL,
    default_backoff_seconds integer[] NOT NULL,
    default_heartbeat_check_period integer,
    default_exec_environment text NOT NULL,
    dequeue_lock_period_seconds integer NOT NULL
);


--
-- Name: dbmate_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dbmate_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: bq_pending_work_item bq_pending_work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_pending_work_item
    ADD CONSTRAINT bq_pending_work_item_pkey PRIMARY KEY (piid);


--
-- Name: bq_queue bq_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_queue
    ADD CONSTRAINT bq_queue_pkey PRIMARY KEY (qid);


--
-- Name: bq_system bq_system_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_system
    ADD CONSTRAINT bq_system_pkey PRIMARY KEY (system_id);


--
-- Name: bq_work_item bq_work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item
    ADD CONSTRAINT bq_work_item_pkey PRIMARY KEY (wiid);


--
-- Name: bq_work_type bq_work_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_type
    ADD CONSTRAINT bq_work_type_pkey PRIMARY KEY (wtid);


--
-- Name: dbmate_migrations dbmate_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dbmate_migrations
    ADD CONSTRAINT dbmate_migrations_pkey PRIMARY KEY (version);


--
-- Name: ix_bq_pending_work_item_wi_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX ix_bq_pending_work_item_wi_id ON public.bq_pending_work_item USING btree (wiid);


--
-- Name: ix_bq_queue_heartbeat_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_heartbeat_at ON public.bq_queue USING btree (heartbeat_at);


--
-- Name: ix_bq_queue_locked_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_locked_until ON public.bq_queue USING btree (locked_until);


--
-- Name: ix_bq_queue_piid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX ix_bq_queue_piid ON public.bq_queue USING btree (piid);


--
-- Name: ix_bq_queue_qid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_qid ON public.bq_queue USING btree (qid);


--
-- Name: ix_bq_work_item_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_group_id ON public.bq_work_item USING btree (group_id);


--
-- Name: ix_bq_work_item_ignore_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_ignore_until ON public.bq_work_item USING btree (ignore_until);


--
-- Name: ix_bq_work_item_system_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_system_id ON public.bq_work_item USING btree (system_id);


--
-- Name: ix_bq_work_item_wtid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_wtid ON public.bq_work_item USING btree (wtid);


--
-- Name: ix_bq_work_type_system_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_type_system_id ON public.bq_work_type USING btree (system_id);


--
-- Name: bq_queue tg_bq_queue_insert_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_bq_queue_insert_notify AFTER INSERT ON public.bq_queue FOR EACH ROW EXECUTE PROCEDURE public.fn_bq_queue_notify();


--
-- Name: bq_queue tg_bq_queue_update_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_bq_queue_update_notify AFTER UPDATE ON public.bq_queue FOR EACH ROW EXECUTE PROCEDURE public.fn_bq_queue_notify();


--
-- Name: bq_pending_work_item bq_pending_work_item_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_pending_work_item
    ADD CONSTRAINT bq_pending_work_item_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.bq_work_item(wiid) ON DELETE CASCADE;


--
-- Name: bq_queue bq_queue_piid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_queue
    ADD CONSTRAINT bq_queue_piid_fkey FOREIGN KEY (piid) REFERENCES public.bq_pending_work_item(piid) ON DELETE CASCADE;


--
-- Name: bq_work_item bq_work_item_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item
    ADD CONSTRAINT bq_work_item_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.bq_system(system_id);


--
-- Name: bq_work_item bq_work_item_wtid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item
    ADD CONSTRAINT bq_work_item_wtid_fkey FOREIGN KEY (wtid) REFERENCES public.bq_work_type(wtid);


--
-- Name: bq_work_type bq_work_type_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_type
    ADD CONSTRAINT bq_work_type_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.bq_system(system_id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.dbmate_migrations (version) VALUES
    ('20211217121058'),
    ('20211217140058'),
    ('20211218143535'),
    ('20211218155829'),
    ('20211219102340');
