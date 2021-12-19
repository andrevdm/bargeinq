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
-- Name: fn_listen(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_listen(channel_ text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    EXECUTE format('LISTEN %I', channel_);
END
$$;


--
-- Name: fn_queue_notify(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_queue_notify() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
$$;


SET default_tablespace = '';

--
-- Name: dbmate_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dbmate_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: pending_work_item_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pending_work_item_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pending_work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_work_item (
    piid bigint DEFAULT nextval('public.pending_work_item_seq'::regclass) NOT NULL,
    wiid uuid NOT NULL,
    created_at timestamp without time zone NOT NULL,
    parent_pending_worker_item bigint
);


--
-- Name: queue_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.queue_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: queue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue (
    qid bigint DEFAULT nextval('public.queue_seq'::regclass) NOT NULL,
    piid bigint NOT NULL,
    locked_until timestamp without time zone,
    created_at timestamp without time zone NOT NULL,
    started_at timestamp without time zone,
    heartbeat_at timestamp without time zone
);


--
-- Name: system_config; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.system_config (
    system_id uuid NOT NULL,
    requires_global_lock boolean NOT NULL,
    poll_period_seconds integer NOT NULL,
    locked_until timestamp without time zone,
    locked_by text
);


--
-- Name: work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.work_item (
    wiid uuid NOT NULL,
    system_id uuid NOT NULL,
    name text NOT NULL,
    wtid uuid NOT NULL,
    ignore_until timestamp without time zone,
    retries_left integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    group_id uuid,
    depends_on_groups uuid[],
    depends_on_work_item uuid[],
    backoff_count integer NOT NULL,
    attempts integer NOT NULL
);


--
-- Name: work_type; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.work_type (
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
-- Name: dbmate_migrations dbmate_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dbmate_migrations
    ADD CONSTRAINT dbmate_migrations_pkey PRIMARY KEY (version);


--
-- Name: pending_work_item pending_work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_work_item
    ADD CONSTRAINT pending_work_item_pkey PRIMARY KEY (piid);


--
-- Name: system_config queue_config_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_config
    ADD CONSTRAINT queue_config_pkey PRIMARY KEY (system_id);


--
-- Name: queue queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_pkey PRIMARY KEY (qid);


--
-- Name: work_item work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_pkey PRIMARY KEY (wiid);


--
-- Name: work_type work_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_type
    ADD CONSTRAINT work_type_pkey PRIMARY KEY (wtid);


--
-- Name: ix_pending_work_item_wi_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_pending_work_item_wi_id ON public.pending_work_item USING btree (wiid);


--
-- Name: ix_queue_heartbeat_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_heartbeat_at ON public.queue USING btree (heartbeat_at);


--
-- Name: ix_queue_locked_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_locked_until ON public.queue USING btree (locked_until);


--
-- Name: ix_queue_piid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX ix_queue_piid ON public.queue USING btree (piid);


--
-- Name: ix_queue_qid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_qid ON public.queue USING btree (qid);


--
-- Name: ix_work_item_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_group_id ON public.work_item USING btree (group_id);


--
-- Name: ix_work_item_ignore_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_ignore_until ON public.work_item USING btree (ignore_until);


--
-- Name: ix_work_item_system_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_system_id ON public.work_item USING btree (system_id);


--
-- Name: ix_work_item_wtid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_wtid ON public.work_item USING btree (wtid);


--
-- Name: ix_work_type_system_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_type_system_id ON public.work_type USING btree (system_id);


--
-- Name: queue tg_queue_insert_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_queue_insert_notify AFTER INSERT ON public.queue FOR EACH ROW EXECUTE PROCEDURE public.fn_queue_notify();


--
-- Name: queue tg_queue_update_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_queue_update_notify AFTER UPDATE ON public.queue FOR EACH ROW EXECUTE PROCEDURE public.fn_queue_notify();


--
-- Name: pending_work_item pending_work_item_parent_pending_worker_item_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_work_item
    ADD CONSTRAINT pending_work_item_parent_pending_worker_item_fkey FOREIGN KEY (parent_pending_worker_item) REFERENCES public.pending_work_item(piid);


--
-- Name: pending_work_item pending_work_item_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_work_item
    ADD CONSTRAINT pending_work_item_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.work_item(wiid);


--
-- Name: queue queue_piid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_piid_fkey FOREIGN KEY (piid) REFERENCES public.pending_work_item(piid);


--
-- Name: work_item work_item_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.system_config(system_id);


--
-- Name: work_item work_item_wtid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_wtid_fkey FOREIGN KEY (wtid) REFERENCES public.work_type(wtid);


--
-- Name: work_type work_type_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_type
    ADD CONSTRAINT work_type_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.system_config(system_id);


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
    ('20211218155829');
