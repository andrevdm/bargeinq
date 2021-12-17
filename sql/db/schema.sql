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
    wiid bigint NOT NULL,
    created_at timestamp without time zone NOT NULL,
    active timestamp without time zone,
    parent_pending_worker_item bigint NOT NULL
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
    wiid bigint NOT NULL,
    locked_until timestamp without time zone,
    created_at timestamp without time zone NOT NULL,
    started_at timestamp without time zone,
    done_at timestamp without time zone,
    active timestamp without time zone,
    heartbeat_at timestamp without time zone,
    worker_name text NOT NULL,
    worker_info text NOT NULL,
    cleanup_done timestamp without time zone,
    status_id integer NOT NULL
);


--
-- Name: queue_config; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_config (
    system_id uuid NOT NULL,
    requires_global_lock boolean NOT NULL
);


--
-- Name: queue_lock; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_lock (
    system_id uuid NOT NULL,
    host text NOT NULL,
    locked_until timestamp without time zone
);


--
-- Name: queue_log_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.queue_log_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: queue_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_log (
    qlid bigint DEFAULT nextval('public.queue_log_seq'::regclass) NOT NULL,
    qid bigint NOT NULL,
    level_id integer NOT NULL,
    log_title text NOT NULL,
    log_type text NOT NULL,
    log_detail text,
    log_data jsonb,
    log_at timestamp without time zone NOT NULL
);


--
-- Name: queue_log_job; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_log_job (
    jobid uuid NOT NULL
)
INHERITS (public.queue_log);


--
-- Name: queue_log_level; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_log_level (
    llid integer NOT NULL,
    name text NOT NULL
);


--
-- Name: queue_log_sample; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_log_sample (
    jobid uuid NOT NULL,
    sampleid uuid NOT NULL
)
INHERITS (public.queue_log);


--
-- Name: queue_status; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queue_status (
    qsid integer NOT NULL,
    name text NOT NULL
);


--
-- Name: work_item_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.work_item_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.work_item (
    wiid bigint DEFAULT nextval('public.work_item_seq'::regclass) NOT NULL,
    system_id uuid NOT NULL,
    wtid uuid NOT NULL,
    ignore_until timestamp without time zone,
    retries_left integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    done_at timestamp without time zone,
    active timestamp without time zone,
    group_id uuid,
    depends_on_groups uuid[],
    depends_on_work_item uuid[],
    status_id integer NOT NULL,
    backoff_count integer NOT NULL,
    attempts integer NOT NULL,
    cleanup_done timestamp without time zone
);


--
-- Name: work_item_status; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.work_item_status (
    wsid integer NOT NULL,
    name text NOT NULL
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
-- Name: queue_log_job qlid; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log_job ALTER COLUMN qlid SET DEFAULT nextval('public.queue_log_seq'::regclass);


--
-- Name: queue_log_sample qlid; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log_sample ALTER COLUMN qlid SET DEFAULT nextval('public.queue_log_seq'::regclass);


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
-- Name: queue_config queue_config_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_config
    ADD CONSTRAINT queue_config_pkey PRIMARY KEY (system_id);


--
-- Name: queue_lock queue_lock_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_lock
    ADD CONSTRAINT queue_lock_pkey PRIMARY KEY (system_id);


--
-- Name: queue_log_level queue_log_level_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log_level
    ADD CONSTRAINT queue_log_level_pkey PRIMARY KEY (llid);


--
-- Name: queue_log queue_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log
    ADD CONSTRAINT queue_log_pkey PRIMARY KEY (qlid);


--
-- Name: queue queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_pkey PRIMARY KEY (qid);


--
-- Name: queue_status queue_status_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_status
    ADD CONSTRAINT queue_status_pkey PRIMARY KEY (qsid);


--
-- Name: work_item work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_pkey PRIMARY KEY (wiid);


--
-- Name: work_item_status work_item_status_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item_status
    ADD CONSTRAINT work_item_status_pkey PRIMARY KEY (wsid);


--
-- Name: work_type work_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_type
    ADD CONSTRAINT work_type_pkey PRIMARY KEY (wtid);


--
-- Name: ix_pending_work_item_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_pending_work_item_active ON public.pending_work_item USING btree (active);


--
-- Name: ix_pending_work_item_wi_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_pending_work_item_wi_id ON public.pending_work_item USING btree (wiid);


--
-- Name: ix_queue_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_active ON public.queue USING btree (active);


--
-- Name: ix_queue_cleanup_done; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_cleanup_done ON public.queue USING btree (cleanup_done);


--
-- Name: ix_queue_heartbeat_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_heartbeat_at ON public.queue USING btree (heartbeat_at);


--
-- Name: ix_queue_locked_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_locked_until ON public.queue USING btree (locked_until);


--
-- Name: ix_queue_log_job_jobid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_log_job_jobid ON public.queue_log_job USING btree (jobid);


--
-- Name: ix_queue_log_sample_job_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_log_sample_job_id ON public.queue_log_sample USING btree (jobid);


--
-- Name: ix_queue_log_sample_sample_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_log_sample_sample_id ON public.queue_log_sample USING btree (sampleid);


--
-- Name: ix_queue_qid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_qid ON public.queue USING btree (qid);


--
-- Name: ix_queue_status_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_status_id ON public.queue USING btree (status_id);


--
-- Name: ix_work_item_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_active ON public.work_item USING btree (active);


--
-- Name: ix_work_item_cleanup_done; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_cleanup_done ON public.work_item USING btree (cleanup_done);


--
-- Name: ix_work_item_done_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_done_at ON public.work_item USING btree (done_at);


--
-- Name: ix_work_item_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_group_id ON public.work_item USING btree (group_id);


--
-- Name: ix_work_item_ignore_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_ignore_until ON public.work_item USING btree (ignore_until);


--
-- Name: ix_work_item_status_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_work_item_status_id ON public.work_item USING btree (status_id);


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
-- Name: queue_lock queue_lock_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_lock
    ADD CONSTRAINT queue_lock_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.queue_config(system_id);


--
-- Name: queue_log queue_log_level_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log
    ADD CONSTRAINT queue_log_level_id_fkey FOREIGN KEY (level_id) REFERENCES public.queue_log_level(llid);


--
-- Name: queue_log queue_log_qid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue_log
    ADD CONSTRAINT queue_log_qid_fkey FOREIGN KEY (qid) REFERENCES public.queue(qid);


--
-- Name: queue queue_status_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_status_id_fkey FOREIGN KEY (status_id) REFERENCES public.queue_status(qsid);


--
-- Name: queue queue_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.pending_work_item(piid);


--
-- Name: work_item work_item_status_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_status_id_fkey FOREIGN KEY (status_id) REFERENCES public.work_item_status(wsid);


--
-- Name: work_item work_item_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.queue_config(system_id);


--
-- Name: work_item work_item_wtid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_item
    ADD CONSTRAINT work_item_wtid_fkey FOREIGN KEY (wtid) REFERENCES public.work_type(wtid);


--
-- Name: work_type work_type_system_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.work_type
    ADD CONSTRAINT work_type_system_id_fkey FOREIGN KEY (system_id) REFERENCES public.queue_config(system_id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.dbmate_migrations (version) VALUES
    ('20211217121058'),
    ('20211217131927'),
    ('20211217140058');
