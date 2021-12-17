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
-- Name: pendingworkitem_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pendingworkitem_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pendingworkitem; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pendingworkitem (
    piid bigint DEFAULT nextval('public.pendingworkitem_seq'::regclass) NOT NULL,
    wiid bigint NOT NULL,
    createdat timestamp without time zone NOT NULL,
    active timestamp without time zone,
    parentpendingworkeritem bigint NOT NULL
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
    lockeduntil timestamp without time zone,
    createdat timestamp without time zone NOT NULL,
    startedat timestamp without time zone,
    doneat timestamp without time zone,
    active timestamp without time zone,
    heartbeatat timestamp without time zone,
    workername text NOT NULL,
    workerinfo text NOT NULL,
    cleanupdone timestamp without time zone,
    statusid integer NOT NULL
);


--
-- Name: queueconfig; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queueconfig (
    systemid uuid NOT NULL,
    requiresgloballock boolean NOT NULL
);


--
-- Name: queuelock; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queuelock (
    systemid uuid NOT NULL,
    host text NOT NULL,
    lockeduntil timestamp without time zone
);


--
-- Name: queuelog_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.queuelog_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: queuelog; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queuelog (
    qlid bigint DEFAULT nextval('public.queuelog_seq'::regclass) NOT NULL,
    qid bigint NOT NULL,
    levelid integer NOT NULL,
    logtitle text NOT NULL,
    logtype text NOT NULL,
    logdetail text,
    logdata jsonb,
    logat timestamp without time zone NOT NULL
);


--
-- Name: queuelogjob; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queuelogjob (
    jobid uuid NOT NULL
)
INHERITS (public.queuelog);


--
-- Name: queueloglevel; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queueloglevel (
    llid integer NOT NULL,
    name text NOT NULL
);


--
-- Name: queuelogsample; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queuelogsample (
    jobid uuid NOT NULL,
    sampleid uuid NOT NULL
)
INHERITS (public.queuelog);


--
-- Name: queuestatus; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.queuestatus (
    qsid integer NOT NULL,
    name text NOT NULL
);


--
-- Name: workitem_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.workitem_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: workitem; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workitem (
    wiid bigint DEFAULT nextval('public.workitem_seq'::regclass) NOT NULL,
    systemid uuid NOT NULL,
    wtid uuid NOT NULL,
    ignoreuntil timestamp without time zone,
    retriesleft integer NOT NULL,
    createdat timestamp without time zone NOT NULL,
    doneat timestamp without time zone,
    active timestamp without time zone,
    groupid uuid,
    dependsongroups uuid[],
    dependsonworkitem uuid[],
    statusid integer NOT NULL,
    backoffcount integer NOT NULL,
    attempts integer NOT NULL,
    cleanupdone timestamp without time zone
);


--
-- Name: workitemstatus; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workitemstatus (
    wsid integer NOT NULL,
    name text NOT NULL
);


--
-- Name: worktype; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.worktype (
    wtid uuid NOT NULL,
    systemid uuid NOT NULL,
    name text NOT NULL,
    defaultretries integer NOT NULL,
    defaultbackoffseconds integer[] NOT NULL,
    defaultheartbeatcheckperiod integer,
    defaultexecenvironment text NOT NULL,
    dequeuelockperiodseconds integer NOT NULL
);


--
-- Name: queuelogjob qlid; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelogjob ALTER COLUMN qlid SET DEFAULT nextval('public.queuelog_seq'::regclass);


--
-- Name: queuelogsample qlid; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelogsample ALTER COLUMN qlid SET DEFAULT nextval('public.queuelog_seq'::regclass);


--
-- Name: dbmate_migrations dbmate_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dbmate_migrations
    ADD CONSTRAINT dbmate_migrations_pkey PRIMARY KEY (version);


--
-- Name: pendingworkitem pendingworkitem_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pendingworkitem
    ADD CONSTRAINT pendingworkitem_pkey PRIMARY KEY (piid);


--
-- Name: queue queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_pkey PRIMARY KEY (qid);


--
-- Name: queueconfig queueconfig_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queueconfig
    ADD CONSTRAINT queueconfig_pkey PRIMARY KEY (systemid);


--
-- Name: queuelock queuelock_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelock
    ADD CONSTRAINT queuelock_pkey PRIMARY KEY (systemid);


--
-- Name: queuelog queuelog_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelog
    ADD CONSTRAINT queuelog_pkey PRIMARY KEY (qlid);


--
-- Name: queueloglevel queueloglevel_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queueloglevel
    ADD CONSTRAINT queueloglevel_pkey PRIMARY KEY (llid);


--
-- Name: queuestatus queuestatus_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuestatus
    ADD CONSTRAINT queuestatus_pkey PRIMARY KEY (qsid);


--
-- Name: workitem workitem_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workitem
    ADD CONSTRAINT workitem_pkey PRIMARY KEY (wiid);


--
-- Name: workitemstatus workitemstatus_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workitemstatus
    ADD CONSTRAINT workitemstatus_pkey PRIMARY KEY (wsid);


--
-- Name: worktype worktype_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.worktype
    ADD CONSTRAINT worktype_pkey PRIMARY KEY (wtid);


--
-- Name: ix_pendingworkitem_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_pendingworkitem_active ON public.pendingworkitem USING btree (active);


--
-- Name: ix_pendingworkitem_wiid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_pendingworkitem_wiid ON public.pendingworkitem USING btree (wiid);


--
-- Name: ix_queue_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_active ON public.queue USING btree (active);


--
-- Name: ix_queue_cleanupdone; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_cleanupdone ON public.queue USING btree (cleanupdone);


--
-- Name: ix_queue_heartbeatat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_heartbeatat ON public.queue USING btree (heartbeatat);


--
-- Name: ix_queue_lockeduntil; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_lockeduntil ON public.queue USING btree (lockeduntil);


--
-- Name: ix_queue_qid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_qid ON public.queue USING btree (qid);


--
-- Name: ix_queue_statusid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queue_statusid ON public.queue USING btree (statusid);


--
-- Name: ix_queuelogjob_jobid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queuelogjob_jobid ON public.queuelogjob USING btree (jobid);


--
-- Name: ix_queuelogsample_jobid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queuelogsample_jobid ON public.queuelogsample USING btree (jobid);


--
-- Name: ix_queuelogsample_sampleid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_queuelogsample_sampleid ON public.queuelogsample USING btree (sampleid);


--
-- Name: ix_workitem_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_active ON public.workitem USING btree (active);


--
-- Name: ix_workitem_cleanupdone; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_cleanupdone ON public.workitem USING btree (cleanupdone);


--
-- Name: ix_workitem_doneat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_doneat ON public.workitem USING btree (doneat);


--
-- Name: ix_workitem_groupid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_groupid ON public.workitem USING btree (groupid);


--
-- Name: ix_workitem_ignoreuntil; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_ignoreuntil ON public.workitem USING btree (ignoreuntil);


--
-- Name: ix_workitem_statusid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_statusid ON public.workitem USING btree (statusid);


--
-- Name: ix_workitem_systemid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_systemid ON public.workitem USING btree (systemid);


--
-- Name: ix_workitem_wtid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_workitem_wtid ON public.workitem USING btree (wtid);


--
-- Name: ix_worktype_systemid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_worktype_systemid ON public.worktype USING btree (systemid);


--
-- Name: pendingworkitem pendingworkitem_parentpendingworkeritem_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pendingworkitem
    ADD CONSTRAINT pendingworkitem_parentpendingworkeritem_fkey FOREIGN KEY (parentpendingworkeritem) REFERENCES public.pendingworkitem(piid);


--
-- Name: pendingworkitem pendingworkitem_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pendingworkitem
    ADD CONSTRAINT pendingworkitem_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.workitem(wiid);


--
-- Name: queue queue_statusid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_statusid_fkey FOREIGN KEY (statusid) REFERENCES public.queuestatus(qsid);


--
-- Name: queue queue_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queue
    ADD CONSTRAINT queue_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.pendingworkitem(piid);


--
-- Name: queuelock queuelock_systemid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelock
    ADD CONSTRAINT queuelock_systemid_fkey FOREIGN KEY (systemid) REFERENCES public.queueconfig(systemid);


--
-- Name: queuelog queuelog_levelid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelog
    ADD CONSTRAINT queuelog_levelid_fkey FOREIGN KEY (levelid) REFERENCES public.queueloglevel(llid);


--
-- Name: queuelog queuelog_qid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.queuelog
    ADD CONSTRAINT queuelog_qid_fkey FOREIGN KEY (qid) REFERENCES public.queue(qid);


--
-- Name: workitem workitem_statusid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workitem
    ADD CONSTRAINT workitem_statusid_fkey FOREIGN KEY (statusid) REFERENCES public.workitemstatus(wsid);


--
-- Name: workitem workitem_systemid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workitem
    ADD CONSTRAINT workitem_systemid_fkey FOREIGN KEY (systemid) REFERENCES public.queueconfig(systemid);


--
-- Name: workitem workitem_wtid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workitem
    ADD CONSTRAINT workitem_wtid_fkey FOREIGN KEY (wtid) REFERENCES public.worktype(wtid);


--
-- Name: worktype worktype_systemid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.worktype
    ADD CONSTRAINT worktype_systemid_fkey FOREIGN KEY (systemid) REFERENCES public.queueconfig(systemid);


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
