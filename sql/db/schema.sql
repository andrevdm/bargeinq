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
-- Name: bq_fetch_queue(uuid, text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.bq_fetch_queue(_sys_id uuid, _host_name text, _host_max_items integer) RETURNS TABLE(r_qid bigint, r_wiid uuid, r_wtid uuid, r_wi_name text, r_dequeued_at timestamp with time zone, r_work_data text, r_frid integer)
    LANGUAGE plpgsql
    AS $$
DECLARE
  max_items integer := (select max_active_items from bq_system where system_id = _sys_id);
  total_active_items integer := (
    select
      count(1)
    from
      bq_queue q
    inner join bq_work_item wi
      on q.wiid = wi.wiid
    where
      wi.system_id = _sys_id
      and q.frId is null
      and (q.dequeued_at is not null and ( q.locked_until is not null
                                           or q.locked_until >= now()
                                         )
          )
  );
  host_active_items integer := (
    select
      count(1)
    from
      bq_queue q
    inner join bq_work_item wi
      on q.wiid = wi.wiid
    where
      wi.system_id = _sys_id
      and q.frId is null
      and q.host = _host_name
      and (q.dequeued_at is not null and ( q.locked_until is not null
                                           or q.locked_until >= now()
                                         )
          )
  );
BEGIN
  return query
  -- Fetch and lock a single item, if there are any available
  with
    cte_lock as (
      -- Get items that have failed first.
      -- These will not result in new work immediately, they may result in new items in the queue to to processed later
      -- Note that these items are returned even if active > max, since they wont result in more running jobs immediately
      select
        xq.qid
      from
        bq_queue xq
      inner join bq_work_item xwi
        on xwi.wiid = xq.wiid
      where exists
        (
          (select
             lq.qid, 0
           from
             bq_queue lq
           inner join bq_work_item lwi
             on lwi.wiid = lq.wiid
           where
             lq.qid = xq.qid
             and lwi.system_id = _sys_id
             and lq.frId is not null
           order by
             lwi.priority desc, lwi.created_at asc
           limit 1
          )
          union
          -- Now get items that have not failed
          (
           select
             lq.qid, 1
           from
             bq_queue lq
           inner join bq_work_item lwi
             on lwi.wiid = lq.wiid
           where
             lq.qid = xq.qid
             and lwi.system_id = _sys_id
             and lq.frId is null
             and (lq.locked_until is null or lq.locked_until < now())
             and ((max_items is null) or (total_active_items < max_items)) -- Only get if no limit, or not above max items
             and ((_host_name is null) or (_host_max_items is null) or (host_active_items < _host_max_items))
           order by
             lwi.priority desc, lwi.created_at asc
           limit 1
          )
        )
      order by
        xq.frId NULLS LAST, xwi.priority desc, xq.created_at asc -- errors before non-error
      limit 1
      for update skip locked
    ),
    cte_data as (
      select
          rq.qid
        , rq.locked_until
        , rwi.wiid
        , rwi.wtid
        , rwi.name as wi_name
        , rq.dequeued_at
        , rwi.work_data
        , rwt.dequeue_lock_period_seconds
        , rq.frId
      from
        cte_lock
      inner join bq_queue rq
        on cte_lock.qid = rq.qid
      inner join bq_work_item rwi
        on rwi.wiid = rq.wiid
      inner join bq_work_type rwt
        on rwi.wtid = rwt.wtid
    )
  update
    bq_queue q
  set
      locked_until = now() + (interval '1 second' * cte_data.dequeue_lock_period_seconds)
    , dequeued_at = COALESCE(q.dequeued_at, now())
    , host = _host_name
  from
    cte_lock
  inner join cte_data
    on cte_data.qid = cte_lock.qid
  where
	  cte_lock.qid = q.qid
  returning
    q.qid, cte_data.wiid, cte_data.wtid, cte_data.wi_name, cte_data.dequeued_at, cte_data.work_data, cte_data.frId;

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
-- Name: fn_bq_queue_all_unblocked(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_bq_queue_all_unblocked(_sys_id uuid) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
  insert into bq_queue
    (wiid, created_at)
    (select v.wiid, now() from vw_bq_unblocked_unqueued v where v.system_id = _sys_id and (v.ignore_until is null or v.ignore_until < now()))
    on conflict do nothing;
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
  FOR r1 IN select i.system_id::text as system_id from bq_work_item i where i.wiid = NEW.wiid
    LOOP
      PERFORM pg_notify('c' || REPLACE(r1, '-', ''), 'trigger');
    END loop;
  RETURN NEW;

  RETURN NEW;
END;
$$;


SET default_tablespace = '';

--
-- Name: bq_fail_reason; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_fail_reason (
    frid integer NOT NULL,
    frname text NOT NULL
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
    wiid uuid NOT NULL,
    locked_until timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    heartbeat_at timestamp with time zone,
    dequeued_at timestamp with time zone,
    frid integer,
    host text
);


--
-- Name: bq_system; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_system (
    system_id uuid NOT NULL,
    name text NOT NULL,
    poll_period_seconds integer NOT NULL,
    locked_until timestamp with time zone,
    locked_by text,
    max_active_items integer,
    auto_queue_unblocked boolean NOT NULL,
    heartbeat_check_period_seconds integer
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
    backoff_count integer NOT NULL,
    attempts integer NOT NULL,
    work_data text,
    priority integer NOT NULL,
    backoff_seconds_override integer[],
    exec_environment_override text
);


--
-- Name: bq_work_item_blockers; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bq_work_item_blockers (
    wiid_blocked uuid NOT NULL,
    wiid_blocker uuid NOT NULL,
    CONSTRAINT chk_work_item_blockers_no_self_ref CHECK ((wiid_blocked <> wiid_blocker))
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
    default_exec_environment text NOT NULL,
    dequeue_lock_period_seconds integer NOT NULL,
    heartbeat_expected_every_seconds integer,
    heartbeat_num_missed_for_error integer,
    CONSTRAINT chk_work_type_heartbeat_nulls CHECK ((((heartbeat_expected_every_seconds IS NULL) AND (heartbeat_num_missed_for_error IS NULL)) OR ((heartbeat_expected_every_seconds IS NOT NULL) AND (heartbeat_num_missed_for_error IS NOT NULL))))
);


--
-- Name: dbmate_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dbmate_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: vw_bq_unblocked_unqueued; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.vw_bq_unblocked_unqueued AS
 SELECT wi.wiid,
    wi.name,
    wi.wtid,
    wi.system_id,
    wi.ignore_until,
    wi.retries_left,
    wi.created_at,
    wi.group_id,
    wi.backoff_count,
    wi.attempts,
    wi.work_data
   FROM (public.bq_work_item wi
     LEFT JOIN public.bq_queue q ON ((q.wiid = wi.wiid)))
  WHERE ((q.qid IS NULL) AND (NOT (EXISTS ( SELECT bq_work_item_blockers.wiid_blocked
           FROM public.bq_work_item_blockers
          WHERE (bq_work_item_blockers.wiid_blocked = wi.wiid)))));


--
-- Name: bq_fail_reason bq_pk_fail_reason; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_fail_reason
    ADD CONSTRAINT bq_pk_fail_reason PRIMARY KEY (frid);


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
-- Name: bq_work_item_blockers bq_work_item_blockers_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item_blockers
    ADD CONSTRAINT bq_work_item_blockers_pkey PRIMARY KEY (wiid_blocked, wiid_blocker);


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
-- Name: ix_bq_queue_fail_readon; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_fail_readon ON public.bq_queue USING btree (frid);


--
-- Name: ix_bq_queue_heartbeat_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_heartbeat_at ON public.bq_queue USING btree (heartbeat_at);


--
-- Name: ix_bq_queue_host; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_host ON public.bq_queue USING btree (host);


--
-- Name: ix_bq_queue_locked_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_locked_until ON public.bq_queue USING btree (locked_until);


--
-- Name: ix_bq_queue_qid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_queue_qid ON public.bq_queue USING btree (qid);


--
-- Name: ix_bq_queue_wiid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX ix_bq_queue_wiid ON public.bq_queue USING btree (wiid);


--
-- Name: ix_bq_work_item_blockers_blocked; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_blockers_blocked ON public.bq_work_item_blockers USING btree (wiid_blocked);


--
-- Name: ix_bq_work_item_blockers_blocker; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_blockers_blocker ON public.bq_work_item_blockers USING btree (wiid_blocker);


--
-- Name: ix_bq_work_item_ignore_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_ignore_until ON public.bq_work_item USING btree (ignore_until);


--
-- Name: ix_bq_work_item_priority; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ix_bq_work_item_priority ON public.bq_work_item USING btree (priority);


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
-- Name: bq_queue tg_bq_queue_delete_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_bq_queue_delete_notify AFTER DELETE ON public.bq_queue FOR EACH ROW EXECUTE PROCEDURE public.fn_bq_queue_notify();


--
-- Name: bq_queue tg_bq_queue_insert_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_bq_queue_insert_notify AFTER INSERT ON public.bq_queue FOR EACH ROW EXECUTE PROCEDURE public.fn_bq_queue_notify();


--
-- Name: bq_queue tg_bq_queue_update_notify; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tg_bq_queue_update_notify AFTER UPDATE ON public.bq_queue FOR EACH ROW EXECUTE PROCEDURE public.fn_bq_queue_notify();


--
-- Name: bq_queue bq_queue_frid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_queue
    ADD CONSTRAINT bq_queue_frid_fkey FOREIGN KEY (frid) REFERENCES public.bq_fail_reason(frid);


--
-- Name: bq_queue bq_queue_wiid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_queue
    ADD CONSTRAINT bq_queue_wiid_fkey FOREIGN KEY (wiid) REFERENCES public.bq_work_item(wiid) ON DELETE CASCADE;


--
-- Name: bq_work_item_blockers bq_work_item_blockers_wiid_blocked_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item_blockers
    ADD CONSTRAINT bq_work_item_blockers_wiid_blocked_fkey FOREIGN KEY (wiid_blocked) REFERENCES public.bq_work_item(wiid) ON DELETE CASCADE;


--
-- Name: bq_work_item_blockers bq_work_item_blockers_wiid_blocker_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bq_work_item_blockers
    ADD CONSTRAINT bq_work_item_blockers_wiid_blocker_fkey FOREIGN KEY (wiid_blocker) REFERENCES public.bq_work_item(wiid) ON DELETE CASCADE;


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
    ('20211219102340'),
    ('20211221065337'),
    ('20211221072601');
