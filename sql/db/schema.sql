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
-- Name: queue_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.queue_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


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
-- Name: workitem_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.workitem_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: worktype_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.worktype_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: dbmate_migrations dbmate_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dbmate_migrations
    ADD CONSTRAINT dbmate_migrations_pkey PRIMARY KEY (version);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

