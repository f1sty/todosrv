--
-- PostgreSQL database dump
--

-- Dumped from database version 15.4 (Debian 15.4-1.pgdg120+1)
-- Dumped by pg_dump version 15.3

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

SET default_table_access_method = heap;

--
-- Name: todos; Type: TABLE; Schema: public; Owner: todosrv
--

CREATE TABLE public.todos (
    id integer NOT NULL,
    user_id integer,
    content text NOT NULL,
    done boolean DEFAULT false
);


ALTER TABLE public.todos OWNER TO todosrv;

--
-- Name: todos_id_seq; Type: SEQUENCE; Schema: public; Owner: todosrv
--

CREATE SEQUENCE public.todos_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.todos_id_seq OWNER TO todosrv;

--
-- Name: todos_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: todosrv
--

ALTER SEQUENCE public.todos_id_seq OWNED BY public.todos.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: todosrv
--

CREATE TABLE public.users (
    id integer NOT NULL,
    name character varying(255)
);


ALTER TABLE public.users OWNER TO todosrv;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: todosrv
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO todosrv;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: todosrv
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: todos id; Type: DEFAULT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.todos ALTER COLUMN id SET DEFAULT nextval('public.todos_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: todos; Type: TABLE DATA; Schema: public; Owner: todosrv
--

COPY public.todos (id, user_id, content, done) FROM stdin;
1	1	wash car	f
2	1	buy food	f
3	1	pet the dog	f
4	2	jogging	f
5	2	finish arduino project	f
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: todosrv
--

COPY public.users (id, name) FROM stdin;
1	user1
2	user2
3	user3
\.


--
-- Name: todos_id_seq; Type: SEQUENCE SET; Schema: public; Owner: todosrv
--

SELECT pg_catalog.setval('public.todos_id_seq', 5, true);


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: todosrv
--

SELECT pg_catalog.setval('public.users_id_seq', 3, true);


--
-- Name: todos todos_pkey; Type: CONSTRAINT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.todos
    ADD CONSTRAINT todos_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: todos todos_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.todos
    ADD CONSTRAINT todos_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- PostgreSQL database dump complete
--

