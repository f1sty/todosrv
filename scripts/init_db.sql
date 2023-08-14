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

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: todos; Type: TABLE; Schema: public; Owner: todosrv
--

CREATE TABLE public.todos (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    user_id uuid NOT NULL,
    text text NOT NULL,
    done boolean DEFAULT false NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.todos OWNER TO todosrv;

--
-- Name: users; Type: TABLE; Schema: public; Owner: todosrv
--

CREATE TABLE public.users (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    display_name character varying(30) NOT NULL,
    encrypted_password text NOT NULL
);


ALTER TABLE public.users OWNER TO todosrv;

--
-- Data for Name: todos; Type: TABLE DATA; Schema: public; Owner: todosrv
--

COPY public.todos (id, user_id, text, done, created_at) FROM stdin;
a1383dab-ca55-4a07-abca-f4b0a3399c9c	c100d829-e427-4bd7-b2ce-35badcf12dcd	wash the car	f	2023-08-13 21:02:36.724803
26afd816-89d6-4397-90ee-6926747122e2	c100d829-e427-4bd7-b2ce-35badcf12dcd	ban annoying redditors	f	2023-08-13 21:02:36.724803
0890b745-f90a-4c35-9e60-b5838f7280f2	c100d829-e427-4bd7-b2ce-35badcf12dcd	eat some ramen	f	2023-08-13 21:02:36.724803
2d34f1cf-f60c-4098-be18-82dcf3c66ca1	3d548b01-c9f3-4843-a2c8-4a7e8139d85b	pet the cat	f	2023-08-13 21:02:36.724803
66658b91-638d-43d9-b42f-5c2e578fee42	3d548b01-c9f3-4843-a2c8-4a7e8139d85b	finish arduino project	f	2023-08-13 21:02:36.724803
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: todosrv
--

COPY public.users (id, display_name, encrypted_password) FROM stdin;
c100d829-e427-4bd7-b2ce-35badcf12dcd	root	$2a$06$/BALvxEesKPiPW8NXQX52.CpnFAuY5BkepLT4Y7WwebejgNQZdLKC
3d548b01-c9f3-4843-a2c8-4a7e8139d85b	f1sty	$2a$06$EbrCzV0G3PklEvRTJ3WwtucQmoaCUWDnifkipUWsSYv43ZSQ5IMDK
\.


--
-- Name: todos todos_pkey; Type: CONSTRAINT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.todos
    ADD CONSTRAINT todos_pkey PRIMARY KEY (id);


--
-- Name: users users_display_name_key; Type: CONSTRAINT; Schema: public; Owner: todosrv
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_display_name_key UNIQUE (display_name);


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

