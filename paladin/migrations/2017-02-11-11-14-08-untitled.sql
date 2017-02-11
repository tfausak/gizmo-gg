insert into arenas (name) values
  ()
on conflict do nothing;

--

create table arena_variants (
  id serial primary key,
  name character varying(100) unique not null
);

insert into arena_variants (name) values
  ();

alter table arenas
add variant_id integer references arena_variants (id);

update arenas
set variant_id = (select id from arena_variants where name = '')
where name = '';

--

create table arena_blueprints (
  id serial primary key,
  name character varying(100) unique not null
);

alter table arenas
add blueprint_id integer references arena_blueprints (id);

update arenas
set blueprint_id = (select id from arena_blueprints where name = '')
where name = '';
