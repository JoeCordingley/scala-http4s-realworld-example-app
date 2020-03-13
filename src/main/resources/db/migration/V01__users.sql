CREATE TABLE users (
  id serial PRIMARY KEY,
  email text UNIQUE NOT NULL,
  username text UNIQUE NOT NULL,
  password text NOT NULL,
  bio text,
  image text,
  created_at timestamp not null,
  updated_at timestamp not null
);
