CREATE TABLE articles (
  id serial PRIMARY KEY,
  slug text UNIQUE NOT NULL,
  title text NOT NULL,
  description text NOT NULL,
  body text NOT NULL,
  author_id integer NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE INDEX articles_idx01 ON articles (author_id);
