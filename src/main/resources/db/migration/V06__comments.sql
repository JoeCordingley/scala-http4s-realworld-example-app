CREATE TABLE comments (
  id serial PRIMARY KEY,
  body text NOT NULL,
  article_id integer NOT NULL REFERENCES articles ON DELETE CASCADE,
  author_id integer NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE INDEX comments_idx01 ON comments (article_id);
CREATE INDEX comments_idx02 ON comments (article_id, author_id);
