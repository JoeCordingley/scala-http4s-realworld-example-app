CREATE TABLE tags (
  article_id integer NOT NULL REFERENCES articles ON DELETE CASCADE,
  tag text NOT NULL
);

CREATE UNIQUE INDEX tags_udx01 ON tags (article_id, tag);

CREATE INDEX tags_idx01 ON tags (article_id);
CREATE INDEX tags_idx02 ON tags (tag);
