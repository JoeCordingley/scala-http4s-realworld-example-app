CREATE TABLE favorites (
  article_id integer NOT NULL REFERENCES articles ON DELETE CASCADE,
  user_id integer NOT NULL REFERENCES users ON DELETE CASCADE
);

CREATE UNIQUE INDEX favorites_udx01 ON favorites (article_id, user_id);

CREATE INDEX favorites_idx01 ON favorites (article_id);
CREATE INDEX favorites_idx02 ON favorites (user_id);
