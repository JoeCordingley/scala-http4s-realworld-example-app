CREATE TABLE followers (
  followee_id integer NOT NULL REFERENCES users ON DELETE CASCADE,
  follower_id integer NOT NULL REFERENCES users ON DELETE CASCADE
);

CREATE UNIQUE INDEX followers_udx01 ON followers (followee_id, follower_id);
