CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS nxo_audit (
  audit_id  UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  activity  TEXT NOT NULL,
  user_id   UUID NULL,
  target    TEXT NULL,
  result    TEXT NULL,
  comment   TEXT NULL,
  action_dt TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);
