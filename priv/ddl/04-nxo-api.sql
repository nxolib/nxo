CREATE TABLE IF NOT EXISTS nxo_api_keys (
  user_id UUID PRIMARY KEY REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  api_key UUID NOT NULL
);

CREATE TABLE IF NOT EXISTS nxo_api_audit (
  audit_id  UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id   UUID NOT NULL REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  action_dt TIMESTAMP NOT NULL DEFAULT now(),
  activity  VARCHAR(256) NOT NULL,
  comment   VARCHAR(256) NULL
);
