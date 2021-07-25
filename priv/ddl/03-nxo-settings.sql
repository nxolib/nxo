CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS nxo_setting_groups (
  setting_group       VARCHAR(128) PRIMARY KEY,
  setting_group_label VARCHAR(128) NOT NULL
);

CREATE TABLE IF NOT EXISTS nxo_settings (
  setting_group  VARCHAR(128) NOT NULL
     REFERENCES nxo_setting_groups(setting_group)
     ON DELETE CASCADE,
  setting        VARCHAR(128) NOT NULL,
  setting_desc   VARCHAR(128) NOT NULL,
  setting_value  TEXT NULL,
  PRIMARY KEY (setting_group, setting)
);
