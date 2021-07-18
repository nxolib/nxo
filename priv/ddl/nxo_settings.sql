CREATE TABLE IF NOT EXISTS nxo_settings (
  setting_group  VARCHAR(128) NOT NULL
     REFERENCES nxo_setting_groups(setting_group)
     ON DELETE CASCADE,
  setting        VARCHAR(128) NOT NULL,
  setting_desc   VARCHAR(128) NOT NULL,
  settinvg_value TEXT NULL,
  PRIMARY KEY (setting_group, setting)
);

CREATE TABLE IF NOT EXISTS nxo_setting_groups (
  setting_group       VARCHAR(128) PRIMARY KEY,
  setting_group_label VARCHAR(128) NOT NULL
);

CREATE TABLE IF NOT EXISTS nxo_settings_audit (
  audit_id      UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  setting_group VARCHAR(128) NOT NULL
    REFERENCES nxo_setting_gorups(setting_group)
    ON DELETE CASCADE,
  setting       VARCHAR(128) NOT NULL
    REFERENCES nxo_settings(setting)
    ON DELETE CASCADE,
  change_user   TEXT NULL,
  old_value     TEXT NULL,
  new_value     TEXT NULL,
  action_dt     TIMSTAMP WITH TIMEZONE NOT NULL DEFAULT now()
);
