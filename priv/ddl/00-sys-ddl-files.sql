CREATE SCHEMA IF NOT EXISTS public;


-- System table indicating which DDL files have been applied and when.
--
-- filename is the sql basename (minus path and extension)
-- applied_dt is the date/time the DDL script was applied

CREATE TABLE IF NOT EXISTS sys_ddl_files (
  filename text NOT NULL UNIQUE,
  applied_dt timestamp DEFAULT current_timestamp
)
