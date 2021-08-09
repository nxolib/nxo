-- Given a table, column, and value return a count of how many times
-- that value appears in the specified table.column.  Used for unique
-- value form validation.
CREATE OR REPLACE FUNCTION check_dup(
       tbl character varying,
       col character varying,
       val character varying)
RETURNS integer
AS $$
  DECLARE
    c int;
  BEGIN
    EXECUTE format('select count(*) from %I where %I::text = $1', tbl, col)
      INTO STRICT c
      USING val;
    RETURN c;
  END;
$$ LANGUAGE plpgsql;

-- As above, but don't count rows with a corresponding id_col=id_val
CREATE OR REPLACE FUNCTION check_dup(
       tbl character varying,
       col character varying,
       val character varying,
       id_col character varying,
       id_val character varying)
RETURNS integer
AS $$
  DECLARE
    c int;
  BEGIN
    EXECUTE format('select count(*)
                      from %I
                     where %I::text = $1 and %I::text != $2',
            tbl, col, id_col)
      INTO STRICT c
      USING val, id_val;
    RETURN c;
  END;
$$ LANGUAGE plpgsql;
