#!/bin/bash
set -euxo pipefail

create_extension_in_separate_schema () {
  local extension_name="$1"
  psql -a -v ON_ERROR_STOP=on -1 -d template2 <<SQL
    CREATE SCHEMA $extension_name;
    GRANT USAGE ON SCHEMA $extension_name TO PUBLIC;
    GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA $extension_name TO PUBLIC;
    ALTER DEFAULT PRIVILEGES IN SCHEMA $extension_name GRANT EXECUTE ON FUNCTIONS TO PUBLIC;
    ALTER DEFAULT PRIVILEGES IN SCHEMA $extension_name GRANT USAGE ON TYPES TO PUBLIC;
    CREATE EXTENSION IF NOT EXISTS $extension_name WITH SCHEMA $extension_name;
SQL
}

psql -d postgres -c 'CREATE DATABASE template2 TEMPLATE template0'

create_extension_in_separate_schema pgcrypto
create_extension_in_separate_schema postgis

psql -a -v ON_ERROR_STOP=on -d postgres <<SQL
  ALTER DATABASE template1 IS_TEMPLATE false;
  DROP DATABASE template1;
  ALTER DATABASE template2 RENAME TO template1;
  ALTER DATABASE template1 IS_TEMPLATE true;
SQL
