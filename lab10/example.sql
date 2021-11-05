-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* This is COMMENT! */
SELECT actor_id, first_name, last_name
FROM actor /* YOU CAN ADD COMMENTS USING
MULTIPLE LINES! */
ORDER by last_name, first_name 
LIMIT 5;

SELECT 1;
