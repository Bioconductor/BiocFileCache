-- TABLE
CREATE TABLE resource (
    rid INTEGER PRIMARY KEY AUTOINCREMENT,
    rname text,
    filepath text,
    create_date DATETIME DEFAULT CURRENT_TIMESTAMP,
    cache_file_name text
);
-- INSERT
INSERT INTO resource (
    rname, filepath, cache_file_name
) VALUES (
    '%s', '%s', '%s'
);
SELECT last_insert_rowid();
-- REMOVE
DELETE FROM resource WHERE rid IN (%s);
