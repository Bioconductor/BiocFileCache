-- TABLE
CREATE TABLE resource (
    rid INTEGER PRIMARY KEY AUTOINCREMENT,
    rname text,
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    access_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    cache_file_name text,
    cache_file_path text
);
-- INSERT
INSERT INTO resource (
    rname, cache_file_name, cache_file_path
) VALUES (
    '%s', '%s', '%s'
);
SELECT rid FROM resource WHERE ROWID = last_insert_rowid();
-- REMOVE
DELETE FROM resource WHERE rid IN (%s);
-- UPDATE_PATH
UPDATE resource 
SET cache_file_path = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%d';
-- UPDATE_TIME
UPDATE resource 
SET access_time = CURRENT_TIMESTAMP
WHERE rid = '%d';
