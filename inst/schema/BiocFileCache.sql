-- IF UPDATE SCHEME CHANGE VARIBLES IN utilities.R
-- METADATA
CREATE TABLE metadata (
    key TEXT UNIQUE NOT NULL,
    value TEXT
);
-- INSERT_METADATA
INSERT INTO metadata (
    key, value
) VALUES (%s);
-- SELECT_METADATA
SELECT * FROM metadata;
-- TABLE
CREATE TABLE resource (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rid TEXT,
    rname TEXT,
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    access_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    rpath TEXT,
    rtype TEXT,
    fpath TEXT,
    last_modified_time DATETIME DEFAULT CURRENT_TIMESTAMP
);
-- INSERT
INSERT INTO resource (
    rname, rpath, rtype, fpath
) VALUES (
    '%s', '%s', '%s', '%s'
);
UPDATE resource SET rid = "BFC" || id WHERE ROWID = last_insert_rowid();
SELECT rid FROM resource WHERE ROWID = last_insert_rowid();
-- REMOVE
DELETE FROM resource WHERE rid IN (%s);
-- UPDATE_PATH
UPDATE resource
SET rpath = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- UPDATE_TIME
UPDATE resource
SET access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- UPDATE_RNAME
UPDATE resource
SET rname = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- UPDATE_RTYPE
UPDATE resource
SET rtype = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- UPDATE_MODIFIED
UPDATE resource
SET last_modified_time  = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- UPDATE_FPATH
UPDATE resource
SET fpath = '%s', access_time = CURRENT_TIMESTAMP
WHERE rid = '%s';
-- QUERY_NAMES
SELECT rid FROM resource
WHERE rname || rpath || fpath
LIKE '%%%s%%';
