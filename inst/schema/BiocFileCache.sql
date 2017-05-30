-- IF UPDATE SCHEME CHANGE VARIBLES IN utilities.R
-- METADATA
CREATE TABLE metadata (
    key TEXT UNIQUE NOT NULL,
    value TEXT
);
-- INSERT_METADATA
INSERT INTO metadata (
    key, value
) VALUES (:key, :value);
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
) VALUES (:rname, :rpath, :rtype, :fpath);
UPDATE resource SET rid = "BFC" || id WHERE ROWID = last_insert_rowid();
SELECT rid FROM resource WHERE ROWID = last_insert_rowid();
-- REMOVE
DELETE FROM resource WHERE rid = :rid;
-- UPDATE_PATH
UPDATE resource
SET rpath = :rpath, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_TIME
UPDATE resource
SET access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_RNAME
UPDATE resource
SET rname = :rname, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_RTYPE
UPDATE resource
SET rtype = :rtype, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_MODIFIED
UPDATE resource
SET last_modified_time  = :last_modified_time, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_FPATH
UPDATE resource
SET fpath = :fpath, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- QUERY_NAMES
SELECT rid FROM resource
WHERE rname || rpath || fpath
LIKE '%' || :value || '%';
