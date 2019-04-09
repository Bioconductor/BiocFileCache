-- IF UPDATE SCHEME CHANGE VARIABLES IN utilities.R
-- CREATE_DB
BEGIN TRANSACTION;
CREATE TABLE metadata (
    key TEXT UNIQUE NOT NULL,
    value TEXT
);
INSERT INTO metadata (
    key, value
) VALUES (:key, :value);
CREATE TABLE resource (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rid TEXT,
    rname TEXT,
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    access_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    rpath TEXT,
    rtype TEXT,
    fpath TEXT,
    last_modified_time DATETIME DEFAULT NA,
    etag TEXT DEFAULT NA,
    expires DATETIME DEFAULT NA
);
COMMIT;
-- INSERT
BEGIN TRANSACTION;
SELECT rid FROM resource;
INSERT INTO resource (
    rname, rpath, rtype, fpath, last_modified_time, etag, expires
) VALUES (
    :rname, :rpath, :rtype, :fpath, :last_modified_time, :etag, :expires
);
UPDATE resource SET rid = "BFC" || id;
COMMIT;
-- REMOVE
DELETE FROM resource WHERE rid IN (%s);
-- UPDATE_PATH
UPDATE resource
SET rpath = :rpath, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_TIME
UPDATE resource
SET access_time = CURRENT_TIMESTAMP
WHERE rid IN (%s);
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
-- UPDATE_ETAG
UPDATE resource
SET etag  = :etag, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- UPDATE_EXPIRES
UPDATE resource
SET expires  = :expires, access_time = CURRENT_TIMESTAMP
WHERE rid = :rid;
-- MIGRATION_0_99_1_to_0_99_2
-- MIGRATION_0_99_2_to_0_99_3
ALTER TABLE resource
ADD etag TEXT;
-- MIGRATION_0_99_3_to_0_99_4
ALTER TABLE resource
ADD expires DATETIME;
-- MIGRATION_UPDATE_METADATA
UPDATE metadata
SET value = :value
WHERE key = :key
