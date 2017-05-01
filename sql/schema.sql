BEGIN;

DROP TABLE IF EXISTS renewal_item;
DROP TABLE IF EXISTS renewal;
DROP TABLE IF EXISTS profile;

CREATE TABLE profile (
    id SERIAL PRIMARY KEY,
    -- the username and password are the McGill credentials
    username TEXT NOT NULL,
    password TEXT NOT NULL,
    -- the address where alerts are sent to
    -- if null, alerts should be sent to the McGill email
    email_address TEXT,
    -- for alerts
    phone_number TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT ( now() ),

    UNIQUE ( username ),
    UNIQUE ( email_address ),
    UNIQUE ( phone_number )
);

-- a renewal is a batch of renewals that happens at a given time, each of which
-- represents a particular book that is renewed
CREATE TABLE renewal (
    id SERIAL PRIMARY KEY,
    -- the academic account used to perform this renewal
    profile_id INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT ( now() ),

    CONSTRAINT fk_profile_id__profile_id
    FOREIGN KEY ( profile_id )
    REFERENCES profile ( id )
);

CREATE TABLE renewal_item (
    id SERIAL PRIMARY KEY,
    renewal_id INTEGER NOT NULL,
    description TEXT NOT NULL,
    item_status TEXT NOT NULL,
    due_date TIMESTAMP NOT NULL,
    -- represents whether the renewal for this item succeeded or not.
    -- 0: renewal succeeded
    -- 1: renewal failed
    renewal_status INTEGER NOT NULL,
    comment TEXT NOT NULL
);

COMMIT;
