CREATE TABLE IF NOT EXISTS docs_ci_package_index (
    name        TEXT NOT NULL,
    version     TEXT NOT NULL,
    step_list   JSON,
    status      INT8 NOT NULL DEFAULT 0,
    pipeline_id INTEGER REFERENCES docs_ci_pipeline_index(id)
);

CREATE TABLE IF NOT EXISTS docs_ci_pipeline_index (
    id              INTEGER PRIMARY KEY,
    epoch_1         TEXT,
    epoch_2         TEXT,
    voodoo_do       TEXT,
    voodoo_gen      TEXT,
    voodoo_compile  TEXT
);