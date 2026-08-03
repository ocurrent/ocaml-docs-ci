CREATE TABLE IF NOT EXISTS docs_ci_package_index (
    name        TEXT NOT NULL,
    version     TEXT NOT NULL,
    step_list   TEXT NOT NULL,
    status      INT8 NOT NULL DEFAULT 0,
    pipeline_id INTEGER REFERENCES docs_ci_pipeline_index(id)
);

CREATE TABLE IF NOT EXISTS docs_ci_pipeline_index (
    id              INTEGER PRIMARY KEY,
    epoch_linked    TEXT,
    epoch_html      TEXT,
    voodoo_do       TEXT,
    voodoo_prep     TEXT
);