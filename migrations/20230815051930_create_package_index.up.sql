CREATE UNIQUE INDEX IF NOT EXISTS idx_packages_pipeline
    ON docs_ci_package_index (name, version, pipeline_id);