ALTER TABLE docs_ci_pipeline_index
DROP COLUMN voodoo_branch TEXT;

ALTER TABLE docs_ci_pipeline_index
DROP COLUMN voodoo_repo TEXT;

ALTER TABLE docs_ci_pipeline_index
DROP COLUMN odoc_commit TEXT;
