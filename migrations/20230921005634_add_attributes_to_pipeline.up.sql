ALTER TABLE docs_ci_pipeline_index
ADD COLUMN voodoo_branch TEXT;

ALTER TABLE docs_ci_pipeline_index
ADD COLUMN voodoo_repo TEXT;

ALTER TABLE docs_ci_pipeline_index
ADD COLUMN odoc_commit TEXT;