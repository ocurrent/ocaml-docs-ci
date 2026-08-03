# Helper dockerfiles

## caddy

Front-end web server for the day11-based pipeline. Serves rendered
HTML directly from the day11 layer cache and reverse-proxies the
dashboard, snapshot, and job-log routes to the daemon's web UI on
the host. See `../docker-compose.yml`.
