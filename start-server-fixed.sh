#!/bin/bash
set -e

echo "=== Starting Tootsville Game Server ==="
echo "Date: $(date)"
echo "Hostname: $(hostname)"

# Wait for database to be ready
echo "=== Waiting for MariaDB to be ready ==="
for i in {1..60}; do
    if mysql -h mariadb -u tootsville -ptootsville_password -e "SELECT 1" tootsville >/dev/null 2>&1; then
        echo "MariaDB is ready"
        break
    fi
    echo "Waiting for MariaDB... ($i/60)"
    sleep 5
done

if [ $i -eq 60 ]; then
    echo "ERROR: MariaDB did not become ready in time"
    exit 1
fi

# Start rsyslog for log aggregation (optional in container)
echo "=== Starting rsyslog ==="
rsyslogd -f /etc/rsyslog.conf || echo "rsyslog failed to start, continuing without it"

# Start memcached for caching
echo "=== Starting memcached ==="
memcached -d -m 64 -p 11211 -u pil

# Set environment variables
export TOOTSVILLE_DB_HOST="${TOOTSVILLE_DB_HOST:-mariadb}"
export TOOTSVILLE_DB_PORT="${TOOTSVILLE_DB_PORT:-3306}"
export TOOTSVILLE_DB_NAME="${TOOTSVILLE_DB_NAME:-tootsville}"
export TOOTSVILLE_DB_USER="${TOOTSVILLE_DB_USER:-tootsville}"
export TOOTSVILLE_DB_PASSWORD="${TOOTSVILLE_DB_PASSWORD:-tootsville_password}"
export TOOTSVILLE_WEBSOCKET_PORT="${TOOTSVILLE_WEBSOCKET_PORT:-5004}"
export TOOTSVILLE_HTTP_PORT="${TOOTSVILLE_HTTP_PORT:-5000}"

# JSCL workaround: set server mode via environment variables
export TOOTSVILLE_MODE="daemon"
export TOOTSVILLE_PORT="5000"
export TOOTSVILLE_HOST="0"

echo "=== Starting Tootsville server ==="
echo "Database: $TOOTSVILLE_DB_HOST:$TOOTSVILLE_DB_PORT/$TOOTSVILLE_DB_NAME"
echo "WebSocket: port $TOOTSVILLE_WEBSOCKET_PORT"
echo "HTTP: port $TOOTSVILLE_HTTP_PORT"
echo "Static files: served by Hunchentoot on port 5000"

echo "Starting Tootsville with command line arguments..."
exec /app/Tootsville daemon 5000
