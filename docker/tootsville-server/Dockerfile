# Tootsville Game Server Dockerfile
FROM fedora:41

# Set metadata
LABEL maintainer="brpocock@tootsville.org"
LABEL description="Tootsville Game Server - Common Lisp application with Vue.js frontend"
LABEL version="4.7.0"

# Install system dependencies using dnf (Fedora package manager)
RUN dnf update -y && \
    dnf install -y \
        # Common Lisp and build tools
        sbcl \
        # Database connectivity
        mariadb-connector-c \
        mariadb-connector-c-devel \
        # Caching server
        memcached \
        # Development and build tools
        git \
        make \
        gcc \
        gcc-c++ \
        libffi-devel \
        openssl-devel \
        readline-devel \
        readline \
        sqlite-devel \
        pkgconfig \
        # Node.js for Vue.js build (if needed)
        nodejs \
        npm \
        # Utilities
        curl \
        wget \
        procps-ng \
        lsof \
        strace \
        ca-certificates \
        mysql \
        # Logging
        rsyslog \
        && \
    # Create OpenSSL 1.1 compatibility symlinks
    ln -sf /usr/lib64/libcrypto.so.3 /usr/lib64/libcrypto.so.1.1 || true && \
    ln -sf /usr/lib64/libssl.so.3 /usr/lib64/libssl.so.1.1 || true && \
    # Create readline compatibility symlinks
    ln -sf /usr/lib64/libreadline.so.8 /usr/lib64/libreadline.so.6.3 || true && \
    dnf clean all

# MySQL client is already installed above with mysql package

# Create application user
RUN useradd -m -s /bin/bash pil && \
    mkdir -p /app && \
    mkdir -p /home/pil/.config/Tootsville && \
    chown pil:pil /app && \
    chown -R pil:pil /home/pil/.config

# Copy source code first (needed for Quicklisp installation)
COPY --chown=pil:pil lib/tootsville.net/ ./lib/tootsville.net/
# Copy Tootsville executable
COPY --chown=pil:pil lib/tootsville.net/Tootsville /app/Tootsville
# Copy source files for runtime
COPY --chown=pil:pil lib/tootsville.net/src /app/src/
# Create expected directory structure for migrations
RUN mkdir -p /home/brpocock/Projects/tootsville.org/lib/tootsville.net/src/db && \
    cp /app/src/db/migrations-minimal.sql /home/brpocock/Projects/tootsville.org/lib/tootsville.net/src/db/migrations.sql && \
    cp /app/src/db/friendly.lisp /home/brpocock/Projects/tootsville.org/lib/tootsville.net/src/db/friendly.lisp && \
    chown -R pil:pil /home/brpocock
# Copy config file
COPY --chown=pil:pil docker-config.lisp /home/pil/.config/Tootsville/Tootsville.config.lisp

# Switch to application user
USER pil
WORKDIR /app

# Install Quicklisp (Common Lisp package manager) as pil user
RUN curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --eval '(quit)'

# Copy additional source code
COPY --chown=pil:pil dist/play/ ./dist/play/
COPY --chown=pil:pil dist/www/ ./dist/www/

# Switch back to root for nginx setup
USER root

# Configure directories for static files
RUN mkdir -p /app/play && \
    mkdir -p /app/www

# Create startup script
RUN cat > /app/start-server.sh << 'EOF'
#!/bin/bash
set -e

echo "=== Starting Tootsville Game Server ==="
echo "Date: $(date)"

# Wait for database to be ready
echo "=== Waiting for MariaDB to be ready ==="
for i in {1..30}; do
    if mysql -h mariadb -u tootsville -ptootsville_password -e "SELECT 1" tootsville >/dev/null 2>&1; then
        echo "MariaDB is ready"
        break
    fi
    echo "Waiting for MariaDB... ($i/30)"
    sleep 2
done

if [ $i -eq 30 ]; then
    echo "ERROR: MariaDB did not become ready in time"
    exit 1
fi

# Start memcached for caching
echo "=== Starting memcached ==="
memcached -d -m 64 -p 11211 -u pil

# Set environment variables
export TOOTSVILLE_DB_HOST="${TOOTSVILLE_DB_HOST:-mariadb}"
export TOOTSVILLE_DB_PORT="${TOOTSVILLE_DB_PORT:-3306}"
export TOOTSVILLE_DB_NAME="${TOOTSVILLE_DB_NAME:-tootsville}"
export TOOTSVILLE_DB_USER="${TOOTSVILLE_DB_USER:-tootsville}"
export TOOTSVILLE_DB_PASSWORD="${TOOTSVILLE_DB_PASSWORD:-tootsville_password}"

echo "=== Starting Tootsville server ==="
echo "Database: $TOOTSVILLE_DB_HOST:$TOOTSVILLE_DB_PORT/$TOOTSVILLE_DB_NAME"
echo "Starting Tootsville with arguments: server 5000"

# Start the Tootsville server in background mode
/app/Tootsville server 5000 &
SERVER_PID=$!

# Wait for the server to start
sleep 10

# Keep the container running by waiting for the server process
wait $SERVER_PID
EOF

RUN chmod +x /app/start-server.sh

# Create custom rsyslog configuration for Tootsville
RUN cat > /etc/rsyslog.d/tootsville.conf << 'EOF'
# Tootsville server logging configuration for Docker
# Minimal configuration for containerized environment

# Log all Tootsville messages to /var/log/tootsville.log
:programname, isequal, "Tootsville" /var/log/tootsville.log
:programname, isequal, "tootsville" /var/log/tootsville.log

# Log all messages to console for Docker
*.* /dev/stdout

# Stop processing after logging
& stop
EOF

# Create log directory
RUN mkdir -p /var/log && touch /var/log/tootsville.log && chown pil:pil /var/log/tootsville.log

# Switch back to pil user
USER pil

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:5000/version/about || exit 1

# Expose ports
EXPOSE 5000 5004

# Start the application
CMD ["/app/start-server.sh"]
