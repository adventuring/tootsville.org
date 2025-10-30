# Apache Load Balancer Dockerfile
FROM httpd:2.4-alpine

# Set metadata
LABEL maintainer="brpocock@tootsville.org"
LABEL description="Apache HTTP Server as load balancer and reverse proxy for Tootsville"
LABEL version="4.7.0"

# Install required packages
RUN apk update && \
    apk add --no-cache \
        curl \
        openssl \
        && \
    rm -rf /var/cache/apk/*

# Enable required Apache modules
RUN sed -i \
    -e 's/^#LoadModule proxy_module/LoadModule proxy_module/' \
    -e 's/^#LoadModule proxy_http_module/LoadModule proxy_http_module/' \
    -e 's/^#LoadModule proxy_wstunnel_module/LoadModule proxy_wstunnel_module/' \
    -e 's/^#LoadModule proxy_balancer_module/LoadModule proxy_balancer_module/' \
    -e 's/^#LoadModule lbmethod_byrequests_module/LoadModule lbmethod_byrequests_module/' \
    -e 's/^#LoadModule slotmem_shm_module/LoadModule slotmem_shm_module/' \
    -e 's/^#LoadModule rewrite_module/LoadModule rewrite_module/' \
    -e 's/^#LoadModule headers_module/LoadModule headers_module/' \
    -e 's/^#LoadModule ssl_module/LoadModule ssl_module/' \
    -e 's/^#LoadModule include_module/LoadModule include_module/' \
    /usr/local/apache2/conf/httpd.conf

# Create directories for web content
RUN mkdir -p /var/www/tootsville/play && \
    mkdir -p /var/www/tootsville/www && \
    mkdir -p /usr/local/apache2/logs

# Remove default Apache index.html to prevent conflicts
RUN rm -f /usr/local/apache2/htdocs/index.html

# Copy static web content
COPY dist/play/ /var/www/tootsville/play/
COPY dist/www/ /var/www/tootsville/www/

# Copy the correct Tootsville error pages
COPY dist/www/error/ /usr/local/apache2/htdocs/error/

# Create self-signed SSL certificate for HTTPS testing
RUN openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
    -keyout /usr/local/apache2/conf/server.key \
    -out /usr/local/apache2/conf/server.crt \
    -subj "/C=US/ST=State/L=City/O=Organization/CN=tootsville.local"

# Copy Apache configuration
COPY docker/apache-proxy/conf/ /usr/local/apache2/conf/

# Add Listen directive for port 5000
RUN echo "Listen 5000" >> /usr/local/apache2/conf/httpd.conf

# Include the virtual host configuration
RUN echo "Include conf/000-default.conf" >> /usr/local/apache2/conf/httpd.conf

# Create health check script
RUN cat > /usr/local/apache2/cgi-bin/health << 'EOF'
#!/bin/sh
echo "Content-Type: text/plain"
echo ""
echo "healthy"
EOF

RUN chmod +x /usr/local/apache2/cgi-bin/health

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD curl -f http://localhost/health || exit 1

# Expose ports
EXPOSE 80 443

# Start Apache
CMD ["httpd-foreground"]
