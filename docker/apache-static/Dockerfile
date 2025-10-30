# Apache Static Server Dockerfile
FROM httpd:2.4-alpine

# Set metadata
LABEL maintainer="brpocock@tootsville.org"
LABEL description="Apache HTTP Server for serving static files for Tootsville"
LABEL version="4.7.0"

# Install required packages
RUN apk update && \
    apk add --no-cache \
        curl \
        && \
    rm -rf /var/cache/apk/*

# Create directories for web content
RUN mkdir -p /var/www/html

# Remove default Apache index.html
RUN rm -f /usr/local/apache2/htdocs/index.html

# Copy static web content
COPY dist/www/ /var/www/html/

# Configure Apache to serve from /var/www/html
RUN sed -i 's|/usr/local/apache2/htdocs|/var/www/html|g' /usr/local/apache2/conf/httpd.conf

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD curl -f http://localhost/ || exit 1

# Expose port
EXPOSE 80

# Start Apache
CMD ["httpd-foreground"]





