#!/bin/bash
# Tootsville Docker Cluster Startup Script
# This script starts the complete Tootsville development cluster

set -e

echo "🐘 Starting Tootsville Docker Cluster..."
echo "========================================"

# Check if Docker is running
if ! docker info >/dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Check if docker-compose is available
if ! command -v docker-compose >/dev/null 2>&1; then
    echo "❌ docker-compose is not installed."
    exit 1
fi

# Create necessary directories
echo "📁 Creating directories..."
mkdir -p logs
mkdir -p data/mariadb

# Build and start the cluster
echo "🏗️ Building and starting services..."
docker-compose up --build -d

# Wait for services to be healthy
echo "⏳ Waiting for services to start..."
sleep 10

# Check MariaDB health
echo "🔍 Checking MariaDB health..."
for i in {1..30}; do
    if docker-compose exec -T mariadb mysql -u tootsville -ptootsville_password -e "SELECT 1" tootsville >/dev/null 2>&1; then
        echo "✅ MariaDB is ready"
        break
    fi
    echo "⏳ Waiting for MariaDB... ($i/30)"
    sleep 5
done

if [ $i -eq 30 ]; then
    echo "❌ MariaDB failed to start properly"
    docker-compose logs mariadb
    exit 1
fi

# Check Tootsville server health
echo "🔍 Checking Tootsville server health..."
for i in {1..30}; do
    if curl -f http://localhost:5000/api/health >/dev/null 2>&1; then
        echo "✅ Tootsville server is ready"
        break
    fi
    echo "⏳ Waiting for Tootsville server... ($i/30)"
    sleep 5
done

if [ $i -eq 30 ]; then
    echo "❌ Tootsville server failed to start properly"
    docker-compose logs tootsville-server
    exit 1
fi

# Check Apache proxy health
echo "🔍 Checking Apache proxy health..."
for i in {1..10}; do
    if curl -f http://localhost/health >/dev/null 2>&1; then
        echo "✅ Apache proxy is ready"
        break
    fi
    echo "⏳ Waiting for Apache proxy... ($i/10)"
    sleep 2
done

if [ $i -eq 10 ]; then
    echo "❌ Apache proxy failed to start properly"
    docker-compose logs apache-proxy
    exit 1
fi

echo ""
echo "🎉 Tootsville Docker Cluster is running!"
echo "=========================================="
echo "🌐 Frontend (Play):    http://localhost/play/"
echo "🌐 Main Website:       http://localhost/"
echo "🔌 API:                http://localhost/api/"
echo "🗣️ WebSocket:          ws://localhost/ws/"
echo "🐬 MariaDB:            localhost:3306 (tootsville/tootsville_password)"
echo "📊 phpMyAdmin:         http://localhost:8081 (if enabled)"
echo ""
echo "📋 Useful commands:"
echo "  docker-compose logs -f              # View all logs"
echo "  docker-compose logs tootsville-server # View server logs"
echo "  docker-compose restart tootsville-server # Restart server"
echo "  docker-compose down                 # Stop cluster"
echo "  docker-compose exec mariadb bash    # Access MariaDB container"
echo ""
echo "🔧 For development:"
echo "  docker-compose -f docker-compose.yml -f docker/docker-compose.override.yml up -d"
echo ""
echo "Happy Tooting! 🐘"
