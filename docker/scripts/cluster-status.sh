#!/bin/bash
# Tootsville Docker Cluster Status Script
# This script shows the current status of all cluster services

echo "🐘 Tootsville Docker Cluster Status"
echo "===================================="

# Check if Docker is running
if ! docker info >/dev/null 2>&1; then
    echo "❌ Docker is not running"
    exit 1
fi

# Show container status
echo "📊 Container Status:"
echo "-------------------"
docker-compose ps

echo ""
echo "🏥 Health Checks:"
echo "----------------"

# Check MariaDB
if docker-compose exec -T mariadb mysql -u tootsville -ptootsville_password -e "SELECT 1" tootsville >/dev/null 2>&1; then
    echo "✅ MariaDB: Healthy"
else
    echo "❌ MariaDB: Unhealthy"
fi

# Check Tootsville server
if curl -f --max-time 5 http://localhost:5000/api/health >/dev/null 2>&1; then
    echo "✅ Tootsville Server: Healthy"
else
    echo "❌ Tootsville Server: Unhealthy"
fi

# Check Apache proxy
if curl -f --max-time 5 http://localhost/health >/dev/null 2>&1; then
    echo "✅ Apache Proxy: Healthy"
else
    echo "❌ Apache Proxy: Unhealthy"
fi

echo ""
echo "🌐 Service Endpoints:"
echo "--------------------"
echo "Frontend (Play):    http://localhost/play/"
echo "Main Website:       http://localhost/"
echo "API:                http://localhost/api/"
echo "WebSocket:          ws://localhost/ws/"
echo "MariaDB:            localhost:3306"

echo ""
echo "💾 Resource Usage:"
echo "------------------"
docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}"
