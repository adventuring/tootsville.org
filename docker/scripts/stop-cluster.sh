#!/bin/bash
# Tootsville Docker Cluster Stop Script
# This script gracefully stops the Tootsville development cluster

set -e

echo "🛑 Stopping Tootsville Docker Cluster..."
echo "========================================"

# Check if services are running
if ! docker-compose ps | grep -q "Up"; then
    echo "ℹ️ No services are currently running."
    exit 0
fi

# Stop services gracefully
echo "⏳ Stopping services..."
docker-compose down

# Clean up volumes (optional - comment out if you want to keep data)
echo "🧹 Cleaning up..."
# Uncomment the following line if you want to remove volumes on stop
# docker-compose down -v

echo ""
echo "✅ Tootsville Docker Cluster stopped successfully!"
echo "=================================================="
echo "To restart: ./docker/scripts/start-cluster.sh"
echo ""
echo "Note: Database data is preserved in Docker volumes."
echo "To completely reset: docker-compose down -v"
