#!/bin/bash
# Tootsville Docker Cluster Logs Script
# This script shows logs from cluster services

SERVICE=${1:-all}
FOLLOW=${2:-false}

echo "🐘 Tootsville Docker Cluster Logs"
echo "=================================="

if [ "$FOLLOW" = "true" ] || [ "$FOLLOW" = "-f" ]; then
    FOLLOW_FLAG="-f"
    echo "📋 Showing live logs (Ctrl+C to stop)..."
else
    FOLLOW_FLAG=""
    echo "📋 Showing recent logs..."
fi

case $SERVICE in
    "mariadb"|"db")
        echo "🐬 MariaDB Logs:"
        docker-compose logs $FOLLOW_FLAG mariadb
        ;;
    "server"|"tootsville"|"app")
        echo "🖥️ Tootsville Server Logs:"
        docker-compose logs $FOLLOW_FLAG tootsville-server
        ;;
    "apache"|"proxy"|"web")
        echo "🌐 Apache Proxy Logs:"
        docker-compose logs $FOLLOW_FLAG apache-proxy
        ;;
    "redis")
        echo "🔴 Redis Logs:"
        docker-compose logs $FOLLOW_FLAG redis
        ;;
    "all"|*)
        echo "📊 All Services Logs:"
        docker-compose logs $FOLLOW_FLAG
        ;;
esac
