# 🐘 Tootsville Docker Cluster

A complete Docker-based development and testing environment for the Tootsville game, featuring Apache load balancing, MariaDB database, and the Common Lisp game server.

## 🚀 Quick Start

### Prerequisites
- Docker (20.10+)
- Docker Compose (2.0+)
- At least 4GB RAM available
- Ports 80, 443, 3306, 5000, 5004, 8080, 9090 available

### Start the Cluster
```bash
# From the project root
./docker/scripts/start-cluster.sh
```

This will:
1. Build all Docker images
2. Start MariaDB, Tootsville server, and Apache proxy
3. Wait for services to be healthy
4. Display access URLs

### Stop the Cluster
```bash
./docker/scripts/stop-cluster.sh
```

## 🏗️ Architecture

```
Internet
    │
    ▼
┌─────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Apache     │    │ Tootsville      │    │   MariaDB       │
│  Proxy      │◄──►│ Game Server     │◄──►│   Database      │
│  (Load      │    │ (Common Lisp)   │    │                 │
│   Balancer) │    │                 │    │                 │
└─────────────┘    └─────────────────┘    └─────────────────┘
       │                       │
       └───────────────────────┼────────────────────────────┐
                               ▼                            │
                       ┌─────────────────┐                  │
                       │ Static Files    │◄─────────────────┘
                       │ (nginx)         │
                       └─────────────────┘
```

## 🌐 Service Endpoints

| Service | URL | Purpose |
|---------|-----|---------|
| **Frontend (Play)** | http://localhost/play/ | Vue.js game client |
| **Main Website** | http://localhost/ | Tootsville website |
| **API** | http://localhost/api/ | REST API endpoints |
| **WebSocket** | ws://localhost/ws/ | Real-time communication |
| **Database** | localhost:3306 | MariaDB (external access) |
| **Health Check** | http://localhost/health | Service health status |

## 📊 Database Access

### From Host Machine
```bash
mysql -h localhost -P 3306 -u tootsville -ptootsville_password tootsville
```

### From Docker Container
```bash
docker-compose exec mariadb mysql -u tootsville -ptootsville_password tootsville
```

### Default Test Data
The cluster includes pre-populated test data:
- **Users**: `admin`, `alice`, `bob`, `charlie`, `diana`
- **Characters**: Various Toots with different abilities
- **Items**: Grow potions, shadow casters, magic wands, etc.
- **Chat messages**: Sample conversations
- **Achievements**: Various game achievements

## 🛠️ Development Mode

For development with hot reloading and different ports:

```bash
# Start with development override
docker-compose -f docker-compose.yml -f docker/docker-compose.override.yml up -d

# Access URLs (different ports to avoid conflicts)
# Frontend: http://localhost:8080/play/
# API: http://localhost:5001/api/
# Database: localhost:3307
```

## 📋 Management Scripts

| Script | Purpose |
|--------|---------|
| `start-cluster.sh` | Start the entire cluster |
| `stop-cluster.sh` | Stop all services gracefully |
| `cluster-status.sh` | Show service status and health |
| `view-logs.sh [service]` | View logs from services |

### Examples
```bash
# View all logs
./docker/scripts/view-logs.sh

# View live server logs
./docker/scripts/view-logs.sh server -f

# View database logs
./docker/scripts/view-logs.sh mariadb

# Check cluster status
./docker/scripts/cluster-status.sh
```

## 🔧 Configuration

### Environment Variables

#### MariaDB
```yaml
MYSQL_ROOT_PASSWORD: tootsville_root_password
MYSQL_DATABASE: tootsville
MYSQL_USER: tootsville
MYSQL_PASSWORD: tootsville_password
```

#### Tootsville Server
```yaml
TOOTSVILLE_DB_HOST: mariadb
TOOTSVILLE_DB_PORT: 3306
TOOTSVILLE_DB_NAME: tootsville
TOOTSVILLE_DB_USER: tootsville
TOOTSVILLE_DB_PASSWORD: tootsville_password
TOOTSVILLE_WEBSOCKET_PORT: 5004
TOOTSVILLE_HTTP_PORT: 5000
```

### Database Schema

The cluster automatically creates:
- **Users & Characters**: Player accounts and avatars
- **Items & Inventory**: Game items and player inventories
- **Chat System**: Messages, emotes, and whispers
- **World Events**: Game events and interactions
- **Achievements**: Player accomplishments
- **Statistics**: Game metrics and analytics

## 🐛 Troubleshooting

### Common Issues

#### Port Conflicts
If ports are already in use:
```bash
# Check what's using the ports
sudo lsof -i :80
sudo lsof -i :3306

# Use development mode (different ports)
docker-compose -f docker-compose.yml -f docker/docker-compose.override.yml up -d
```

#### Database Connection Issues
```bash
# Check MariaDB logs
./docker/scripts/view-logs.sh mariadb

# Restart MariaDB
docker-compose restart mariadb

# Reset database
docker-compose down -v
docker-compose up -d mariadb
```

#### Build Failures
```bash
# Clean and rebuild
docker-compose down
docker system prune -f
./docker/scripts/start-cluster.sh
```

### Health Checks

The cluster includes health checks for all services:
- **MariaDB**: Database connectivity test
- **Tootsville Server**: HTTP API health endpoint
- **Apache Proxy**: Load balancer health check

## 📁 Directory Structure

```
docker/
├── apache-proxy/           # Apache load balancer
│   ├── Dockerfile
│   ├── conf/
│   │   └── 000-default.conf
│   └── ssl/                # SSL certificates
├── mariadb/               # MariaDB database
│   ├── Dockerfile         # (uses official MariaDB)
│   ├── init/             # Database initialization
│   │   └── 01-init-tootsville-db.sql
│   └── conf.d/           # MariaDB configuration
│       └── tootsville.cnf
├── tootsville-server/    # Game server
│   └── Dockerfile
├── scripts/              # Management scripts
│   ├── start-cluster.sh
│   ├── stop-cluster.sh
│   ├── cluster-status.sh
│   └── view-logs.sh
├── docker-compose.override.yml  # Development config
└── README.md           # This file
```

## 🔒 Security Notes

### Development Environment
- Default passwords are used (change for production)
- SSL certificates are self-signed
- CORS is configured for local development

### Production Considerations
- Use strong passwords
- Configure proper SSL certificates
- Set up firewall rules
- Enable database backups
- Configure log rotation
- Set up monitoring and alerting

## 🚀 Deployment Options

### Local Development
```bash
./docker/scripts/start-cluster.sh
```

### Production Deployment
1. Update passwords in `docker-compose.yml`
2. Configure SSL certificates
3. Set up reverse proxy (nginx/Caddy)
4. Configure backups
5. Set up monitoring

### Kubernetes Deployment
The Docker setup can be adapted for Kubernetes:
- Convert docker-compose to Kubernetes manifests
- Use ConfigMaps for configuration
- Use Secrets for passwords
- Set up Ingress for load balancing

## 📞 Support

For issues with the Docker cluster:
1. Check the logs: `./docker/scripts/view-logs.sh`
2. Check service status: `./docker/scripts/cluster-status.sh`
3. Restart services: `docker-compose restart`
4. Reset cluster: `docker-compose down -v && ./docker/scripts/start-cluster.sh`

## 🏷️ Version History

- **v4.7.0**: Initial Docker cluster setup
  - Apache load balancer with SSL
  - MariaDB with Tootsville schema
  - Tootsville server with health checks
  - Complete management scripts
  - Development and production configs

---

**Happy Tooting!** 🐘

*Built with ❤️ for the Tootsville community*
