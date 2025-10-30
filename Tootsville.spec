# Tootsville servers

# Maintainer: brpocock@tootsville.org

# Copyright 2018-2024, CIWTA; © 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA

# Tootsville server is free software; you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation; either version 3, or
# (at your option) any later version.

# Tootsville server is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU Affero General Public
# License along with Tootsville server; see the file COPYING.  If not,
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301, USA.

Summary: Tootsville
Name: Tootsville
Version: 0.7.0
Release: 1
License: AGPL
BuildArchitectures: x86_64
URL: https://www.tootsville.org/development
Source0: https://goethe.tootsville.org/Software/Dist/${name}-%{version}.tar.bz2
Group: Games/Servers
BuildRoot: %{_tmppath}/%{name}-root
# Runtime dependencies
Requires: readline
Requires: sbcl >= 2.3.0
Requires: systemd
Requires: openssl
Requires: sqlite3
Requires: mariadb-client
Requires: httpd
Requires: nodejs >= 18.0.0
Requires: npm >= 9.0.0

# Build dependencies
BuildRequires: readline-devel
BuildRequires: openssl-devel
BuildRequires: sqlite3-devel
BuildRequires: mariadb-devel
BuildRequires: nodejs-devel
BuildRequires: npm
BuildRequires: curl
BuildRequires: git
BuildRequires: make
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: texinfo
BuildRequires: systemd-rpm-macros

%description
Tootsville game servers installation package. See
https://www.tootsville.org/development/ for information.

%prep
%setup -c -q

%package play
Summary: play.tootsville.org web site

# 

%description play

JavaScript application and HTML, CSS, &c.  that is statically
distributed from play.tootsville.org (front end)

%package servers
Summary: users,gossip,world.tootsville.org servers

# 

%description servers

Server core that runs users, gossip, and world endpoints (middle tier)

%build
# Build the Lisp backend
cd lib/tootsville.net
make clean
make Tootsville

# Build the React frontend
cd ../../react-migration
npm install --legacy-peer-deps
npm run build

# Build the play client
cd ../play
npm install --legacy-peer-deps
make

%install
rm -rf '%{buildroot}'

# Create directory structure
mkdir -p '%{buildroot}'/usr/local/bin
mkdir -p '%{buildroot}'/usr/local/lib/tootsville
mkdir -p '%{buildroot}'/var/log/tootsville
mkdir -p '%{buildroot}'/var/lib/tootsville
mkdir -p '%{buildroot}'/etc/tootsville
mkdir -p '%{buildroot}'/usr/lib/systemd/system
mkdir -p '%{buildroot}'/usr/share/tootsville/www
mkdir -p '%{buildroot}'/usr/share/tootsville/play

# Install Lisp binary
install -c -m 0755 lib/tootsville.net/Tootsville '%{buildroot}'/usr/local/bin/

# Install frontend files
cp -r react-migration/build/* '%{buildroot}'/usr/share/tootsville/www/
cp -r play/dist/* '%{buildroot}'/usr/share/tootsville/play/

# Install configuration files
install -c -m 0644 lib/tootsville.net/config.lisp '%{buildroot}'/etc/tootsville/
install -c -m 0644 httpd.conf '%{buildroot}'/etc/tootsville/

# Install systemd service
install -c -m 0644 tootsville.service '%{buildroot}'/usr/lib/systemd/system/

%post
# Create system user
getent group tootsville >/dev/null || groupadd -r tootsville
getent passwd tootsville >/dev/null || useradd -r -g tootsville -d /var/lib/tootsville -s /sbin/nologin tootsville

# Set up directories
mkdir -p /var/lib/tootsville
chown tootsville:tootsville /var/lib/tootsville
chown tootsville:tootsville /var/log/tootsville

# Enable and start services
systemctl daemon-reload
systemctl enable tootsville
systemctl enable httpd

%preun
# Stop services before removal
systemctl stop tootsville || true
systemctl stop nginx || true

%postun
# Clean up user if package is being removed
if [ $1 -eq 0 ]; then
    userdel tootsville 2>/dev/null || true
    groupdel tootsville 2>/dev/null || true
fi

%files
%defattr(-,root,root)
%license LICENSE
%doc README.org docs/

# Binaries
/usr/local/bin/Tootsville

# Configuration
%config /etc/tootsville/
%config /etc/nginx/conf.d/tootsville.conf

# Web content
/usr/share/tootsville/www/
/usr/share/tootsville/play/

# Systemd service
/usr/lib/systemd/system/tootsville.service

# Logs and data directories
%dir /var/log/tootsville
%dir /var/lib/tootsville

# 

%changelog

* Wed Jan 22 2025 Bruce-Robert Pocock <brpocock@tootsville.org> - 0.7.0-1
- Updated to version 0.7.0
- Added comprehensive RPM packaging for Fedora 42
- Updated dependencies for modern SBCL and Node.js
- Added systemd service integration
- Added nginx configuration
- Added proper user/group management
- Added comprehensive build and install procedures

* Wed Oct 10 2018 Bruce-Robert Pocock <brpocock@tootsville.org> - 0.3.6-1
- Initial, non-functional sketch
