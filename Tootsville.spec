# Tootsville servers

# Maintainer: brpocock@tootsville.org

# Copyright 2018, the Corporation for Inter-World Tourism and
# Adventuring <https://www.ciwta.org>

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
Version: 0.3.6
Release: 1
License: AGPL
BuildArchitectures: x86_64
URL: https://www.tootsville.org/development
Source0: https://goethe.tootsville.org/Software/Dist/${name}-%{version}.tar.bz2
Group: Games/Servers
BuildRoot: %{_tmppath}/%{name}-root
Prereq: info
Prereq: nroff
Requires: readline
Requires: sbcl = 1.4.6
Requires: tetex
BuildRequires: closure-compile
BuildRequires: curl
BuildRequires: emacs-nox
BuildRequires: gnupg
BuildRequires: nodejs-less
BuildRequires: nodejs-source-map
BuildRequires: readline-devel
BuildRequires: rlwrap
BuildRequires: sharutils
BuildRequires: texinfo >= 4.0
BuildRequires: texinfo-tex
BuildRequires: time
BuildRequires: uglify-js

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

%prep
%setup -c -q

FIXME

%package servers
Summary: users,gossip,world.tootsville.org servers

# 

%description servers

Server core that runs users, gossip, and world endpoints (middle tier)

%prep
%setup -c -q

%build

make -C servers

%install

rm -rf '%{buildroot}'

mkdir -p '%{buildroot}'/usr/local/bin
install -c -m 0755 Tootsville '%{buildroot}'/usr/local/bin

mkdir -p '%{buildroot}'/var/log/Tootsville

mkdir -p '%{buildroot}'/usr/lib/systemd/system/
install -c -m 0644 Tootsville.service \
  '%{buildroot}'/usr/lib/systemd/system/

/sbin/install-info '--info-dir=%{_infodir}' --delete \
  '%{_infodir}/Tootsville.info'

%post servers
useradd # FIXME
systemctl enable Tootsville
systemctl start Tootsville

%files servers
%defattr(-,root,root)
/usr/local/bin/Tootsville
/usr/lib/systemd/system/Tootsville.service

%doc %{_infodir}/Tootsville.info*

# 

%changelog

* Wed Oct 10 2018 Bruce-Robert Pocock <brpocock@tootsville.org>
- Initial, non-functional sketch
