# Tootsville React Migration Makefile
# Copyright © 2025 Interworldly Adventuring, LLC.
# This program is Free Software; Refer to COPYING.AGPL for details.
#
# Build system for Tootsville client with mobile platform support
# Uses ASDF for Lisp code and appropriate tools for other environments

# Configuration
PROJECT_NAME := tootsville-client
VERSION := 1.0.0
BUILD_DIR := build
DIST_DIR := dist
DOC_DIR := docs

# Platform detection
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# ASDF configuration
ASDF := asdf
LISP := sbcl
LISP_FLAGS := --noinform --no-sysinit --no-userinit --non-interactive

# Node.js configuration
NODE := node
NPM := npm
NODE_ENV := production

# Mobile platforms
MOBILE_PLATFORMS := android ios firetv samsungtv lgwebos roku

# Default target
.PHONY: all
all: build

# Help target
.PHONY: help
help:
	@echo "Tootsville Client Build System"
	@echo "=============================="
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Build everything (default)"
	@echo "  build            - Build React client"
	@echo "  build-mobile     - Build for all mobile platforms"
	@echo "  build-android    - Build Android app"
	@echo "  build-ios        - Build iOS app"
	@echo "  build-firetv     - Build FireTV app"
	@echo "  build-tv         - Build for all TV platforms"
	@echo "  test             - Run all tests"
	@echo "  test-unit        - Run unit tests"
	@echo "  test-integration - Run integration tests"
	@echo "  test-mobile      - Run mobile-specific tests"
	@echo "  lint             - Run linting"
	@echo "  format           - Format code"
	@echo "  clean            - Clean build artifacts"
	@echo "  clean-all        - Clean everything including node_modules"
	@echo "  install          - Install dependencies"
	@echo "  install-lisp     - Install Lisp dependencies via ASDF"
	@echo "  doc              - Generate documentation"
	@echo "  doc-texinfo      - Generate TeXinfo documentation"
	@echo "  doc-html         - Generate HTML documentation"
	@echo "  doc-pdf          - Generate PDF documentation"
	@echo "  deploy           - Deploy to production"
	@echo "  deploy-mobile    - Deploy mobile apps"
	@echo "  package          - Create distribution package"
	@echo "  package-mobile   - Create mobile distribution packages"
	@echo "  check            - Run all checks (lint, test, build)"
	@echo "  check-mobile     - Run mobile-specific checks"
	@echo "  dev              - Start development server"
	@echo "  dev-mobile       - Start mobile development server"
	@echo "  analyze          - Analyze bundle size"
	@echo "  optimize         - Optimize for production"
	@echo "  optimize-mobile  - Optimize for mobile platforms"
	@echo "  security         - Run security audit"
	@echo "  security-mobile  - Run mobile security audit"
	@echo "  update           - Update dependencies"
	@echo "  update-lisp      - Update Lisp dependencies"
	@echo "  version          - Show version information"
	@echo "  platforms        - Show supported platforms"
	@echo "  requirements     - Show system requirements"
	@echo ""

# Version information
.PHONY: version
version:
	@echo "Tootsville Client v$(VERSION)"
	@echo "Copyright © 2025 Interworldly Adventuring, LLC."
	@echo "This program is Free Software; Refer to COPYING.AGPL for details."
	@echo ""
	@echo "Build System:"
	@echo "  Node.js: $(shell $(NODE) --version)"
	@echo "  npm: $(shell $(NPM) --version)"
	@echo "  ASDF: $(shell $(ASDF) --version 2>/dev/null || echo 'Not installed')"
	@echo "  Lisp: $(shell $(LISP) --version 2>/dev/null | head -1 || echo 'Not installed')"
	@echo "  Platform: $(UNAME_S) $(UNAME_M)"

# Show supported platforms
.PHONY: platforms
platforms:
	@echo "Supported Platforms:"
	@echo "  Web: Chrome, Firefox, Safari, Edge"
	@echo "  Mobile: Android, iOS"
	@echo "  TV: FireTV, Samsung Smart TV, LG WebOS TV, Roku TV"
	@echo "  Desktop: Windows, macOS, Linux"

# Show system requirements
.PHONY: requirements
requirements:
	@echo "System Requirements:"
	@echo "  Node.js >= 16.0.0"
	@echo "  npm >= 8.0.0"
	@echo "  ASDF (for Lisp development)"
	@echo "  SBCL >= 2.0.0 (for Lisp development)"
	@echo "  Make >= 3.81"
	@echo "  Git >= 2.0.0"

# Install dependencies
.PHONY: install
install: install-node install-lisp

.PHONY: install-node
install-node:
	@echo "Installing Node.js dependencies..."
	$(NPM) install

.PHONY: install-lisp
install-lisp:
	@echo "Installing Lisp dependencies via ASDF..."
	@if command -v $(ASDF) >/dev/null 2>&1; then \
		$(ASDF) install $(LISP) latest; \
		$(ASDF) local $(LISP) latest; \
		echo "Lisp dependencies installed via ASDF"; \
	else \
		echo "ASDF not found. Please install ASDF first."; \
		echo "Visit: https://asdf-vm.com/"; \
		exit 1; \
	fi

# Build targets
.PHONY: build
build: install
	@echo "Building React client..."
	$(NPM) run build

.PHONY: build-mobile
build-mobile: $(addprefix build-,$(MOBILE_PLATFORMS))

.PHONY: build-android
build-android:
	@echo "Building Android app..."
	$(NODE) scripts/build-mobile.js android pwa
	$(NODE) scripts/build-mobile.js android webview
	$(NODE) scripts/build-mobile.js android capacitor

.PHONY: build-ios
build-ios:
	@echo "Building iOS app..."
	$(NODE) scripts/build-mobile.js ios pwa
	$(NODE) scripts/build-mobile.js ios webview
	$(NODE) scripts/build-mobile.js ios capacitor

.PHONY: build-firetv
build-firetv:
	@echo "Building FireTV app..."
	$(NODE) scripts/build-mobile.js firetv pwa
	$(NODE) scripts/build-mobile.js firetv webview

.PHONY: build-samsungtv
build-samsungtv:
	@echo "Building Samsung Smart TV app..."
	$(NODE) scripts/build-mobile.js samsungtv pwa
	$(NODE) scripts/build-mobile.js samsungtv webview

.PHONY: build-lgwebos
build-lgwebos:
	@echo "Building LG WebOS TV app..."
	$(NODE) scripts/build-mobile.js lgwebos pwa
	$(NODE) scripts/build-mobile.js lgwebos webview

.PHONY: build-roku
build-roku:
	@echo "Building Roku TV app..."
	$(NODE) scripts/build-mobile.js roku pwa
	$(NODE) scripts/build-mobile.js roku webview

.PHONY: build-tv
build-tv: build-firetv build-samsungtv build-lgwebos build-roku

# Test targets
.PHONY: test
test: test-unit test-integration test-mobile

.PHONY: test-unit
test-unit:
	@echo "Running unit tests..."
	$(NPM) run test:unit

.PHONY: test-integration
test-integration:
	@echo "Running integration tests..."
	$(NPM) run test:integration

.PHONY: test-mobile
test-mobile:
	@echo "Running mobile-specific tests..."
	$(NPM) run test:mobile

.PHONY: test-lisp
test-lisp:
	@echo "Running Lisp tests..."
	@if command -v $(ASDF) >/dev/null 2>&1; then \
		$(LISP) $(LISP_FLAGS) --eval "(asdf:test-system :tootsville-client)"; \
	else \
		echo "ASDF not available for Lisp testing"; \
	fi

# Linting and formatting
.PHONY: lint
lint:
	@echo "Running linting..."
	$(NPM) run lint

.PHONY: lint-fix
lint-fix:
	@echo "Fixing linting issues..."
	$(NPM) run lint:fix

.PHONY: format
format:
	@echo "Formatting code..."
	$(NPM) run format

# Documentation targets
.PHONY: doc
doc: doc-texinfo doc-html doc-pdf

.PHONY: doc-texinfo
doc-texinfo:
	@echo "Generating TeXinfo documentation..."
	@mkdir -p $(DOC_DIR)
	$(NPM) run doc:texinfo

.PHONY: doc-html
doc-html:
	@echo "Generating HTML documentation..."
	@mkdir -p $(DOC_DIR)
	$(NPM) run doc:html

.PHONY: doc-pdf
doc-pdf:
	@echo "Generating PDF documentation..."
	@mkdir -p $(DOC_DIR)
	$(NPM) run doc:pdf

.PHONY: doc-mobile
doc-mobile:
	@echo "Generating mobile documentation..."
	@mkdir -p $(DOC_DIR)/mobile
	$(NPM) run doc:mobile

# Development targets
.PHONY: dev
dev:
	@echo "Starting development server..."
	$(NPM) run dev

.PHONY: dev-mobile
dev-mobile:
	@echo "Starting mobile development server..."
	$(NPM) run dev:mobile

# Analysis and optimization
.PHONY: analyze
analyze:
	@echo "Analyzing bundle size..."
	$(NPM) run analyze

.PHONY: optimize
optimize:
	@echo "Optimizing for production..."
	$(NPM) run optimize

.PHONY: optimize-mobile
optimize-mobile:
	@echo "Optimizing for mobile platforms..."
	$(NPM) run optimize:mobile

# Security targets
.PHONY: security
security:
	@echo "Running security audit..."
	$(NPM) audit

.PHONY: security-fix
security-fix:
	@echo "Fixing security vulnerabilities..."
	$(NPM) audit fix

.PHONY: security-mobile
security-mobile:
	@echo "Running mobile security audit..."
	$(NPM) run security:mobile

# Update targets
.PHONY: update
update: update-node update-lisp

.PHONY: update-node
update-node:
	@echo "Updating Node.js dependencies..."
	$(NPM) update

.PHONY: update-lisp
update-lisp:
	@echo "Updating Lisp dependencies..."
	@if command -v $(ASDF) >/dev/null 2>&1; then \
		$(ASDF) update; \
		$(ASDF) plugin update --all; \
	else \
		echo "ASDF not available for Lisp updates"; \
	fi

# Check targets
.PHONY: check
check: lint test build

.PHONY: check-mobile
check-mobile: lint test-mobile build-mobile

# Package targets
.PHONY: package
package: build
	@echo "Creating distribution package..."
	@mkdir -p $(DIST_DIR)
	tar -czf $(DIST_DIR)/$(PROJECT_NAME)-$(VERSION).tar.gz -C $(BUILD_DIR) .

.PHONY: package-mobile
package-mobile: build-mobile
	@echo "Creating mobile distribution packages..."
	@mkdir -p $(DIST_DIR)/mobile
	@for platform in $(MOBILE_PLATFORMS); do \
		if [ -d "build/$$platform" ]; then \
			tar -czf $(DIST_DIR)/mobile/$(PROJECT_NAME)-$$platform-$(VERSION).tar.gz -C build/$$platform .; \
		fi \
	done

# Deploy targets
.PHONY: deploy
deploy: check
	@echo "Deploying to production..."
	$(NPM) run deploy

.PHONY: deploy-mobile
deploy-mobile: check-mobile
	@echo "Deploying mobile apps..."
	$(NPM) run deploy:mobile

# Clean targets
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	rm -rf $(DIST_DIR)
	rm -rf $(DOC_DIR)
	rm -rf coverage
	rm -rf .nyc_output

.PHONY: clean-all
clean-all: clean
	@echo "Cleaning everything..."
	rm -rf node_modules
	rm -rf package-lock.json
	rm -rf .cache
	rm -rf .parcel-cache

# Platform-specific targets
.PHONY: android
android: build-android

.PHONY: ios
ios: build-ios

.PHONY: firetv
firetv: build-firetv

.PHONY: tv
tv: build-tv

# Quick development targets
.PHONY: quick
quick: install build

.PHONY: quick-mobile
quick-mobile: install build-mobile

.PHONY: quick-test
quick-test: install test

# Maintenance targets
.PHONY: maintenance
maintenance: clean install lint test build

.PHONY: maintenance-mobile
maintenance-mobile: clean install lint test-mobile build-mobile

# Release targets
.PHONY: release
release: clean-all install check package

.PHONY: release-mobile
release-mobile: clean-all install check-mobile package-mobile

# Debug targets
.PHONY: debug
debug:
	@echo "Debug information:"
	@echo "  Project: $(PROJECT_NAME)"
	@echo "  Version: $(VERSION)"
	@echo "  Build dir: $(BUILD_DIR)"
	@echo "  Dist dir: $(DIST_DIR)"
	@echo "  Doc dir: $(DOC_DIR)"
	@echo "  Platform: $(UNAME_S) $(UNAME_M)"
	@echo "  Node: $(shell $(NODE) --version)"
	@echo "  npm: $(shell $(NPM) --version)"
	@echo "  ASDF: $(shell command -v $(ASDF) >/dev/null 2>&1 && echo "Available" || echo "Not available")"
	@echo "  Lisp: $(shell command -v $(LISP) >/dev/null 2>&1 && echo "Available" || echo "Not available")"

# Default target
.DEFAULT_GOAL := all
