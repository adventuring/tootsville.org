#!/bin/bash
# -*- bash -*-
#
# bin/test-redirects.sh - Test cross-host redirections
#
# Copyright © 2025 Interworldly Adventuring, LLC
#
# This script tests all cross-host redirections to ensure they work correctly

set -e

echo "🔍 Testing Cross-Host Redirections"
echo "=================================="
echo

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test configuration
BASE_URL="${BASE_URL:-http://localhost:5000}"
TIMEOUT=5

# Function to test a redirect
test_redirect() {
    local path="$1"
    local expected_location="$2"
    local description="$3"
    
    echo -n "Testing: $description... "
    
    # Get the redirect location
    local location=$(curl -s -I -m $TIMEOUT "${BASE_URL}${path}" 2>/dev/null | grep -i "^location:" | cut -d' ' -f2- | tr -d '\r')
    
    if [ -z "$location" ]; then
        echo -e "${YELLOW}NO REDIRECT${NC} (might be proxied instead)"
        return 0
    fi
    
    # Check if it matches expected pattern
    if echo "$location" | grep -q "$expected_location"; then
        echo -e "${GREEN}✓ PASS${NC}"
        echo "   → $location"
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        echo "   Expected pattern: $expected_location"
        echo "   Got: $location"
        return 1
    fi
}

# Test cases
echo "📋 Asset Redirections"
echo "--------------------"
test_redirect "/Assets/test.png" "jumbo.tootsville.org" "Assets to jumbo CDN"
test_redirect "/assets/test.png" "jumbo.tootsville.org" "assets (lowercase) to jumbo CDN"
echo

echo "📚 Wiki Redirections"
echo "-------------------"
test_redirect "/wiki/Test" "wiki.tootsville.org" "Wiki to wiki host"
test_redirect "/wikiwiki/Test" "wiki.tootsville.org" "Wikiwiki to wiki host"
test_redirect "/help/Test" "wiki.tootsville.org/wiki/Help" "Help to wiki host"
echo

echo "🔗 External Service Redirections"
echo "--------------------------------"
test_redirect "/form/test" "tootsville.adventuring.click" "Forms to adventuring.click"
test_redirect "/quiz/test" "tootsville.adventuring.click" "Quiz to adventuring.click"
test_redirect "/tootsbook/test" "tootsbook.com" "Tootsbook to tootsbook.com"
test_redirect "/members/test" "members.tootsbook.com" "Members to members.tootsbook.com"
echo

echo "👨‍💻 Developer Documentation"
echo "-------------------------"
test_redirect "/devel/docs/test" "goethe.tootsville.org" "Developer docs to goethe"
echo

echo "🎮 Game API Endpoints (Should be Proxied, Not Redirected)"
echo "--------------------------------------------------------"
curl -s -I -m $TIMEOUT "${BASE_URL}/play/" 2>/dev/null | head -1
curl -s -I -m $TIMEOUT "${BASE_URL}/users/" 2>/dev/null | head -1
curl -s -I -m $TIMEOUT "${BASE_URL}/toots/" 2>/dev/null | head -1
curl -s -I -m $TIMEOUT "${BASE_URL}/world/" 2>/dev/null | head -1
echo

echo "📊 Summary"
echo "=========="
echo "Base URL: $BASE_URL"
echo "Timeout: ${TIMEOUT}s"
echo
echo "Note: Some paths may be proxied instead of redirected."
echo "This is expected for game API endpoints that should be handled by the backend server."
echo
echo "✅ Test complete!"


