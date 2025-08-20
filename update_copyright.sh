#!/bin/bash

# Script to update copyright information from CIWTA to Interworldly Adventuring, LLC
# This script updates all copyright notices to reflect the ownership transfer

echo "Updating copyright information from CIWTA to Interworldly Adventuring, LLC..."

# Update main README.org
sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' README.org

# Update Tootsville.spec
sed -i 's/# Copyright 2018, the Corporation for Inter-World Tourism and/# Copyright 2018, Interworldly Adventuring, LLC of Portland, OR, USA/g' Tootsville.spec

# Update all JavaScript files in play/ directory
find play/ -name "*.js" -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update all CSS/LESS files in play/ directory
find play/ -name "*.less" -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update all HTML files in play/ directory
find play/ -name "*.html" -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update all files in www/ directory
find www/ -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update worker files
find worker/ -name "*.js" -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update build files
find build/ -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update mesh files
find mesh/ -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (ciwta.org)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update specific CIWTA references
sed -i 's/CIWTA.org/Interworldly Adventuring, LLC/g' README.org
sed -i 's/ciwta.org/interworldly.com/g' README.org

# Update copyright year ranges to include 2024-2025
find . -name "*.js" -o -name "*.lisp" -o -name "*.org" -o -name "*.html" -o -name "*.less" | xargs sed -i 's/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025/g'
find . -name "*.js" -o -name "*.lisp" -o -name "*.org" -o -name "*.html" -o -name "*.less" | xargs sed -i 's/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021 The/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025 The/g'

echo "Copyright update completed for main repository."
