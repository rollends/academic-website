#!/bin/sh
#
# Deployment Script
# Requirements:
#  - Appropriate SSH Key Must be on Host.
#  - Site is built and in _site

rsync -avzc _site/* rollends.ca:/mnt/ext1/public/
