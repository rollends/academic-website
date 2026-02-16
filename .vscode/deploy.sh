#!/bin/sh
#
# Deployment Script
# Requirements:
#  - SSH config configures 'webserver' host with correct URI and
#    identity file.
#  - Site is built in publish mode and resides in _site.

rsync -avzc _site/* webserver:/var/www/
