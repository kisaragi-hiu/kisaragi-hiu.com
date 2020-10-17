#!/bin/bash
set +x

netlify_access_token="$(cat ~/.netlify-access-token)"
curl -H "Content-Type: application/zip" \
	-H "Authorization: Bearer $netlify_access_token" \
	--data-binary "@public.zip" \
	https://api.netlify.com/api/v1/sites/a9979c17-2cfe-4bc9-9dd9-ee9e6bf1b161/deploys
