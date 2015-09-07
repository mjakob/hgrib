#!/usr/bin/env bash
# Install non-haskell dependencies on Travis CI.

set -ev

download_dir=/tmp
download_url_prefix=https://software.ecmwf.int/wiki/download/attachments/14975058
grib_api_deb=grib-api_${GRIB_API_VERSION}_amd64.deb
grib_api_devel_deb=grib-api-devel_${GRIB_API_VERSION}_amd64.deb

curl -o ${download_dir}/${grib_api_deb} \
     ${download_url_prefix}/${grib_api_deb}\?api=v2
curl -o ${download_dir}/${grib_api_devel_deb} \
     ${download_url_prefix}/${grib_api_devel_deb}\?api=v2
sudo apt-get update -qq
sudo dpkg -i \
     ${download_dir}/${grib_api_deb} \
     ${download_dir}/${grib_api_devel_deb} ||
    sudo apt-get install -f
