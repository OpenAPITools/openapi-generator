#!/usr/bin/env python
import requests
import shutil
import gunzip

s = requests.session()
res = s.get(
    "https://api.github.com/repos/swagger-api/swagger-ui/releases"
)
j = res.json()
for release in j:
    url = release["tarball_url"]
    res = s.get(url)

    local_filename = url.split('/')[-1]
    r = s.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024): 
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
