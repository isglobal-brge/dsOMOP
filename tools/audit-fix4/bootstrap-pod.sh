set -eu
mkdir -p /root/rt/audit-fix4-fixtures /root/rt/audit-fix4-deps
python3 - <<'PY'
import urllib.request,pathlib,hashlib,zipfile,json
root=pathlib.Path('/root/rt/audit-fix4-fixtures')
items={
 'GiBleed_5.3.zip':'https://raw.githubusercontent.com/OHDSI/EunomiaDatasets/3d349dcdd79cc346b8e36eb837e49feffe7f7497/datasets/GiBleed/GiBleed_5.3.zip',
 'Eunomia.tar.gz':'https://api.github.com/repos/OHDSI/Eunomia/tarball/cccf503f2f52b9e02459be03bcd5f8c44031f576',
 'CommonDataModel.tar.gz':'https://api.github.com/repos/OHDSI/CommonDataModel/tarball/b90ae94430e9a266891890a549ac188137fd7f00'}
manifest={}
for name,url in items.items():
 b=urllib.request.urlopen(url).read();(root/name).write_bytes(b)
 manifest[name]={'url':url,'sha256':hashlib.sha256(b).hexdigest(),'bytes':len(b)}
print(json.dumps(manifest,indent=2))
(root/'downloads.json').write_text(json.dumps(manifest,indent=2)+'\n')
print(zipfile.ZipFile(root/'GiBleed_5.3.zip').namelist())
PY
R_LIBS_USER=/root/rt/audit-fix4-deps:/root/rt/numeric-deps:/root/rt/labels-deps R CMD INSTALL -l /root/rt/audit-fix4-deps /root/rt/audit-fix4-fixtures/CommonDataModel.tar.gz
R_LIBS_USER=/root/rt/audit-fix4-deps:/root/rt/numeric-deps:/root/rt/labels-deps R CMD INSTALL -l /root/rt/audit-fix4-deps /root/rt/audit-fix4-fixtures/Eunomia.tar.gz
