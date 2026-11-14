# Pinned upstream SQL oracle environment

The oracle uses the unmodified SQL shipped with PhenotypeLibrary 304, pinned
under both packages' `tests/fixtures/phenotype-library`. SQL is rendered and
translated by SqlRender 1.19.7 and executed against typed PostgreSQL tables;
it is not translated by handwritten substitutions. rJava is 1.0-18.

The pod initially lacked usable Java for SqlRender. This job used `apt-get
download` for OpenJDK 17 JRE/JDK headless packages and `dpkg-deb -x` into the
private `/root/rt/audit-fix1-deps/java` tree. It did not install system packages.
Absolute Java configuration symlinks into `/etc/java-17-openjdk` were redirected
within that private tree to the corresponding privately extracted `etc` files.
`R CMD javareconf -e sh` supplied compilation environment variables without
changing the system R Java configuration. rJava and SqlRender were then installed
into `/root/rt/audit-fix1-deps/R`. The three oracle-deps logs retain initial
failures and the successful installation; oracle-environment.log records exact
deb filenames and SHA-256 values.

Final test environment (in addition to RTEST_WITH pointing to the sibling):

```
R_LIBS_USER=/root/rt/audit-fix1-deps/R:/root/rt/numeric-deps:/root/rt/labels-deps
JAVA_HOME=/root/rt/audit-fix1-deps/java/usr/lib/jvm/java-17-openjdk-amd64
LD_LIBRARY_PATH=/root/rt/audit-fix1-deps/java/usr/lib/jvm/java-17-openjdk-amd64/lib/server
```

All R tests run through `/workspace/dsomop/tools/rtest.sh`, with timeout 1200
and the exact filters/files recorded in report.md. No full suite is required
for reproduction of these targeted repairs. The upstream oracle explicitly
requires PostgreSQL and SqlRender; a missing-dependency skip is not acceptance.
The job's private dependencies are deleted after preserving evidence, so a
later run must prepare this environment again. Shared libraries are retained.
