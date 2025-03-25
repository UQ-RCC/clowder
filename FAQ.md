## Frequently asked questions for Clowder development

### How to bump Clowder version?

Update `version` value in `project/Build.scala`

```
val version = "1.20.6"
```

### How to build Clowder docker image?

```
docker build . -t "uqrcc/clowder:pitschi1.20.6"
```

### How to build Clowder without building docker image?

```
cd work/pitschi/github/clowder
./sbt dist
```

### How to debug Scala code snippets without building and deploying docker image?

```
cd work/pitschi/github/clowder
sbt console
```

### How to commit updates on rcc branch

```
git checkout rcc
# make changes
git commit -m "crm# 241111-003762 rims user orcid, institution sync"
git tag pitschi1.20.6
git push origin rcc
git push origin pitschi1.20.6
```
