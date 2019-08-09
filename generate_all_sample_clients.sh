for f in bin/*.sh; do
  bash "$f" || break
done

for f in bin/openapi3/*.sh; do
  bash "$f" || break
done
