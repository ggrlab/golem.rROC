docker build --file Dockerfile -t registry.gunthergl.com/golem.rroc:0.2 .
docker build --file Dockerfile -t registry.gunthergl.com/golem.rroc:latest .

docker push registry.gunthergl.com/golem.rroc:0.2
docker push registry.gunthergl.com/golem.rroc:latest
# docker push registry.gunthergl.com/golem.rroc:latest
# docker run --rm -it --entrypoint /bin/bash registry.gunthergl.com/golem.rroc:0.1
docker run -p 9207:9207 -it --rm registry.gunthergl.com/golem.rroc:0.2