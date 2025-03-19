# Variables
DOCKERFILE_DSOMOP := Dockerfile_dsOMOP
DOCKERFILE_MIMICIV := Dockerfile_mimiciv
IMAGE_DSOMOP := brgelab/rock-dsomop
IMAGE_MIMICIV := brgelab/mimiciv-db
DSOMOP_VERSION := 1.1.1
R_VERSION := 4.4

# Build and push all images
all: build-all push-all

# Build both Docker images with no cache and latest base images
build-all:
	docker build -f $(DOCKERFILE_DSOMOP) --no-cache=true --pull -t="$(IMAGE_DSOMOP):$(DSOMOP_VERSION)-R$(R_VERSION)" . && \
	docker tag $(IMAGE_DSOMOP):$(DSOMOP_VERSION)-R$(R_VERSION) $(IMAGE_DSOMOP):$(DSOMOP_VERSION) && \
	docker tag $(IMAGE_DSOMOP):$(DSOMOP_VERSION)-R$(R_VERSION) $(IMAGE_DSOMOP):latest && \
	docker build -f $(DOCKERFILE_MIMICIV) --no-cache=true --pull -t="$(IMAGE_MIMICIV):latest" .

# Push all Docker images to registry
push-all:
	docker image push $(IMAGE_DSOMOP):$(DSOMOP_VERSION)-R$(R_VERSION) && \
	docker image push $(IMAGE_DSOMOP):$(DSOMOP_VERSION) && \
	docker image push $(IMAGE_DSOMOP):latest && \
	docker image push $(IMAGE_MIMICIV):latest

# Build dsOMOP Docker image only
build-dsomop:
	docker build --pull -f $(DOCKERFILE_DSOMOP) -t $(IMAGE_DSOMOP):$(DSOMOP_VERSION)-R$(R_VERSION) .

# Build MIMIC-IV Docker image only  
build-mimiciv:
	docker build --pull -f $(DOCKERFILE_MIMICIV) -t $(IMAGE_MIMICIV):latest .
