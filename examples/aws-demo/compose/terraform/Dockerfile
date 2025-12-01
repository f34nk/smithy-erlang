FROM l-docker-klarna-production.artifactory.klarna.net/klarna/amazonlinux:2023.202407220110

COPY entrypoint.sh ./entrypoint.sh
COPY main.tf ./main.tf
COPY config.toml ./config.toml

USER root

ARG ARCHITECTURE=${ARCHITECTURE:-'arm64'}
ARG TERRAFORM_VERSION=1.7.1
ENV TERRAFORM_RELEASE=terraform_${TERRAFORM_VERSION}_linux_${ARCHITECTURE}

# Download and extract `terraform` binary
RUN dnf -y --quiet install unzip && \
  # Download and extract `terraform` binary
  # See https://www.hashicorp.com/official-release-channels
  curl --silent --show-error --fail https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/${TERRAFORM_RELEASE}.zip --output ${TERRAFORM_RELEASE}.zip && \
  unzip -q ./${TERRAFORM_RELEASE}.zip -d /opt/hashicorp

ENV PATH="/opt/hashicorp:${PATH}"

RUN chown -R klarna:klarna /home/klarna
RUN chmod 755 /home/klarna

USER klarna

CMD ["./entrypoint.sh"]
