# https://wiki.klarna.net/wiki/KEP_Images/Images_List#Python
FROM l-docker-klarna-production.artifactory.klarna.net/klarna/python:3_12.202509011140

USER root
WORKDIR /app

RUN yum install -y procps  \
    && yum clean all

COPY --chown=1000:1000 --chmod=755 pyproject.toml uv.lock entrypoint.sh /app/

RUN uv sync

USER klarna
WORKDIR /app

# Add virtual environment to PATH
ENV PATH="/app/.venv/bin:${PATH}"

CMD ["./entrypoint.sh"]
