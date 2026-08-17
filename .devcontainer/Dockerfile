FROM docker.io/library/rollends.research-container:v1.0

# SSH Configuration.
RUN mkdir -p /root/.ssh/
RUN chmod 700 /root/.ssh/
COPY ssh_config.private deploykey.private /root/.ssh/
RUN mv /root/.ssh/ssh_config.private /root/.ssh/config