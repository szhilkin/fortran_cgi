FROM ubuntu:latest

RUN \
  DEBIAN_FRONTEND=noninteractive \
  apt-get -q -y update && \
  apt-get -q -y install nginx fcgiwrap curl libgfortran3 && \
  curl -s -L $(curl -s https://api.github.com/repos/szhilkin/fortran_cgi/releases/latest | grep browser_download_url |  cut -d '"' -f 4) >dart_cgi && \
  curl -s -L https://raw.githubusercontent.com/szhilkin/fortran_cgi/master/nginx/nginx.conf >/etc/nginx/nginx.conf && \
  chmod +x dart_cgi && \
  sed -i 's/^\(user .*\)$/user root;/' /etc/nginx/nginx.conf

CMD spawn-fcgi -a 127.0.0.1 -p 9000 ./dart_cgi && nginx
