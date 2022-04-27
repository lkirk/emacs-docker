FROM python:3.9-slim-bullseye


ARG BUILD_TOOLS='wget build-essential pkg-config autoconf'


RUN \
	set -ex ;\
	apt-get update ;\
	apt-get install -y --no-install-recommends \
		$BUILD_TOOLS \
		ca-certificates \
		gnutls-bin \
		libncurses-dev \
		libgnutls28-dev \
		fonts-powerline \
		# for jedi mode
		python3-virtualenv \
		python3-setuptools \
		python3-pip \
		python3-wheel \
		virtualenv \
		&& rm -rf /var/lib/apt/lists/*

RUN set -ex; \
	pip install --upgrade --no-cache-dir pip setuptools; \
	pip install --upgrade --no-cache-dir flake8 'black==22.3.0' cython


RUN \
	set -ex ;\
	wget --quiet http://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz ;\
	tar -xJf emacs-27.1.tar.xz ;\
	( \
	cd emacs-27.1 ;\
	autoreconf ;\
   	./configure \
		--with-x-toolkit=no ;\
	make ;\
	make install ;\
	) ;\
	rm -r emacs-27.1* ;\
	apt-get remove --purge -y $BUILD_TOOLS ;\
    apt-get clean

RUN \
	set -ex ;\
	useradd --home-dir /home/user user ;\
	mkdir -p /home/user ;\
	chown user:user /home/user

WORKDIR /home/user
USER user
ENV HOME=/home/user

ADD .emacs .
ADD install-packages.el .
RUN \
	set -ex ;\
	emacs -Q --script install-packages.el

CMD ["emacs"]
