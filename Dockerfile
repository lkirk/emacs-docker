FROM python:3.9-slim


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
		python-virtualenv \
		python-setuptools \
		python-pip \
		python-wheel \
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
	apt-get remove --purge -y $BUILD_TOOLS

RUN \
	set -ex ;\
	useradd --home-dir /home/user user ;\
	mkdir -p /home/user ;\
	chown user:user /home/user

WORKDIR /home/user
USER user
ENV HOME=/home/user

# WORKDIR /root
# RUN ln -s /usr/bin/python3 /usr/bin/python

ADD .emacs .
ADD install-packages.el .
RUN \
	set -ex ;\
	# ln -s /usr/bin/python3 /usr/bin/python ;\
	emacs -Q --script install-packages.el

CMD ["emacs"]
