FROM ubuntu

LABEL maintainer="theo@latindictionary.io"
LABEL version="0.1"
LABEL description="This is custom Docker Image for the JSON wrapper for words"

WORKDIR /code
# Update Ubuntu Software repository
RUN apt update && apt install -y gnat gprbuild build-essential \
                                libssl-dev libffi-dev python3-dev \
                                python3-pip git && \
                                apt-get clean -y && \
                                rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN git clone --depth 1 https://github.com/theotarr/wordsjson.git && \
    cd wordsjson/app/src && \
    gnatmake -O3 wordsxml && \
    gnatmake -O3 makedict && \
    gnatmake -O3 makestem && \
    gnatmake -O3 makeefil && \
    gnatmake -O3 makeinfl && \
    strip wordsxml makedict makestem makeefil makeinfl && \
    echo G | ./makedict && \
    echo G | ./makestem && \
    ./makeefil && \
    ./makeinfl && \
    cd .. && \
    cd .. && \
    pip install --no-cache-dir -r requirements.txt

EXPOSE 80
CMD cd wordsjson/app/src && ./wordsxml canis && cd ../.. && gunicorn --workers 4 --bind 0.0.0.0:80 run:app

