#! /bin/bash

apt install gnat
apt install gprbuild
apt install -y build-essential libssl-dev libffi-dev python3-dev
apt install -y python3-pip
apt install -y python3-venv


# TODO - cd into the right directory

gnatmake -O3 wordsxml
gnatmake makedict
gnatmake makestem
gnatmake makeefil
gnatmake makeinfl

echo G | ./makedict
echo G | ./makestem
echo G | ./makeefil
echo G | ./makeinfl
