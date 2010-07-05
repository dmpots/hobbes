#!/bin/bash


papi-analyze $@ > __OLD.txt
./dist/build/papi-analyze/papi-analyze $@ > __NEW.txt

diff __OLD.txt __NEW.txt
echo $?

