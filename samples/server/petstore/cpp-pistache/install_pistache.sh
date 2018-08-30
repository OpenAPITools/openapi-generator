#/bin/bash
# ref: http://pistache.io/quickstart#installing-pistache
#
echo "Installing Pistache ..."

git clone https://github.com/oktal/pistache.git || true
cd pistache
git submodule update --init
mkdir -p build
cd build
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ..
make
sudo make install
