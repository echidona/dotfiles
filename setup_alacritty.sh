#!/bin/sh

## faster terminal alacritty install

curl https://sh.rustup.rs -sSf | sh
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc # reflection zshrc

echo "install alacritty"
git clone https://github.com/jwilm/alacritty.git ~/alacritty
cd ~/alacritty
# build
make app
cp -r target/release/osx/Alacritty.app /Applications/

echo "alacritty installing finished!"
