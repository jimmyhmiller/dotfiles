export ZPLUG_HOME=/opt/homebrew/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-syntax-highlighting"

zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
zplug "plugins/autojump", from:oh-my-zsh

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

snap_screen() {
  if [ $# -eq 0 ]
  then
    name="screenshot.png"
  else
    name="$1.png"
  fi
  adb shell screencap -p /sdcard/$name
  adb pull /sdcard/$name
  adb shell rm /sdcard/$name
  curr_dir=pwd
  echo "save to `pwd`/$name"
}


export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

zplug load

alias s='git status'

export EDITOR=emacs
alias changes='git add . && git commit -am "Changes" && git push'
alias staged='git diff --staged'

alias disassemble='yaxdis -a armv8'

# Appends every command to the history file once it is executed
setopt inc_append_history

alias -g pair-greg='--trailer "Co-authored-by: Grzegorz Caban <nabacg@gmail.com>"'
setopt share_history
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# Be able to search forward
stty -ixon

# make git scroll with scroll wheel
git config --global core.pager "less -+\$LESS -RS"


export PATH="/opt/homebrew/opt/lvm/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib -L/opt/homebrew/opt/openssl@3/lib -L/Users/jimmyhmiller/Documents/Code/PlayGround/rust/mamba/target/debug/"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include -I/opt/homebrew/opt/openssl@3/include"
# export LLVM_SYS_130_PREFIX="/usr/local/opt/llvm/"

# export PATH="/Library/Developer/CommandLineTools/Library/PrivateFrameworks/:$PATH"
source /opt/homebrew/opt/chruby/share/chruby/auto.sh
[ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh

export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"
export PATH="/opt/homebrew/opt/bison/bin:$PATH"
export PATH="/Users/jimmyhmiller/.cargo/bin/:$PATH"
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/lib"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"
export PATH="/Users/jimmyhmiller/.local/bin:$PATH"
export PATH="/Users/jimmyhmiller/Downloads/flutter/bin:$PATH"
export PATH="/Users/jimmyhmiller/Library/Android/sdk/platform-tools/:$PATH"


export ANDROID_HOME="/Users/jimmyhmiller/Library/Android/sdk/"

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
