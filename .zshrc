# Bootstrap zplug on a new machine, but keep package-manager work out of the
# normal startup path. Support both Apple Silicon and Intel Homebrew prefixes.
if [[ -r /opt/homebrew/opt/zplug/init.zsh ]]; then
  export ZPLUG_HOME=/opt/homebrew/opt/zplug
elif [[ -r /usr/local/opt/zplug/init.zsh ]]; then
  export ZPLUG_HOME=/usr/local/opt/zplug
elif (( $+commands[brew] )); then
  echo "Installing zplug (first shell on this machine)..."
  brew install zplug
  export ZPLUG_HOME="$(brew --prefix zplug)"
fi

zplug_syntax="$ZPLUG_HOME/repos/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh"
zplug_pure="$ZPLUG_HOME/repos/sindresorhus/pure/pure.zsh"
zplug_omz="$ZPLUG_HOME/repos/robbyrussell/oh-my-zsh"

if [[ -r "$zplug_syntax" && -r "$zplug_pure" && -r "$zplug_omz/plugins/autojump/autojump.plugin.zsh" ]]; then
  # Fast path: load installed plugins directly without initializing zplug,
  # inspecting repositories, rebuilding caches, or acquiring locks.
  fpath=("${zplug_pure:h}" $fpath)
  autoload -Uz compinit
  compinit
  source "$zplug_omz/lib/functions.zsh"
  source "$zplug_omz/plugins/autojump/autojump.plugin.zsh"
  source "$zplug_pure"
  source "$zplug_syntax"
elif [[ -r "$ZPLUG_HOME/init.zsh" ]]; then
  # Bootstrap path: reached on a new machine or after adding a plugin here.
  source "$ZPLUG_HOME/init.zsh"
  zplug "zsh-users/zsh-syntax-highlighting"
  zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
  zplug "plugins/autojump", from:oh-my-zsh

  if ! zplug check; then
    echo "Installing missing zsh plugins (first run only)..."
    zplug install
  fi
  zplug load
else
  echo "zplug is unavailable; install Homebrew or zplug to enable shell plugins" >&2
fi

unset zplug_syntax zplug_pure zplug_omz

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

alias s='git status'

export EDITOR=emacs
alias changes='git add . && git commit -am "Changes" && git push'
alias staged='git diff --staged'

alias disassemble='yaxdis -a armv8'


# Set word characters (remove / to make it a word boundary for Alt+arrows)
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

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
if (( $+commands[git] )) && [[ "$(git config --global --get core.pager)" != 'less -+$LESS -RS' ]]; then
  git config --global core.pager 'less -+$LESS -RS'
fi


export PATH="/opt/homebrew/opt/lvm/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib -L/opt/homebrew/opt/openssl@3/lib -L/Users/jimmyhmiller/Documents/Code/PlayGround/rust/mamba/target/debug/"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include -I/opt/homebrew/opt/openssl@3/include"
# export LLVM_SYS_130_PREFIX="/usr/local/opt/llvm/"

# export PATH="/Library/Developer/CommandLineTools/Library/PrivateFrameworks/:$PATH"
source /opt/homebrew/opt/chruby/share/chruby/auto.sh

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
if (( $+commands[jenv] )); then
  jenv_init_cache="$HOME/.cache/zsh/jenv-init.zsh"
  if [[ ! -s "$jenv_init_cache" || "$commands[jenv]" -nt "$jenv_init_cache" ]]; then
    mkdir -p "${jenv_init_cache:h}"
    jenv_init_cache_tmp="$jenv_init_cache.$$.new"
    if jenv init - >| "$jenv_init_cache_tmp"; then
      mv "$jenv_init_cache_tmp" "$jenv_init_cache"
    else
      rm -f "$jenv_init_cache_tmp"
    fi
  fi
  [[ -r "$jenv_init_cache" ]] && source "$jenv_init_cache"
  unset jenv_init_cache jenv_init_cache_tmp
fi
